{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Agent (runAgentMonitor) where

import Control.Concurrent        (threadDelay)
import Control.Exception         (throwIO, bracket, catch, SomeException)
import Control.Monad             (unless, when)
import Data.Aeson                -- bring in Value, Object, String, plus all your parser/encoder fns
  ( Value(Object, String)
  , FromJSON(parseJSON), ToJSON
  , eitherDecode, decode, encode, object, (.:), (.=), withObject
  , fromJSON, Result(Success)
  )
import Data.Aeson.Types          (Parser)
import Data.Ratio                (numerator, denominator)
import GHC.Generics              (Generic)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import Network.HTTP.Client       ( Manager
                                 , RequestBody(RequestBodyLBS)
                                 , httpLbs, method, newManager
                                 , parseRequest, requestBody
                                 , requestHeaders, responseBody
                                 , responseStatus
                                 , HttpException(..)
                                 )
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import System.Directory          (createDirectoryIfMissing, doesFileExist, getHomeDirectory, getModificationTime, removeDirectoryRecursive)
import System.FilePath           ((</>), takeDirectory)
import System.Posix.Process      (getProcessID)
import System.Posix.Signals      (installHandler, sigINT, Handler(Catch))
import System.Process.Typed      (readProcessStdout_, proc)
import System.IO                 (openFile, hClose, IOMode(AppendMode), Handle)
import Data.Time.Clock           (diffUTCTime, getCurrentTime, UTCTime, addUTCTime, utctDay)
import Data.Time.Calendar        (toGregorian)
import Data.Time.Format          (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX     (getPOSIXTime)
import qualified CardanoSwaps.OneWaySwap   as OneWay
import Data.Maybe                (fromMaybe, isJust, isNothing)
import Data.List                 (head, sortBy, sortOn, take)
import Text.Read                 (readMaybe)
import System.Environment        (lookupEnv)
import Data.Ord                  (comparing)
import Control.Concurrent        (MVar, newMVar, takeMVar, putMVar, withMVar, modifyMVar_)
import qualified Data.Aeson.KeyMap  as KM
-- for Plutus‐style rationals
import qualified PlutusTx.Ratio as Ratio
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

-- For improved OpenAI handling
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import qualified Control.Lens as L

import CLI.Types                 -- shared types: AgentOptions, MemoryEntry, Network, etc.
import CLI.Query                 -- bring in only the query functions
  ( runQueryAllSwapsByTradingPair
  , runQueryPersonalAddress
  )
import CLI.Parsers               ()
import CardanoSwaps.Utils        (showTokenName)
import Data.Aeson                (Value)
import Network.HTTP.Client       (Request, HttpException(..))
import Text.Read                 (readMaybe)

-- | Convert an AssetConfig into "policyId.tokenName"
formatAsset :: AssetConfig -> String
formatAsset (pid, name) = show pid ++ "." ++ show name

-- | AiAction returned by Agent API
data AIAction
  = TakeSwap   { aiUtxo :: T.Text, aiRationale :: T.Text }
  | CreateSwap { aiPrice :: Double, aiAmount :: Integer, aiRationale :: T.Text }
  | NoAction   { aiRationale :: T.Text }
  deriving (Show, Generic)

instance FromJSON AIAction where
  parseJSON = withObject "AIAction" $ \v -> do
    action <- v .: "action"
    rationale <- v .: "rationale"
    case (action :: T.Text) of
      "TakeSwap" -> TakeSwap <$> v .: "aiUtxo" <*> pure rationale
      "CreateSwap" -> CreateSwap <$> v .: "aiPrice" <*> v .: "aiAmount" <*> pure rationale
      "NoAction" -> pure $ NoAction rationale
      _ -> fail $ "Unknown action: " ++ T.unpack action

-- | Memory entry for knowledge graph
data MemoryEntry = MemoryEntry
  { topic   :: T.Text
  , content :: T.Text
  , ts      :: T.Text
  } deriving (Show, Generic)
instance ToJSON MemoryEntry

-- | Agent state (for backoff tracking)
data AgentState = AgentState
  { asFailCount   :: Int          -- consecutive failures
  , asBackoffTime :: Int          -- current backoff in microseconds
  , asLastFail    :: Maybe UTCTime -- time of last failure
  , asLogHandle   :: Maybe Handle  -- current log file handle
  , asLogDay      :: Maybe Int     -- current log day
  , asSessionId   :: Maybe String  -- current AI conversation session ID
  }

-- | Initialize agent state
initAgentState :: IO AgentState
initAgentState = pure $ AgentState 0 0 Nothing Nothing Nothing Nothing

-- | Reset backoff after successful operations
resetBackoff :: MVar AgentState -> IO ()
resetBackoff mst = do
  st <- takeMVar mst
  putMVar mst (st { asFailCount = 0, asBackoffTime = 0 })

-- | Increase backoff after failure (exponential with max)
increaseBackoff :: MVar AgentState -> IO Int
increaseBackoff mst = do
  now <- getCurrentTime
  st <- takeMVar mst
  let newCount = asFailCount st + 1
      -- Exponential backoff: 5s -> 10s -> 20s, with 10 minute max (600 seconds)
      newBackoff = min 600000000 $ if asBackoffTime st == 0 then 5000000 else asBackoffTime st * 2
  putMVar mst (st { asFailCount = newCount, asBackoffTime = newBackoff, asLastFail = Just now })
  return newBackoff

--------------------------------------------------------------------------------
-- Main entrypoint
--------------------------------------------------------------------------------
runAgentMonitor :: AgentOptions -> IO ()
runAgentMonitor cfg@AgentOptions{..} = do
    -- Resolve defaults or env overrides
    home <- getHomeDirectory
    let skFile     = aoSigningKey  -- override via --signing-key or env AGENT_SKEY
        vkFile     = aoVerificationKey
        addrSpec   = aoAgentAddress
        defaultProto = home </> ".agent-wallet" </> "protocol-params.json"
        logDir     = home </> ".agent-wallet" </> "logs"

    -- Validate key files
    forM_ [skFile, vkFile] $ \f -> do
      exists <- doesFileExist f
      unless exists $ throwIO $ userError $ "Missing file: " ++ f

    -- Resolve payment address (file or literal)
    addr <- do
      fpExists <- doesFileExist addrSpec
      if fpExists then T.strip . TE.decodeUtf8 . BL.toStrict <$> BL.readFile addrSpec
                  else pure (T.pack addrSpec)
    let agentAddr = T.unpack addr

    -- Determine network and CLI flag
    let (netFlag, networkType) = case aoNetworkMagic of
          Nothing -> (["--mainnet"], Mainnet)
          Just n  -> (["--testnet-magic", show n], PreProdTestnet)

    -- Check if this is a dry run
    let isDryRun = aoDryRun
        isMakerOnly = aoMakerOnly
        
    -- Ensure protocol params file
    let protoFile = fromMaybe defaultProto aoProtocolParamsFile
    paramsFile <- ensureProtocolParams netFlag protoFile

    -- Create log directory if it doesn't exist
    createDirectoryIfMissing True logDir

    -- Check node socket if on-chain submission is enabled
    when (isNothing aoBlockfrostProjectId) $ do
      case aoNodeSocket of
        Just _  -> pure ()
        Nothing -> do m <- lookupEnv "CARDANO_NODE_SOCKET_PATH"
                      unless (isJust m) $ throwIO $ userError "Missing node socket: provide --node-socket or set CARDANO_NODE_SOCKET_PATH"

    -- Create agent state for API request backoff tracking
    stateMVar <- newMVar =<< initAgentState

    -- Prepare temp dir using bracket to ensure cleanup on exit
    pid <- getProcessID
    let tmpDir = "/tmp/cardano-swaps-agent" </> show pid
    
    -- Set up signal handler for graceful shutdown
    let cleanupAndExit = do
          st <- takeMVar stateMVar
          -- Close any open log files
          case asLogHandle st of
            Just h  -> hClose h
            Nothing -> pure ()
          -- Remove the temp directory
          whenM (doesFileExist tmpDir) $ removeDirectoryRecursive tmpDir
          logInfo "Agent shutting down gracefully."
          putStrLn "Agent shutting down. Goodbye!"
    
    _ <- installHandler sigINT (Catch cleanupAndExit) Nothing
    
    -- Use bracket for guaranteed cleanup even with exceptions
    bracket 
      (createDirectoryIfMissing True tmpDir >> pure tmpDir)
      (const $ whenM (doesFileExist tmpDir) (removeDirectoryRecursive tmpDir))
      (\dir -> agentLoop cfg stateMVar agentAddr skFile vkFile netFlag networkType paramsFile dir isDryRun isMakerOnly logDir)
  where
    orElse (Just x) _ = Just x
    orElse Nothing  y = Just y

--------------------------------------------------------------------------------
-- Loop
--------------------------------------------------------------------------------
agentLoop
  :: AgentOptions
  -> MVar AgentState      -- agent state for backoff tracking
  -> String               -- agent address
  -> FilePath             -- signing key
  -> FilePath             -- verification key
  -> [String]             -- network flags
  -> Network              -- CLI.Query Network type
  -> FilePath             -- protocol parameters file
  -> FilePath             -- temp dir
  -> Bool                 -- dry run mode
  -> Bool                 -- maker only mode
  -> FilePath             -- log directory
  -> IO ()
agentLoop cfg@AgentOptions{..} stateMVar agentAddr skFile vkFile netFlag networkType paramsFile tmpDir isDryRun isMakerOnly logDir = do
    manager <- newManager tlsManagerSettings
    logInfo "🔄 Agent started."
    logInfo "DEBUG: about to enter loop"
    loop manager
  where
    loop mgr = do
      -- 0. Check and rotate log if needed
      logInfo "DEBUG: starting iteration; now rotating logs"
      ensureLogFile stateMVar logDir
      logInfo "DEBUG: passed log-rotation (completed successfully)"
      
      -- 1. Fetch order book
      logInfo "DEBUG: about to fetch swaps..."
      swaps <- runQueryAllSwapsByTradingPair networkType Koios (OfferAsset aoOfferAsset) (AskAsset aoAskAsset)

      -- 2. Retrieve memory
      mems <- withRetry stateMVar (fetchMem stateMVar cfg mgr) "memory fetch"

      -- 3. Ask AI
      action <- if length swaps > 0
                then withRetry stateMVar (askAI cfg mgr stateMVar (summarizeOrders swaps) mems) "AI query"
                else pure $ NoAction "No swaps available"

      -- 4. Handle action
      outcome <- case action of
        NoAction r   -> logInfo ("⚠️ No action. Reason: " ++ T.unpack r) >> pure Nothing
        TakeSwap u r -> do 
                         logInfo $ "🔍 AI Reasoning: " ++ T.unpack r
                         ok <- runTakeSwapTx cfg mgr agentAddr skFile vkFile netFlag networkType paramsFile tmpDir (T.unpack u) swaps isDryRun
                         let note = if ok then "Filled swap " <> u else "Failed swap " <> u
                             ts = T.pack . show <$> getCurrentTime
                         ts' <- ts
                         pure (Just $ MemoryEntry (pairTag cfg) note ts')
        CreateSwap{..} -> 
          if isMakerOnly 
          then do
            logInfo $ "🔍 AI Reasoning: " ++ T.unpack aiRationale
            logInfo $ "CreateSwap price=" ++ show aiPrice ++ ", amount=" ++ show aiAmount
            -- TODO: Implement create swap in future iteration
            logInfo "🚧 CreateSwap not yet implemented, but maker-only flag is enabled."
            pure Nothing
          else do
            logInfo $ "🔍 AI Reasoning: " ++ T.unpack aiRationale
            logInfo "⚠️ AI suggested CreateSwap but maker-only flag is not enabled. Skipping."
            pure Nothing

      -- 5. Store memory
      case outcome of
        Just mem -> withRetry stateMVar (storeMem cfg mgr mem) "memory storage"
        Nothing  -> pure ()

      -- 6. Wait and repeat
      threadDelay (aoPollSeconds * 1_000_000)
      loop mgr

    -- | Current timestamp in ISO8601
    currentTs = T.pack . show <$> getCurrentTime

--------------------------------------------------------------------------------
-- Order summarization (for AI prompt security)
--------------------------------------------------------------------------------
summarizeOrders :: [SwapUTxO] -> [SwapUTxO]
summarizeOrders swaps =
  -- Take at most 5 swaps, prioritizing the most attractive ones
  take 5 $ sortOn sortCriteria swaps
  where
    -- Sort by price = den/num (uses PlutusTx.Ratio)
    sortCriteria utxo = case swapDatum utxo of
      Just (OneWayDatum datum) ->
        let r   = OneWay.swapPrice datum
            num = Ratio.numerator   r  -- Integer
            den = Ratio.denominator r  -- Integer
        in  fromIntegral den / fromIntegral num
      _ -> 999999.0

--------------------------------------------------------------------------------
-- Log file handling
--------------------------------------------------------------------------------
ensureLogFile :: MVar AgentState -> FilePath -> IO ()
ensureLogFile mst logDir = modifyMVar_ mst $ \st -> do
  now <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay now
      dayNum = fromIntegral day
      today = formatTime defaultTimeLocale "%Y%m%d" now
      logFile = logDir </> "agent-" ++ today ++ ".log"
  
  -- Check if we need to rotate the log
  case (asLogHandle st, asLogDay st) of
    (Just h, Just d) | d == dayNum -> 
      -- Still the same day, keep using existing handle
      pure st
    (Just h, _) -> do
      -- Day changed, close old file and open new one
      hClose h
      handle <- openFile logFile AppendMode
      let newState = st { asLogHandle = Just handle, asLogDay = Just dayNum }
      -- Compress the old log file
      let oldDate = formatTime defaultTimeLocale "%Y%m%d" $ addUTCTime (-86400) now
          oldLog = logDir </> "agent-" ++ oldDate ++ ".log"
      doesFileExist oldLog >>= \exists -> when exists $ do
        -- gzip the old log, ignoring its stdout
        _ <- readProcessStdout_ (proc "gzip" ["-f", oldLog])
        pure ()
      pure newState
    (Nothing, _) -> do
      -- First run, create a new log file
      createDirectoryIfMissing True logDir
      handle <- openFile logFile AppendMode
      pure (st { asLogHandle = Just handle, asLogDay = Just dayNum })

--------------------------------------------------------------------------------
-- Retry logic with exponential backoff
--------------------------------------------------------------------------------
withRetry :: MVar AgentState -> IO a -> String -> IO a
withRetry mst action label = do
  catchRetry 3  -- Start with max 3 retries
  where
    catchRetry 0 = do
      logError $ "Failed " ++ label ++ " after all retries"
      -- Return a default value or throw exception depending on the operation
      throwIO $ userError $ "Failed " ++ label ++ " after all retries"
    
    catchRetry n = catch action $ \(e :: SomeException) -> do
      waitTime <- increaseBackoff mst  -- Increase and get backoff time
      logError $ label ++ " failed: " ++ show e ++ ". Retrying in " ++ show (waitTime `div` 1000000) ++ "s"
      threadDelay waitTime
      catchRetry (n-1)

--------------------------------------------------------------------------------
-- Protocol params management
--------------------------------------------------------------------------------
ensureProtocolParams :: [String] -> FilePath -> IO FilePath
ensureProtocolParams netFlag file = do
    exists <- doesFileExist file
    if exists
      then do
        modTime <- getModificationTime file
        now <- getCurrentTime
        if diffUTCTime now modTime < 18*3600
          then return file
          else fetchParams
      else fetchParams
  where
    fetchParams = do
      createDirectoryIfMissing True (takeDirectory file)
      _ <- readProcessStdout_ (proc "cardano-cli" (["query","protocol-parameters","--out-file",file] ++ netFlag))
      return file

--------------------------------------------------------------------------------
-- HTTP helpers
--------------------------------------------------------------------------------
fetchMem :: MVar AgentState -> AgentOptions -> Manager -> IO [Value]
fetchMem mst AgentOptions{..} mgr = do
    -- Check if we're using OpenAI API
    let useOpenAIDirect = T.isInfixOf "openai.com" (T.pack aoApiBase)
    
    if useOpenAIDirect
    then do
      -- When using OpenAI, return an empty memory array
      -- OpenAI doesn't have a memory API endpoint
      logInfo "Using OpenAI API - no memory storage available, using empty memory"
      return []
    else do
      -- Standard Agent API path
      let topic = T.unpack (TE.decodeUtf8 $ BL.toStrict $ encode (formatAsset aoOfferAsset <> "-" <> formatAsset aoAskAsset))
          url = aoApiBase <> "/knowledge?topic=" <> topic
      req0 <- parseRequest url
      let req = attachKey aoApiKey req0 { method = "GET" }
      resp <- httpLbs req mgr
      case eitherDecode (responseBody resp) of
        Right xs -> do
          resetBackoff mst
          return xs
        Left err -> logError ("Memory decode error: " ++ err) >> return []

askAI :: AgentOptions -> Manager -> MVar AgentState -> [SwapUTxO] -> [Value] -> IO AIAction
askAI AgentOptions{..} mgr stateMVar swaps mems = do
    -- Check if we should use OpenAI API path or standard agent API path
    let useOpenAIDirect = T.isInfixOf "openai.com" (T.pack aoApiBase)
    
    if useOpenAIDirect
    then askOpenAI aoApiKey swaps mems mgr
    else askAgentAPI aoApiKey aoApiBase stateMVar swaps mems mgr

-- | Ask OpenAI directly using the Responses API for o4-mini
askOpenAI :: Maybe String -> [SwapUTxO] -> [Value] -> Manager -> IO AIAction
askOpenAI mApiKey swaps mems mgr = do
    case mApiKey of
      Nothing -> logError "No OpenAI API key provided" >> return (NoAction "No API key")
      Just apiKey -> do
        -- Prepare user message
        let ordJson = encode swaps
            memJson = encode mems
            usrMsg :: T.Text
            usrMsg = "You are an autonomous Cardano swap-bot that helps trade tokens on the Cardano blockchain. You need to analyze available swap opportunities and make strategic trading decisions based on market data, price trends, and previous actions in memory. Always provide detailed reasoning for your decisions.\n\n"
                  <> "I'm looking for your trading recommendation based on this data.\n\nAvailable Orders (swap opportunities):\n" <> TE.decodeUtf8 (BL.toStrict ordJson)
                  <> "\n\nPrevious Trading History:\n" <> TE.decodeUtf8 (BL.toStrict memJson)
                  <> "\n\nAnalyze this data and provide a JSON object representing your decision WITH A DETAILED EXPLANATION OF YOUR REASONING. Consider price trends, market gaps, and trading strategies. You must respond in valid JSON with one of these formats:\n"
                  <> "1. To take an existing swap: {\"action\":\"TakeSwap\",\"aiUtxo\":\"txhash#index\",\"rationale\":\"your detailed reasoning\"}\n"
                  <> "2. To create a new swap: {\"action\":\"CreateSwap\",\"aiPrice\":0.45,\"aiAmount\":1000000,\"rationale\":\"your detailed reasoning\"}\n"
                  <> "3. To do nothing: {\"action\":\"NoAction\",\"rationale\":\"your detailed reasoning\"}\n"
                  <> "\nYour response must be valid JSON that follows one of these formats exactly. In the rationale field, provide a thorough explanation of why this is the optimal decision."
            
            -- New Responses API payload format
            payload = object [ 
                "model" .= ("o4-mini" :: T.Text),
                "reasoning" .= object ["effort" .= ("medium" :: T.Text)],
                "input" .= [
                    object ["role" .= ("user" :: T.Text), "content" .= usrMsg]
                ],
                "max_output_tokens" .= (1000 :: Int)
              ]
            
            -- Use Responses API endpoint
            url = "https://api.openai.com/v1/responses"
        
        -- Prepare HTTP request
        req0 <- parseRequest url
        
        -- Add authentication headers
        let req = req0 { 
              method = "POST",
              requestHeaders = [
                  ("Content-Type", "application/json"),
                  ("Authorization", "Bearer " <> TE.encodeUtf8 (T.pack apiKey))
              ],
              requestBody = RequestBodyLBS (encode payload)
            }
        
        -- Send request
        logInfo "Sending request to OpenAI Responses API..."
        resp <- httpLbs req mgr
        
        -- Parse OpenAI response
        logInfo $ "Response status: " ++ show (responseStatus resp)
        let respBody = responseBody resp
        
        case decode respBody :: Maybe Value of
          Just responseObj -> do
            -- Extract the output_text from the response (Responses API format)
            case responseObj ^? key "output_text" . _String of
              Just outputText -> do
                logInfo $ "OpenAI response received"
                
                -- Try to parse JSON from the text
                case extractAndParseJSON outputText of
                  Just action -> do
                    logInfo $ "Successfully parsed AI action: " ++ show action
                    return action
                  Nothing -> do
                    logError "Failed to parse JSON from AI response"
                    return $ NoAction "Failed to parse AI response"
              Nothing -> do
                logError "Could not find output_text in response"
                return $ NoAction "No response content"
          _ -> do
            logError "Failed to decode API response"
            return $ NoAction "API decode failure"

-- | Helper function to extract and parse JSON from the LLM response
extractAndParseJSON :: T.Text -> Maybe AIAction
extractAndParseJSON text = do
  -- Try to parse directly
  let directResult = decode (BL.fromStrict $ TE.encodeUtf8 text)
  case directResult of
    Just action -> return action
    Nothing -> do
      -- Look for JSON object within text
      let jsonStart = T.findIndex (== '{') text
          jsonEnd = findIndexEnd (== '}') text
      case (jsonStart, jsonEnd) of
        (Just start, Just end) -> 
          let jsonText = T.take (end - start + 1) $ T.drop start text
          in decode (BL.fromStrict $ TE.encodeUtf8 jsonText)
        _ -> Nothing

-- Helper function to find the last index of a character
findIndexEnd :: (Char -> Bool) -> T.Text -> Maybe Int
findIndexEnd p t = 
  let len = T.length t
      indices = [i | i <- [0..len - 1], i < len && p (T.index t i)]
  in if null indices then Nothing else Just (last indices)

-- | Ask the agent API (implementation for external agent API)
askAgentAPI :: Maybe String -> String -> MVar AgentState -> [SwapUTxO] -> [Value] -> Manager -> IO AIAction
askAgentAPI mApiKey apiBase stateMVar swaps mems mgr = do
    -- Create or retrieve session ID
    st <- takeMVar stateMVar
    sessionId <- case asSessionId st of
      Just sid -> do
        putMVar stateMVar st
        pure sid
      Nothing -> do
        newSid <- UUID.toString <$> UUID.nextRandom
        putMVar stateMVar (st { asSessionId = Just newSid })
        pure newSid

    let sysMsg = "You are an autonomous Cardano swap‑bot."
        ordJson = encode swaps
        memJson = encode mems
        usrMsg = "Orders:\n" <> TE.decodeUtf8 (BL.toStrict ordJson)
              <> "\nMemory:\n" <> TE.decodeUtf8 (BL.toStrict memJson)
              <> "\nRespond with JSON: {\"action\":...}."
        
        -- Updated payload format to match API requirements
        payload = object [ 
            "messages" .= [ 
                object ["role" .= ("system"::T.Text), "content" .= (sysMsg::T.Text)]
              , object ["role" .= ("user"::T.Text), "content" .= usrMsg]
            ]
          , "message" .= usrMsg
          , "session_id" .= (T.pack sessionId)
          ]
        
        url = apiBase <> "/chat"
    
    logInfo $ "Using session ID: " ++ sessionId
    req0 <- parseRequest url
    let req = attachKey mApiKey req0
                { method = "POST"
                , requestBody = RequestBodyLBS (encode payload)
                , requestHeaders = [("Content-Type","application/json")] }
    resp <- httpLbs req mgr
    
    -- Parse assistant reply
    case eitherDecode (responseBody resp) of
      Right v -> do
        resetBackoff stateMVar
        case KM.lookup "reply" v of
          Just (String txt) -> case eitherDecode (BL.fromStrict $ TE.encodeUtf8 txt) of
            Right act -> pure act
            _ -> pure $ NoAction "Failed to decode action"
          _ -> pure $ NoAction "Invalid response format"
      Left err -> do
        logError $ "Failed to decode API response: " ++ err
        pure $ NoAction "API error"

attachKey :: Maybe String -> Request -> Request
attachKey Nothing  req = req
attachKey (Just k) req = req { requestHeaders = 
    -- Try both formats for maximum compatibility
    [ ("Authorization", TE.encodeUtf8 (T.pack k))
    , ("Authorization", TE.encodeUtf8 (T.pack $ "Bearer " ++ k))
    , ("X-API-Key", TE.encodeUtf8 (T.pack k))
    ] ++ requestHeaders req 
  }

--------------------------------------------------------------------------------
-- Transaction building & submission
--------------------------------------------------------------------------------
runTakeSwapTx
  :: AgentOptions
  -> Manager
  -> String      -- agent address
  -> FilePath    -- signing key
  -> FilePath    -- verification key
  -> [String]    -- network flags
  -> Network     -- network type
  -> FilePath    -- protocol params
  -> FilePath    -- temp dir
  -> String      -- swap utxo ref "txhash#index"
  -> [SwapUTxO]
  -> Bool        -- dry run mode
  -> IO Bool
runTakeSwapTx cfg@AgentOptions{..} mgr agentAddr skFile vkFile netFlag networkType paramsFile tmpDir utxoRef swaps isDryRun = do
    let rFile1 = tmpDir </> "redeemer-spend.json"
        rFile2 = tmpDir </> "redeemer-beacon.json"
        swapScriptFile  = tmpDir </> "swap.script"
        beaconScriptFile= tmpDir </> "beacon.script"
        txBody  = tmpDir </> "tx.body"
        txSigned= tmpDir </> "tx.signed"
    -- produce redeemers
    _ <- readProcessStdout_ (proc "cardano-swaps" ["spending-redeemers","one-way","--swap","--out-file",rFile1])
    _ <- readProcessStdout_ (proc "cardano-swaps" ["beacon-redeemers","one-way","--mint-or-burn","--out-file",rFile2])
    -- export scripts
    _ <- readProcessStdout_ (proc "cardano-swaps" ["scripts","one-way","swap-script","--out-file",swapScriptFile])
    _ <- readProcessStdout_ (proc "cardano-swaps" ["scripts","one-way","beacon-script","--out-file",beaconScriptFile])
    -- fetch personal UTxOs
    pUtxos <- runQueryPersonalAddress networkType Koios (UserAddress $ T.pack agentAddr)
    -- select collateral UTxO: ADA-only >= 5 ADA
    let isAdaOnly u = case personalValue u of
          [Asset pid tok qty] -> pid == "" && tok == "" && maybe False (>=5000000) (readMaybe (T.unpack qty))
          _                   -> False
        collUtxos = filter isAdaOnly pUtxos
    if null collUtxos
      then logError "No collateral UTxO >= 5 ADA found." >> return False
      else do
        let coll = head collUtxos
            collRef = T.unpack (personalTxHash coll) ++ "#" ++ show (personalOutputIndex coll)
        -- select funding UTxO for ask asset
        let (askPol, askTok) = aoAskAsset
            askPolTxt = T.pack (show askPol)
            askTokTxt = T.pack (show askTok)
            isFunding u = any (\Asset{..} -> assetPolicyId == askPolTxt && assetTokenName == askTokTxt) (personalValue u)
            fundUtxos = filter isFunding pUtxos
        if null fundUtxos
          then logError "No funding UTxO with required ask asset found." >> return False
          else do
            let fund = head fundUtxos
                fundRef = T.unpack (personalTxHash fund) ++ "#" ++ show (personalOutputIndex fund)
            -- compute beacon mint/burn argument
            let policyId   = show OneWay.beaconCurrencySymbol
                beaconName = showTokenName $ OneWay.genPairBeaconName (OfferAsset aoOfferAsset) (AskAsset aoAskAsset)
                mintArg    = "-1 " ++ policyId ++ "." ++ beaconName
                socketArg  = maybe [] (\sock -> ["--socket-path", sock]) aoNodeSocket
            -- Build transaction
            let buildArgs = ["transaction","build"
                             ,"--tx-in",utxoRef
                             ,"--tx-in",fundRef
                             ,"--tx-in-collateral",collRef
                             ,"--tx-in-script-file",swapScriptFile
                             ,"--tx-in-inline-datum-present"
                             ,"--tx-in-redeemer-file",rFile1
                             ,"--mint",mintArg
                             ,"--mint-script-file",beaconScriptFile
                             ,"--mint-redeemer-file",rFile2
                             ,"--change-address",agentAddr
                             ,"--protocol-params-file",paramsFile
                             ] ++ netFlag ++ socketArg ++ ["--out-file",txBody]
            _ <- readProcessStdout_ (proc "cardano-cli" buildArgs)
            -- Sign
            let signArgs = ["transaction","sign"
                           ,"--tx-body-file",txBody
                           ,"--signing-key-file",skFile
                           ,"--signing-key-file",vkFile
                           ,"--out-file",txSigned
                           ] ++ netFlag ++ socketArg
            _ <- readProcessStdout_ (proc "cardano-cli" signArgs)
            
            -- If in dry run mode, just display transaction info and exit
            when isDryRun $ do
               logInfo "🔍 DRY RUN MODE - Transaction prepared but not submitted."
               txInfo <- readProcessStdout_ (proc "cardano-cli" ["transaction", "view", "--tx-file", txSigned])
               putStrLn $ "Transaction details:\n" ++ byteStringToString txInfo
               pure ()
            
            -- Submit
            case (isDryRun, aoBlockfrostProjectId) of
              (True, _) -> pure True -- Already handled above
              (_, Just pid) -> do
                cbor <- BL.readFile txSigned
                let url = if null netFlag then "https://cardano-mainnet.blockfrost.io/api/v0/tx/submit"
                          else "https://cardano-testnet.blockfrost.io/api/v0/tx/submit"
                req0 <- parseRequest url
                let req = req0 { method = "POST"
                               , requestBody = RequestBodyLBS cbor
                               , requestHeaders = [("project_id", TE.encodeUtf8 (T.pack pid))]
                               }
                _ <- httpLbs req mgr
                return True
              (_, Nothing) -> do
                _ <- readProcessStdout_ (proc "cardano-cli" (["transaction","submit","--tx-file",txSigned] ++ netFlag ++ socketArg))
                return True

--------------------------------------------------------------------------------
-- Memory storage
--------------------------------------------------------------------------------
storeMem :: AgentOptions -> Manager -> MemoryEntry -> IO ()
storeMem AgentOptions{..} mgr entry = do
    -- Check if we're using OpenAI API
    let useOpenAIDirect = T.isInfixOf "openai.com" (T.pack aoApiBase)
    
    if useOpenAIDirect
    then do
      -- When using OpenAI, just log the memory but don't try to store it
      logInfo $ "Memory [OpenAI mode]: " ++ T.unpack (topic entry) ++ " - " ++ T.unpack (content entry)
    else do
      -- Standard Agent API path
      let url = aoApiBase <> "/knowledge"
      req0 <- parseRequest url
      let req = attachKey aoApiKey req0 { method = "POST"
                                        , requestBody = RequestBodyLBS (encode entry)
                                        , requestHeaders=[("Content-Type","application/json")] }
      _ <- httpLbs req mgr
      pure ()

--------------------------------------------------------------------------------
-- Simple logging
--------------------------------------------------------------------------------
logInfo, logError :: String -> IO ()
logInfo  msg = do
  now <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
  putStrLn $ "[" ++ now ++ "] [INFO ] " ++ msg

logError msg = do
  now <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
  putStrLn $ "[" ++ now ++ "] [ERROR] " ++ msg

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

-- | Build a memory topic tag from pair
pairTag :: AgentOptions -> T.Text
pairTag AgentOptions{..} =
  let key = formatAsset aoOfferAsset <> "-" <> formatAsset aoAskAsset
  in TE.decodeUtf8 $ BL.toStrict $ encode key

-- | Like whenM in Control.Monad.Extra
whenM :: IO Bool -> IO () -> IO ()
whenM mb action = mb >>= (`when` action)

-- | Convert BL.ByteString to String
byteStringToString :: BL.ByteString -> String
byteStringToString = T.unpack . TE.decodeUtf8 . BL.toStrict 