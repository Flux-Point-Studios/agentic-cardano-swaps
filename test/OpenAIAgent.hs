module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try, catch, throwIO)
import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe, isJust)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import GHC.Generics (Generic)

-- | AI action data type with rationale
data AIAction
  = TakeSwap { aiUtxo :: T.Text, aiRationale :: T.Text }
  | CreateSwap { aiPrice :: Double, aiAmount :: Integer, aiRationale :: T.Text }
  | NoAction { aiRationale :: T.Text }
  deriving (Show, Generic)

-- | JSON parser for AIAction
instance FromJSON AIAction where
  parseJSON = withObject "AIAction" $ \v -> do
    action <- v .: "action"
    rationale <- v .: "rationale"
    case (action :: T.Text) of
      "TakeSwap" -> TakeSwap <$> v .: "aiUtxo" <*> pure rationale
      "CreateSwap" -> CreateSwap <$> v .: "aiPrice" <*> v .: "aiAmount" <*> pure rationale
      "NoAction" -> pure $ NoAction rationale
      _ -> fail $ "Unknown action: " ++ T.unpack action

-- | Swap UTxO type for simulating available swaps
data SwapUTxO = SwapUTxO
  { txHash :: T.Text
  , outputIndex :: Int
  , price :: Double
  , amount :: Integer
  } deriving (Show, Generic)

-- | JSON encoder for SwapUTxO
instance ToJSON SwapUTxO

-- | Memory entry for tracking history
data MemoryEntry = MemoryEntry
  { topic :: T.Text
  , content :: T.Text
  , timestamp :: T.Text
  } deriving (Show, Generic)

-- | JSON encoder for MemoryEntry
instance ToJSON MemoryEntry

-- | Improved OpenAI reasoning for swap decisions
askOpenAI :: String -> [SwapUTxO] -> [Value] -> IO (Either SomeException (Maybe AIAction))
askOpenAI apiKey swaps mems = try $ do
    -- Initialize HTTP manager
    manager <- newManager tlsManagerSettings
    
    -- Prepare system and user messages
    let sysMsg :: T.Text
        sysMsg = "You are an autonomous Cardano swap-bot that helps trade tokens on the Cardano blockchain. You need to analyze available swap opportunities and make strategic trading decisions based on market data, price trends, and previous actions in memory. Always provide detailed reasoning for your decisions."
        ordJson = encode swaps
        memJson = encode mems
        usrMsg :: T.Text
        usrMsg = "I'm looking for your trading recommendation based on this data.\n\nAvailable Orders (swap opportunities):\n" <> TE.decodeUtf8 (BL.toStrict ordJson)
              <> "\n\nPrevious Trading History:\n" <> TE.decodeUtf8 (BL.toStrict memJson)
              <> "\n\nAnalyze this data and provide a JSON object representing your decision WITH A DETAILED EXPLANATION OF YOUR REASONING. Consider price trends, market gaps, and trading strategies. You must respond in valid JSON with one of these formats:\n"
              <> "1. To take an existing swap: {\"action\":\"TakeSwap\",\"aiUtxo\":\"txhash#index\",\"rationale\":\"your detailed reasoning\"}\n"
              <> "2. To create a new swap: {\"action\":\"CreateSwap\",\"aiPrice\":0.45,\"aiAmount\":1000000,\"rationale\":\"your detailed reasoning\"}\n"
              <> "3. To do nothing: {\"action\":\"NoAction\",\"rationale\":\"your detailed reasoning\"}\n"
              <> "\nYour response must be valid JSON that follows one of these formats exactly. In the rationale field, provide a thorough explanation of why this is the optimal decision."
        
        -- Payload format for OpenAI API
        payload = object [ 
            "model" A..= ("gpt-4o-mini" :: T.Text),
            "messages" A..= [ 
                object ["role" A..= ("system" :: T.Text), "content" A..= sysMsg]
              , object ["role" A..= ("user" :: T.Text), "content" A..= usrMsg]
            ],
            "temperature" A..= (0.2 :: Double),
            "max_tokens" A..= (1000 :: Int)
          ]
        
        url = "https://api.openai.com/v1/chat/completions"
    
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
    putStrLn "Sending request to OpenAI API..."
    resp <- httpLbs req manager
    
    -- Parse OpenAI response
    putStrLn $ "Response status: " ++ show (responseStatus resp)
    let respBody = responseBody resp
    
    case decode respBody :: Maybe Value of
      Just responseObj -> do
        -- Extract the content from the response
        case responseObj ^? key "choices" . nth 0 . key "message" . key "content" . _String of
          Just outputText -> do
            putStrLn $ "OpenAI response: " ++ T.unpack (T.take 100 outputText) ++ "..."
            
            -- Try to parse JSON from the text
            case extractAndParseJSON outputText of
              Just action -> do
                putStrLn $ "Successfully parsed AI action"
                return (Just action)
              Nothing -> do
                putStrLn "Failed to parse JSON from AI response"
                return Nothing
          Nothing -> do
            putStrLn "Could not find content in response"
            return Nothing
      _ -> do
        putStrLn $ "Failed to decode API response: " ++ show respBody
        return Nothing

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

-- | Sample swaps data for testing
mockSwaps :: [(Double, Integer, T.Text)] -> [SwapUTxO]
mockSwaps = map createMockSwap
  where
    createMockSwap (p, a, utxoRef) = SwapUTxO
      { txHash = T.takeWhile (/= '#') utxoRef
      , outputIndex = read $ T.unpack $ T.drop 1 $ T.dropWhile (/= '#') utxoRef
      , price = p
      , amount = a
      }

-- | Sample memory entries for testing
mockMemory :: [T.Text] -> [MemoryEntry]
mockMemory = map createMemoryEntry
  where
    createMemoryEntry content = MemoryEntry
      { topic = "ada-1d7f33bd23d85e1a25d87d73fcf1df8d7d72b7756c165fc178fb3c1.5457"
      , content = content
      , timestamp = "2023-04-29T12:00:00Z"
      }

-- | Sample test scenarios for agent
scenarios :: [(String, [(Double, Integer, T.Text)], [T.Text])]
scenarios =
  [ ("Taker Scenario (Multiple good swaps available)", 
     [ (0.5, 1000000, "txhash1#0")  -- 0.5 ADA per token, 1M tokens
     , (0.4, 500000, "txhash2#1")   -- 0.4 ADA per token, 500K tokens (better price)
     , (0.45, 2000000, "txhash3#0") -- 0.45 ADA per token, 2M tokens
     ],
     ["Previously took swap at 0.47 ADA per token"])
    
  , ("Maker Scenario (Market gap opportunity)", 
     [ (0.6, 1000000, "txhash1#0")  -- 0.6 ADA per token, 1M tokens
     , (0.65, 500000, "txhash2#1")  -- 0.65 ADA per token, 500K tokens
     , (0.7, 2000000, "txhash3#0")  -- 0.7 ADA per token, 2M tokens
     ],
     ["Created swap yesterday at 0.62 ADA per token"])
    
  , ("Market Analysis (Price trend monitoring)",
     [ (0.55, 1000000, "txhash1#0")  -- Current offers
     , (0.57, 500000, "txhash2#1")
     , (0.59, 2000000, "txhash3#0")
     ],
     [ "Created swap 3 days ago at 0.50 ADA per token"
     , "Created swap 2 days ago at 0.52 ADA per token" 
     , "Created swap yesterday at 0.53 ADA per token"
     ])
  ]

-- | Main entry point - test agent with OpenAI reasoning
main :: IO ()
main = do
  -- Get OpenAI API key from environment
  mApiKey <- lookupEnv "OPENAI_API_KEY"
  
  -- Verify we have an API key
  case mApiKey of
    Nothing -> do
      hPutStrLn stderr "Error: OPENAI_API_KEY environment variable not set"
      hPutStrLn stderr "Please export OPENAI_API_KEY=your_key_here"
      exitFailure
    Just apiKey -> do
      putStrLn "====================================================================="
      putStrLn "AI Agent Reasoning Simulation for Cardano Swaps"
      putStrLn "====================================================================="
      putStrLn $ "API Key: " ++ take 5 apiKey ++ "..." ++ take 5 (reverse apiKey)
      putStrLn "Using the improved OpenAI reasoning model (GPT-4o-mini)"
      putStrLn "====================================================================="
      
      -- Test each scenario
      forM_ scenarios $ \(name, swapData, memoryData) -> do
        putStrLn $ "\n## Scenario: " ++ name
        putStrLn $ "Available swaps: " ++ show (length swapData)
        
        -- Create the mock data
        let swaps = mockSwaps swapData
            memory = map toJSON (mockMemory memoryData)
        
        putStrLn "Swaps:"
        forM_ swapData $ \(price, amount, utxoRef) ->
          putStrLn $ "- UTxO: " ++ T.unpack utxoRef 
                    ++ ", Price: " ++ show price ++ " ADA/token" 
                    ++ ", Amount: " ++ show amount ++ " tokens"
        
        putStrLn "Memory:"
        forM_ memoryData $ \entry ->
          putStrLn $ "- " ++ T.unpack entry
        
        -- Ask OpenAI for decision
        result <- askOpenAI apiKey swaps memory
        
        -- Process and print result
        case result of
          Left err -> 
            putStrLn $ "Error from API: " ++ show err
          Right Nothing ->
            putStrLn "Failed to get a valid AI response"
          Right (Just action) -> do
            putStrLn $ "AI Decision: " ++ showAction action
            putStrLn "\nRationale:"
            case action of
              TakeSwap _ r -> putStrLn $ T.unpack r
              CreateSwap _ _ r -> putStrLn $ T.unpack r
              NoAction r -> putStrLn $ T.unpack r
        
        putStrLn "\n-----------------------------------"
      
      putStrLn "\nTest completed."

-- | Helper function to display AI decisions
showAction :: AIAction -> String
showAction (TakeSwap utxo _) = "TakeSwap " ++ T.unpack utxo
showAction (CreateSwap price amount _) = "CreateSwap price=" ++ show price ++ ", amount=" ++ show amount
showAction (NoAction _) = "NoAction"

-- | Helper function similar to forM_
forM_ :: [a] -> (a -> IO ()) -> IO ()
forM_ xs f = mapM_ f xs 