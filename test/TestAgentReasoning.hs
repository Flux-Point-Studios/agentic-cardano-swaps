{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CLI.Types
import Data.Aeson (encode, decode, object, (.=), ToJSON(..), Value(..), FromJSON(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getArgs, lookupEnv)
import System.Directory (getHomeDirectory)
import Control.Concurrent.MVar (newMVar, MVar, takeMVar, putMVar)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Exception (try, SomeException)
import Data.Time.Clock (getCurrentTime, UTCTime)
import System.IO (Handle)
import Control.Monad (forM_)
import qualified Data.Aeson.KeyMap as KM
import GHC.Generics (Generic)

-- | Data types needed for testing that are not in CLI.Types
data TestRatio = TestRatio
  { testNumerator :: Integer
  , testDenominator :: Integer
  } deriving (Show)

data TestOneWaySwapDatum = TestOneWaySwapDatum
  { testSwapPrice :: TestRatio
  , testSwapBeacon :: String
  , testSwapOwner :: Maybe String
  , testSwapExpiration :: Maybe Integer
  } deriving (Show)

-- | Mock Swap UTxO for testing
mockSwaps :: [(Double, Integer, T.Text)] -> [SwapUTxO]
mockSwaps priceAmountUtxos = map createMockSwap priceAmountUtxos
  where
    createMockSwap (price, amount, utxoRef) = SwapUTxO
      { swapTxHash = T.takeWhile (/= '#') utxoRef
      , swapOutputIndex = read $ T.unpack $ T.drop 1 $ T.dropWhile (/= '#') utxoRef
      , swapAddress = UserAddress "addr_test1xyz123..."  -- Use UserAddress constructor
      , swapDatum = Just (OneWayDatum (T.pack $ show amount))  -- Use Text for dummy datum
      , swapValue = [mockAsset amount]
      }

    mockAsset amount = Asset
      { assetPolicyId = "1d7f33bd23d85e1a25d87d73fcf1df8d7d72b7756c165fc178fb3c1"
      , assetTokenName = "5457"
      , assetQuantity = T.pack $ show amount
      }

-- | Create mock memory entries
mockMemory :: [T.Text] -> [Value]
mockMemory msgs = map createMemoryEntry msgs
  where
    createMemoryEntry msg = object
      [ "topic" .= ("ada-1d7f33bd23d85e1a25d87d73fcf1df8d7d72b7756c165fc178fb3c1.5457" :: T.Text)
      , "content" .= msg
      , "ts" .= ("2023-04-29T12:00:00Z" :: T.Text)
      ]

-- | Test scenarios
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

-- | Agent state for tracking API context
data AgentState = AgentState
  { asFailCount   :: Int          -- consecutive failures
  , asBackoffTime :: Int          -- current backoff in microseconds  
  , asLastFail    :: Maybe UTCTime -- time of last failure
  , asLogHandle   :: Maybe Handle  -- current log file handle
  , asLogDay      :: Maybe Int     -- current log day
  , asSessionId   :: Maybe String  -- current AI conversation session ID
  }

instance Show AgentState where
  show _ = "AgentState {...}"

-- | AiAction returned by Agent API
data AIAction
  = TakeSwap   { aiUtxo :: T.Text }
  | CreateSwap { aiPrice :: Double, aiAmount :: Integer }
  | NoAction
  deriving (Show, Generic)
instance FromJSON AIAction

-- | Custom implementation of the askAI function based on the original in CLI.Agent
askAI :: AgentOptions -> Manager -> MVar AgentState -> [SwapUTxO] -> [Value] -> IO AIAction
askAI AgentOptions{..} mgr stateMVar swaps mems = do
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
        
        -- Payload format matching API requirements
        payload = object [ 
            "messages" .= [ 
                object ["role" .= ("system"::T.Text), "content" .= (sysMsg::T.Text)]
              , object ["role" .= ("user"::T.Text), "content" .= usrMsg]
            ]
          , "message" .= usrMsg
          , "session_id" .= (T.pack sessionId)
          ]
        
        url = aoApiBase <> "/chat"
    
    -- Log for debug purposes
    putStrLn $ "Using session ID: " ++ sessionId
    req0 <- parseRequest url
    
    -- Add authentication headers
    let req = case aoApiKey of
          Nothing -> req0
          Just k -> req0 { requestHeaders = 
                [ ("Content-Type", "application/json")
                , ("Authorization", TE.encodeUtf8 (T.pack k))
                , ("Authorization", TE.encodeUtf8 (T.pack $ "Bearer " ++ k))
                , ("X-API-Key", TE.encodeUtf8 (T.pack k))
                ] }
        
        -- Add request body
        finalReq = req { 
            method = "POST"
          , requestBody = RequestBodyLBS (encode payload)
          , requestHeaders = ("Content-Type", "application/json") : requestHeaders req
          }
    
    -- Send request
    resp <- httpLbs finalReq mgr
    
    -- Parse assistant reply
    case decode (responseBody resp) of
      Just v -> do
        case KM.lookup "reply" v of
          Just (String txt) -> case decode (BL.fromStrict $ TE.encodeUtf8 txt) of
            Just act -> pure act
            _ -> do
              putStrLn $ "AI response: " ++ T.unpack txt
              pure NoAction
          Just other -> do
            putStrLn $ "Unexpected reply format: " ++ show other
            pure NoAction
          Nothing -> do
            putStrLn $ "No 'reply' field in response: " ++ show v
            pure NoAction
      Nothing -> do
        putStrLn $ "Failed to decode API response"
        pure NoAction

main :: IO ()
main = do
  args <- getArgs
  home <- getHomeDirectory
  
  -- Create stateMVar for agent
  stateMVar <- newMVar $ AgentState 0 0 Nothing Nothing Nothing Nothing
  
  -- Get API credentials - either from args or environment
  apiKey <- case args of
    (k:_) -> pure k
    _ -> do
      envKey <- lookupEnv "AGENT_API_KEY"
      case envKey of
        Just k -> pure k
        Nothing -> pure "16dc5e5e6671ae253eded051fab30876ca44d23c8089af7a07679528ed21eee2" -- Default key
  
  apiBase <- lookupEnv "AGENT_API_URL" >>= \murl -> 
    case murl of
      Just url -> pure url
      Nothing -> pure "https://api.fluxpointstudios.com"
      
  let config = AgentOptions
        { aoSigningKey = ""  -- Not used in this test
        , aoVerificationKey = ""  -- Not used in this test
        , aoAgentAddress = ""  -- Not used in this test
        , aoOfferAsset = ("", "")  -- ADA
        , aoAskAsset = ("1d7f33bd23d85e1a25d87d73fcf1df8d7d72b7756c165fc178fb3c1", "5457")
        , aoNetworkMagic = Just 1
        , aoDryRun = True
        , aoBlockfrostProjectId = Nothing
        , aoNodeSocket = Nothing
        , aoApiBase = apiBase
        , aoApiKey = Just apiKey
        , aoPollSeconds = 5
        , aoProtocolParamsFile = Nothing
        , aoMakerOnly = False  -- Test both maker and taker
        }
  
  -- Initialize HTTP manager
  manager <- newManager tlsManagerSettings
  
  putStrLn "=== AI Agent Reasoning Test ==="
  putStrLn $ "Using API at: " ++ apiBase
  putStrLn $ "API Key: " ++ take 10 apiKey ++ "..."
  putStrLn ""
  
  -- Test each scenario
  forM_ scenarios $ \(name, swapData, memoryData) -> do
    putStrLn $ "## Scenario: " ++ name
    putStrLn $ "Available swaps: " ++ show (length swapData)
    
    -- Create the mock data
    let swaps = mockSwaps swapData
        memory = mockMemory memoryData
    
    putStrLn "Swaps:"
    forM_ swapData $ \(price, amount, utxoRef) ->
      putStrLn $ "- UTxO: " ++ T.unpack utxoRef 
                ++ ", Price: " ++ show price ++ " ADA/token" 
                ++ ", Amount: " ++ show amount ++ " tokens"
    
    putStrLn "Memory:"
    forM_ memoryData $ \entry ->
      putStrLn $ "- " ++ T.unpack entry
    
    putStrLn "\nAsking AI for decision..."
    result <- try $ askAI config manager stateMVar swaps memory
    
    case result of
      Left (err :: SomeException) -> 
        putStrLn $ "Error from API: " ++ show err
      Right action -> do
        putStrLn $ "AI Decision: " ++ show action
    
    putStrLn "\n-----------------------------------\n"
  
  putStrLn "Test completed." 