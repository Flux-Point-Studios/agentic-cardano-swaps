{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getArgs, lookupEnv)
import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import GHC.Generics (Generic)
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified Data.Aeson.KeyMap as KM

-- | Data types for the test

-- Actions the AI can take
data AIAction
  = TakeSwap { aiUtxo :: T.Text }
  | CreateSwap { aiPrice :: Double, aiAmount :: Integer }
  | NoAction
  deriving (Show, Generic)

instance FromJSON AIAction

-- Mock swap data
data SwapUTxO = SwapUTxO
  { txHash :: T.Text
  , outputIndex :: Int
  , price :: Double
  , amount :: Integer
  } deriving (Show, Generic)

instance ToJSON SwapUTxO

-- Mock memory entry
data MemoryEntry = MemoryEntry
  { topic :: T.Text
  , content :: T.Text
  , timestamp :: T.Text
  } deriving (Show, Generic)

instance ToJSON MemoryEntry

-- Test scenarios
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

-- | Create mock swap UTXOs from price/amount/utxo triple
mockSwaps :: [(Double, Integer, T.Text)] -> [SwapUTxO]
mockSwaps = map createMockSwap
  where
    createMockSwap (p, a, utxoRef) = SwapUTxO
      { txHash = T.takeWhile (/= '#') utxoRef
      , outputIndex = read $ T.unpack $ T.drop 1 $ T.dropWhile (/= '#') utxoRef
      , price = p
      , amount = a
      }

-- | Create mock memory entries
mockMemory :: [T.Text] -> [MemoryEntry]
mockMemory = map createMemoryEntry
  where
    createMemoryEntry content = MemoryEntry
      { topic = "ada-1d7f33bd23d85e1a25d87d73fcf1df8d7d72b7756c165fc178fb3c1.5457"
      , content = content
      , timestamp = "2023-04-29T12:00:00Z"
      }

-- | Ask the AI for a decision
askAI :: String -> Maybe String -> [SwapUTxO] -> [MemoryEntry] -> IO (Either SomeException (Maybe AIAction))
askAI apiBase apiKey swaps mems = try $ do
    -- Initialize HTTP manager
    manager <- newManager tlsManagerSettings
    
    -- Generate session ID
    sessionId <- UUID.toString <$> UUID.nextRandom
    
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
          , "session_id" .= sessionId
          ]
        
        url = apiBase <> "/chat"
    
    -- Log for debug purposes
    putStrLn $ "Using session ID: " ++ sessionId
    req0 <- parseRequest url
    
    -- Add authentication headers
    let req = case apiKey of
          Nothing -> req0 { requestHeaders = [("Content-Type", "application/json")] }
          Just k -> req0 { requestHeaders = 
                [ ("Content-Type", "application/json")
                , ("Authorization", TE.encodeUtf8 (T.pack ("Bearer " ++ k)))
                ] }
        
        -- Add request body
        finalReq = req { 
            method = "POST"
          , requestBody = RequestBodyLBS (encode payload)
          }
    
    -- Send request
    resp <- httpLbs finalReq manager
    
    -- Parse assistant reply
    case decode (responseBody resp) of
      Just obj -> do
        -- Try to parse the response as an object
        case obj of
          Object o -> 
            -- Look for the reply field
            case KM.lookup "reply" o of
              Just (String replyText) ->
                case decode (BL.fromStrict $ TE.encodeUtf8 replyText) of
                  Just action -> return (Just action)
                  Nothing -> do
                    putStrLn $ "Raw AI response: " ++ T.unpack replyText
                    return Nothing
              _ -> do
                putStrLn $ "No reply field in response: " ++ show obj
                return Nothing
          _ -> do
            putStrLn $ "Response is not an object: " ++ show obj
            return Nothing
      Nothing -> do
        putStrLn $ "Failed to decode API response: " ++ show (responseBody resp)
        return Nothing

main :: IO ()
main = do
  args <- getArgs
  
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
    result <- askAI apiBase (Just apiKey) swaps memory
    
    case result of
      Left err -> 
        putStrLn $ "Error from API: " ++ show err
      Right Nothing ->
        putStrLn "Failed to get a valid AI response"
      Right (Just action) -> 
        putStrLn $ "AI Decision: " ++ show action
    
    putStrLn "\n-----------------------------------\n"
  
  putStrLn "Test completed." 