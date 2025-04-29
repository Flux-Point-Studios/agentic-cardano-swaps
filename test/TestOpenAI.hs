{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import qualified Control.Lens as L
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
  = TakeSwap { aiUtxo :: T.Text, rationale :: T.Text }
  | CreateSwap { aiPrice :: Double, aiAmount :: Integer, rationale :: T.Text }
  | NoAction { rationale :: T.Text }
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

-- | Ask the OpenAI for a decision
askOpenAI :: String -> [SwapUTxO] -> [MemoryEntry] -> IO (Either SomeException (Maybe AIAction))
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
        
        -- Payload format for OpenAI Responses API
        payload = object [ 
            "model" A..= ("o4-mini" :: T.Text),
            "reasoning" A..= object [
                "effort" A..= ("medium" :: T.Text)
                -- No summary field to avoid verification issues if not fully propagated
            ],
            "input" A..= [ 
                object [
                    "role" A..= ("system" :: T.Text), 
                    "content" A..= sysMsg
                ],
                object [
                    "role" A..= ("user" :: T.Text), 
                    "content" A..= usrMsg
                ]
            ]
          ]
        
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
    putStrLn "Sending request to OpenAI API..."
    resp <- httpLbs req manager
    
    -- Parse OpenAI response
    putStrLn $ "Response status: " ++ show (responseStatus resp)
    let respBody = responseBody resp
    
    case decode respBody :: Maybe Value of
      Just responseObj -> do
        -- Extract the message content from the output array with correct path
        -- Format is: output[].content[].text (where type is "message" and "output_text")
        let messageTexts = responseObj ^.. key "output" . values 
                          . filtered (\o -> o ^? key "type" . _String == Just "message")
                          . key "content" . values
                          . filtered (\c -> c ^? key "type" . _String == Just "output_text")
                          . key "text" . _String
        
        case messageTexts of
          (outputText:_) -> do  -- Take first message if there are multiple
            putStrLn $ "OpenAI response: " ++ T.unpack (T.take 100 outputText) ++ "..."
            
            -- Try to parse JSON from the text
            case extractAndParseJSON outputText of
              Just action -> do
                putStrLn $ "Successfully parsed JSON action"
                return (Just action)
              Nothing -> do
                putStrLn "Failed to parse JSON from AI response"
                return Nothing
          [] -> do
            putStrLn "Could not find message content in the output array"
            return Nothing
      _ -> do
        putStrLn $ "Failed to decode API response"
        return Nothing

-- Helper function to extract and parse JSON from the LLM response
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

extractJson :: T.Text -> T.Text
extractJson t = do
  let p c = c == '}'
  let mindex = T.findIndex p t
  case mindex of
    Nothing -> T.empty
    Just index -> do
      let s = T.take (index + 1) t
      let prefix = "```json"
      case T.stripPrefix prefix s of
        Nothing -> T.empty 
        Just rest -> 
          let trimmed = T.stripStart rest
          in if T.null trimmed then T.empty else trimmed

main :: IO ()
main = do
  -- Get OpenAI API key from environment
  maybeApiKey <- lookupEnv "OPENAI_API_KEY"
  
  apiKey <- case maybeApiKey of
    Just k -> pure k
    Nothing -> do
      putStrLn "Error: OPENAI_API_KEY environment variable not set"
      putStrLn "Please set the environment variable with: export OPENAI_API_KEY=your_key_here"
      error "Missing API key"
  
  putStrLn "=== AI Agent Reasoning Test (Using OpenAI API) ==="
  putStrLn $ "API Key: " ++ take 5 apiKey ++ "..." ++ take 5 (reverse apiKey)
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
    
    putStrLn "\nAsking OpenAI for decision..."
    result <- askOpenAI apiKey swaps memory
    
    case result of
      Left err -> 
        putStrLn $ "Error from API: " ++ show err
      Right Nothing ->
        putStrLn "Failed to get a valid AI response"
      Right (Just action) -> do
        putStrLn $ "AI Decision: " ++ show action
        putStrLn "\nRationale:"
        case action of
          TakeSwap _ r -> putStrLn $ T.unpack r
          CreateSwap _ _ r -> putStrLn $ T.unpack r
          NoAction r -> putStrLn $ T.unpack r
    
    putStrLn "\n-----------------------------------\n"
  
  putStrLn "Test completed." 