module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (encode, object, (.=))
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception (try, SomeException)
import System.Environment (getArgs)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)

main :: IO ()
main = do
  args <- getArgs
  -- Use the configured values
  let apiUrl = "https://api.fluxpointstudios.com"
      apiKey = "16dc5e5e6671ae253eded051fab30876ca44d23c8089af7a07679528ed21eee2"
      
  -- Initialize HTTP manager
  manager <- newManager tlsManagerSettings
  
  putStrLn "Testing API connection..."
  putStrLn $ "API URL: " ++ apiUrl
  putStrLn $ "Using API key: " ++ take 10 apiKey ++ "..."
  
  result <- testChatEndpoint manager apiUrl apiKey
  
  case result of
    Left err -> putStrLn $ "Error connecting to API: " ++ show err
    Right response -> do
      putStrLn "Successfully connected to API!"
      putStrLn $ "Response length: " ++ show (BL.length response) ++ " bytes"
      putStrLn $ "Response preview: " ++ (T.unpack . TE.decodeUtf8 . BL.toStrict . BL.take 200) response
      putStrLn "Full response (truncated to 1000 chars):"
      putStrLn $ take 1000 $ T.unpack $ TE.decodeUtf8 $ BL.toStrict response

-- | Test the /chat endpoint with a simple request
testChatEndpoint :: Manager -> String -> String -> IO (Either SomeException BL.ByteString)
testChatEndpoint mgr apiUrl apiKey = try $ do
  -- Create request
  let url = apiUrl ++ "/chat"
      sysMsg = "You are an autonomous Cardano swap-bot." :: T.Text
      usrMsg = "Hello! What can you tell me about Cardano swaps?" :: T.Text
  
  -- Generate a session ID
  sessionId <- toString <$> nextRandom
  
  let payload = object [ 
          "messages" .= [ 
              object ["role" .= ("system" :: T.Text), "content" .= sysMsg]
            , object ["role" .= ("user" :: T.Text), "content" .= usrMsg]
          ]
        , "message" .= usrMsg
        , "session_id" .= sessionId
        ]
  
  -- Build and execute request
  initialRequest <- parseRequest url
  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS (encode payload)
        , requestHeaders = 
            [ ("Content-Type", "application/json")
            -- Try several auth header formats
            , ("Authorization", TE.encodeUtf8 (T.pack apiKey))
            , ("X-API-Key", TE.encodeUtf8 (T.pack apiKey))
            ]
        }
  
  putStrLn "Sending request..."
  putStrLn $ "Session ID: " ++ sessionId
  -- Send request and return response body
  response <- httpLbs request mgr
  return $ responseBody response 