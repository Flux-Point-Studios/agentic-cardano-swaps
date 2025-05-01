module Main where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This test demonstrates reasoning about multi-step swap paths where
--   the eventual profit is only realised after several conversions.
--   It is meant to showcase how well the LLM (o4-mini via the
--   /v1/responses endpoint) can plan across multiple swaps rather than
--   a single immediate opportunity.
--
--   The AI is instructed to output JSON of the form:
--
--   { "action": "ChainedSwap"
--   , "utxos": ["tx1#0", "tx2#1", ...]  -- ordered execution path
--   , "expectedProfitADA": 2.35            -- final net profit in ADA
--   , "rationale": "..."                  -- detailed explanation
--   }
--
--   The test prints the chosen path and the rationale.

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                , (.:)
                                                , (.=)
                                                , object
                                                , encode
                                                , withObject
                                                )
import qualified Data.Aeson                     as A
import           Data.Aeson.Lens                ( key, _String, values )
import           Control.Lens                   ( (^?), (^..), filtered )
import           GHC.Generics                   ( Generic )
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Environment             ( lookupEnv )
import           Control.Exception              ( try, SomeException )
import           Control.Monad                  ( forM_ )
import           Data.Aeson.Types               ( Parser )

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

-- | A single swap that can be executed from one asset to another.
--   Only the high-level details are needed for LLM reasoning.
--   (UTxO reference, from asset, to asset and effective price).

data SwapEdge = SwapEdge
  { seUtxo   :: T.Text   -- ^ UTxO reference (TxHash#Ix)
  , seFrom   :: T.Text   -- ^ Offered asset policy.asset or "lovelace"
  , seTo     :: T.Text   -- ^ Asked asset
  , seRate   :: Double   -- ^ Units of `seTo` received per 1 unit of `seFrom`
  } deriving (Show, Generic)

instance ToJSON SwapEdge

-- | AI response capturing a chain of swaps.

data AIAction
  = ChainedSwap { aiUtxos :: [T.Text]
                , aiExpectedProfit :: Double
                , aiRationale :: T.Text }
  | NoAction { aiRationale :: T.Text }
  deriving (Show, Generic)

instance FromJSON AIAction where
  parseJSON = withObject "AIAction" $ \v -> do
    action <- v .: "action" :: Parser T.Text
    case action of
      "ChainedSwap" -> ChainedSwap
                          <$> v .: "utxos"
                          <*> v .: "expectedProfitADA"
                          <*> v .: "rationale"
      "NoAction"    -> NoAction <$> v .: "rationale"
      _              -> fail $ "Unknown action: " ++ T.unpack action

--------------------------------------------------------------------------------
-- Test scenarios
--------------------------------------------------------------------------------

-- | A collection of edge lists capturing different arbitrage possibilities.
--   We exercise the LLM with:
--   1. A simple 3-hop profit case (baseline).
--   2. A 4-hop chain with small but compound gains.
--   3. A 5-hop chain that starts negative yet ends positive.
--   4. A case where *no* profitable path exists (expect NoAction).

scenarios :: [(String, [SwapEdge])]
scenarios =
  [ ( "Three-hop chain profit"
    , [ SwapEdge "tx1#0" "lovelace"                 "policyTalos.54616c6f73" 0.98  -- -2 %
      , SwapEdge "tx2#1" "policyTalos.54616c6f73"  "policyXYZ.58595a"      1.01  -- +1 %
      , SwapEdge "tx3#0" "policyXYZ.58595a"        "lovelace"                 1.10  -- +10 %
      ] )

  , ( "Four-hop micro-arb"
    , [ SwapEdge "tx4#0" "lovelace"                 "policyFoo.464f4f"         1.00  -- neutral
      , SwapEdge "tx4#1" "policyFoo.464f4f"        "policyBar.424152"         1.02  -- +2 %
      , SwapEdge "tx4#2" "policyBar.424152"        "policyBaz.42415a"         1.01  -- +1 %
      , SwapEdge "tx4#3" "policyBaz.42415a"        "lovelace"                 1.03  -- +3 %
      ] )  -- overall ≈ +6 %

  , ( "Five-hop delayed upside"
    , [ SwapEdge "tx5#0" "lovelace"                 "policyTalos.54616c6f73" 0.97  -- -3 %
      , SwapEdge "tx5#1" "policyTalos.54616c6f73"  "policyXYZ.58595a"      1.02  -- +2 %
      , SwapEdge "tx5#2" "policyXYZ.58595a"        "policyFoo.464f4f"      0.99  -- -1 %
      , SwapEdge "tx5#3" "policyFoo.464f4f"        "policyBar.424152"      1.04  -- +4 %
      , SwapEdge "tx5#4" "policyBar.424152"        "lovelace"                 1.10  -- +10 %
      ] ) -- cumulative ≈ +9.7 %

  , ( "No profitable path"
    , [ SwapEdge "tx6#0" "lovelace"                 "policyTalos.54616c6f73" 0.99
      , SwapEdge "tx6#1" "policyTalos.54616c6f73"  "policyXYZ.58595a"      0.99
      , SwapEdge "tx6#2" "policyXYZ.58595a"        "lovelace"                 0.99
      ] )
  ]

--------------------------------------------------------------------------------
-- OpenAI call helper
--------------------------------------------------------------------------------

askOpenAI :: String                 -- ^ API key
          -> [SwapEdge]             -- ^ Available swap edges
          -> IO (Either SomeException (Maybe AIAction))
askOpenAI apiKey edges = try $ do
  manager <- newManager tlsManagerSettings

  -- Craft system and user messages
  let sysMsg :: T.Text
      sysMsg = "You are an advanced autonomous trader on Cardano. "
            <> "Consider all available single-step swaps and identify if a "
            <> "multi-step sequence (of any length <= 7) can yield positive "
            <> "ADA profit after fees. Optimise for maximum profit, even if "
            <> "initial steps are negative."

      usrMsg :: T.Text
      usrMsg = let jsonText = TE.decodeUtf8 $ BL.toStrict $ encode edges
                 in  "Here are the available swaps (directed edges):\n" <> jsonText
                 <> "\nEvaluate if any ordered sequence (≤ 7 hops) yields positive ADA after fees."
                 <> " Reply in JSON ONLY with either:\n"
                 <> "{\"action\": \"ChainedSwap\", \"utxos\": [...], \"expectedProfitADA\": <double>, \"rationale\": \"...\"}\n"
                 <> "or {\"action\": \"NoAction\", \"rationale\": \"...\"}."
                 <> " In the rationale, give up to 5 concise sentences explaining your choice."

      payload = object
        [ "model"     .= ("o4-mini" :: T.Text)
        , "reasoning" .= object [ "effort" .= ("medium" :: T.Text) ]
        , "input"     .=
            [ object [ "role" .= ("system" :: T.Text)
                     , "content" .= sysMsg ]
            , object [ "role" .= ("user" :: T.Text)
                     , "content" .= usrMsg ]
            ]
        ]

  req0 <- parseRequest "https://api.openai.com/v1/responses"
  let req = req0 { method = "POST"
                 , requestHeaders = [ ("Content-Type","application/json")
                                    , ("Authorization","Bearer " <> TE.encodeUtf8 (T.pack apiKey))
                                    ]
                 , requestBody = RequestBodyLBS (encode payload)
                 }

  resp <- httpLbs req manager

  -- Extract output_text messages
  let respBody = responseBody resp
      texts = respBody
              ^.. key "output" . values
              . filtered (\o -> o ^? key "type" . _String == Just "message")
              . key "content" . values
              . filtered (\c -> c ^? key "type" . _String == Just "output_text")
              . key "text" . _String
  case texts of
    (t:_) -> pure $ extractAndParseJSON t
    _     -> pure Nothing

  where
    -- | Attempt to parse JSON from the raw LLM text,
    --   falling back to detecting the first {...} block.
    extractAndParseJSON :: T.Text -> Maybe AIAction
    extractAndParseJSON txt =
      let direct = A.decode @AIAction (BL.fromStrict $ TE.encodeUtf8 txt)
       in case direct of
            Just a  -> Just a
            Nothing -> do
              let start = T.findIndex (== '{') txt
                  end   = findLastIndex (== '}') txt
              case (start, end) of
                (Just s, Just e) ->
                  let jsonTxt = T.take (e - s + 1) (T.drop s txt)
                  in A.decode @AIAction (BL.fromStrict $ TE.encodeUtf8 jsonTxt)
                _ -> Nothing

    findLastIndex p t = let idxs = [ i | i <- [0 .. T.length t - 1], p (T.index t i) ]
                        in if null idxs then Nothing else Just (last idxs)

--------------------------------------------------------------------------------
-- Main driver
--------------------------------------------------------------------------------

main :: IO ()
main = do
  mKey <- lookupEnv "OPENAI_API_KEY"
  key  <- maybe (fail "OPENAI_API_KEY not set") pure mKey

  forM_ scenarios $ \(name, edges) -> do
    putStrLn $ "\n=== Scenario: " ++ name
    putStrLn "Available edges:"
    forM_ edges $ \SwapEdge{..} ->
      putStrLn $ "  - " ++ T.unpack seUtxo ++ ": " ++ T.unpack seFrom ++ " -> " ++ T.unpack seTo
                 ++ " @ rate " ++ show seRate

    result <- askOpenAI key edges
    case result of
      Left err -> putStrLn $ "Error calling API: " ++ show err
      Right Nothing -> putStrLn "Failed to parse AI response."
      Right (Just act) -> case act of
        ChainedSwap uts profit r -> do
          putStrLn $ "Chosen path: " ++ show uts
          putStrLn $ "Expected profit (ADA): " ++ show profit
          putStrLn $ "Rationale: " ++ T.unpack r
        NoAction r -> putStrLn $ "AI chose NoAction: " ++ T.unpack r 