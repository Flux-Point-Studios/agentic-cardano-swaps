module Main where

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}

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

-- | Order-book style edge.  `seRate` is units of `seTo` per 1 unit of `seFrom`.
--   `seMaxQty` caps the amount of `seFrom` that can be taken (thin = small cap).
data SwapEdge = SwapEdge
  { seUtxo    :: T.Text   -- ^ UTxO reference (TxHash#Ix)
  , seFrom    :: T.Text   -- ^ Offered asset (policy.asset or "lovelace")
  , seTo      :: T.Text   -- ^ Asked   asset
  , seRate    :: Double   -- ^ Units of `seTo` received per 1 unit `seFrom`
  , seMaxQty  :: Double   -- ^ Maximum executable quantity of `seFrom`
  , seIsThin  :: Bool     -- ^ Flag for illiquid (True = thin)
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

-- | Helper to build a pool with a target price (y/x) while keeping total liquidity reasonable.
mkEdge :: T.Text -> T.Text -> T.Text -> Double -> SwapEdge
mkEdge utxo from to rate = SwapEdge utxo from to rate 10_000 False

-- | Illiquid edge with small quantity cap.
mkThinEdge :: T.Text -> T.Text -> T.Text -> Double -> Double -> SwapEdge
mkThinEdge utxo from to rate cap = SwapEdge utxo from to rate cap True

-- | Illiquid WMT/AGIX pool – only 4 k vs 3.5 k tokens.
illiquidWmtAgix = mkThinEdge "thin#0" "policyWMT.574d54" "policyAGIX.41474958" 0.875 40

-- | A collection of edge lists capturing different arbitrage possibilities.
--   We exercise the LLM with:
--   1. A simple 3-hop profit case (baseline).
--   2. A 4-hop chain with small but compound gains.
--   3. A 5-hop chain that starts negative yet ends positive.
--   4. A case where *no* profitable path exists (expect NoAction).

scenarios :: [(String, [SwapEdge])]
scenarios =
  [ ( "Three-hop chain profit"
    , [ mkEdge     "tx1#0" "lovelace"                 "policyTalos.54616c6f73" 0.98  -- liquid
      , mkThinEdge "tx2#1" "policyTalos.54616c6f73"  "policyXYZ.58595a"      1.01  30  -- THIN hop
      , mkEdge     "tx3#0" "policyXYZ.58595a"        "lovelace"                 1.10  -- liquid
      ] )

  , ( "Four-hop micro-arb"
    , [ mkEdge     "tx4#0" "lovelace"                 "policyFoo.464f4f"         1.00  -- liquid
      , mkThinEdge "tx4#1" "policyFoo.464f4f"        "policyBar.424152"         1.02  25  -- THIN
      , mkEdge     "tx4#2" "policyBar.424152"        "policyBaz.42415a"         1.01  -- liquid
      , mkThinEdge "tx4#3" "policyBaz.42415a"        "lovelace"                 1.03  25  -- THIN to exit
      ] )

  , ( "Five-hop delayed upside"
    , [ mkEdge     "tx5#0" "lovelace"                 "policyTalos.54616c6f73" 0.97  -- liquid
      , mkThinEdge "tx5#1" "policyTalos.54616c6f73"  "policyXYZ.58595a"      1.02  20  -- THIN
      , mkEdge     "tx5#2" "policyXYZ.58595a"        "policyFoo.464f4f"      0.99  -- liquid
      , mkThinEdge "tx5#3" "policyFoo.464f4f"        "policyBar.424152"      1.04  20  -- THIN
      , mkEdge     "tx5#4" "policyBar.424152"        "lovelace"                 1.10  -- liquid
      ] )

  , ( "Illiquid WMT/AGIX hop"
    , [ mkEdge "liq1#0" "lovelace"                 "policyWMT.574d54"        1.20  -- ADA/WMT liquid
      , illiquidWmtAgix                                                -- thin hop
      , mkEdge "liq1#1" "policyAGIX.41474958"     "lovelace"                 0.80  -- AGIX/ADA liquid
      ] )

  , ( "Hidden profit across illiquid bridge"
    , [ mkEdge     "vis1#0" "lovelace"           "policyAAA.414141"        0.95    --  -5 %
      , mkThinEdge "thin2#0" "policyAAA.414141"  "policyILL.494c4c"        0.97  15  -- thin, -3 %
      , mkEdge     "vis1#1" "policyILL.494c4c"  "lovelace"                 1.15    -- +15 %
      ] )

  , ( "Two thin negatives, big final upside"
    , [ mkEdge     "vis2#0" "lovelace"           "policyBBB.424242"        0.96    -- -4 %
      , mkThinEdge "thin3#0" "policyBBB.424242" "policyCCC.434343"        0.94  12  -- thin, -6 %
      , mkThinEdge "thin3#1" "policyCCC.434343" "policyDDD.444444"        0.95  12  -- thin, -5 %
      , mkEdge     "vis2#1" "policyDDD.444444" "lovelace"                 1.30    -- +30 %
      ] )

  , ( "No profitable path"
    , [ mkEdge "tx6#0" "lovelace"                 "policyTalos.54616c6f73" 0.99
      , mkEdge "tx6#1" "policyTalos.54616c6f73"  "policyXYZ.58595a"      0.99
      , mkEdge "tx6#2" "policyXYZ.58595a"        "lovelace"                 0.99
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
                 , responseTimeout = responseTimeoutMicro (120 * 1_000_000)  -- 120-second timeout
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
               ++ " | price " ++ show seRate
               ++ " | maxQty " ++ show seMaxQty
               ++ (if seIsThin then " (thin)" else "")

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

--------------------------------------------------------------------------------
-- Constant-product slippage helpers
--------------------------------------------------------------------------------

-- | Walk a path, mutating reserves and charging a fixed fee per hop.
--runPath :: Double        -- ^ initial amount (in units of first edge's `from`)
--        -> [SwapEdge]
--        -> (Double, [SwapEdge])  -- ^ (final out amount, updated edges)
--runPath startAmt = go startAmt []
--  where
--    dexFee = 0.003           -- 0.3 %
--
--    go amt acc [] = (amt, reverse acc)
--    go amt acc (e:es) =
--      let (out, x', y') = applyCPMM amt (seRate e, seMaxQty e)
--          out'          = out * (1 - dexFee)
--          e'            = e { seRate = x', seMaxQty = y' }
--      in  go out' (e' : acc) es 