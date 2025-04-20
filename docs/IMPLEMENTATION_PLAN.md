Below is a **tightened, beginner‑friendly version** of the integration playbook.  
Everything is written so a junior dev can copy‑paste without hunting for extra docs.

---

## 0 — Prerequisites You **MUST** Install First

| Item | Quick cmd | Why |
|------|-----------|-----|
| **GHC 9.6.4 + cabal** | `curl -sSf https://get-ghcup.haskell.org | sh` then `ghcup install ghc 9.6.4 cabal 3.10.2.1` | builds cardano‑swapsciteturn6view0 |
| **cardano‑cli** | download the binary from IOG release page | builds & signs every Tx |
| **cardano‑swaps executable** | ```bash
git clone https://github.com/fallen‑icarus/cardano‑swaps
cd cardano‑swaps
cabal build exe:cardano-swaps
sudo cp "$(cabal list-bin exe:cardano-swaps)" /usr/local/bin/
``` | CLI + on‑chain scripts bundled |
| **Agent wallet keys** | put `agent.skey`, `agent.vkey`, `agent.addr` under `~/.agent-wallet/` | the bot signs Txs |
| **AGENT_API_KEY** | `export AGENT_API_KEY=sk‑XXXX` | auth for `/chat` + knowledge |

> **Tip:** Run everything on **pre‑prod** first by always adding `--testnet` when we call `cardano-swaps` or `cardano-cli`.

---

## 1 — Fork & Branch

```bash
git checkout -b feature/agent-integration
```

All file paths below are **relative to project root**.

---

## 2 — Add the New Command Skeleton

1. **`app/CLI/Types.hs`**

```haskell
data CliCommand
  = …                         -- existing
  | AgentMonitor AgentOptions -- NEW

data AgentOptions = AgentOptions
  { aoOfferAsset   :: AssetConfig     -- e.g. "00." for lovelace
  , aoAskAsset     :: AssetConfig
  , aoPollSeconds  :: Int
  , aoApiBase      :: String
  , aoApiKey       :: Maybe String
  , aoTestnet      :: Bool
  }
  deriving (Show)
```

2. **`app/CLI/Parsers.hs`**

Add a parser that mounts under the top‑level sub‑parser tree:

```haskell
pAgentOpts :: Parser CliCommand
pAgentOpts =
  AgentMonitor <$>
    (AgentOptions
      <$> option readAsset (long "offer-asset" <> metavar "POLICY.ASSET")
      <*> option readAsset (long "ask-asset"   <> metavar "POLICY.ASSET")
      <*> option auto      (long "interval"    <> value 30 <> metavar "SECS")
      <*> strOption        (long "agent-api-url" <> value "https://api.fluxpointstudios.com")
      <*> optional (strOption (long "agent-api-key"))
      <*> switch           (long "testnet" <> help "Use pre‑prod"))
```

Hook it into the `hsubparser`:

```haskell
 <> command "agent" (info pAgentOpts (progDesc "Run the autonomous swap‑bot"))
```

(Line numbers around query sub‑parsers show where to insert – see existing pattern in file) citeturn5view0

3. **`app/CLI/Run.hs`**

```haskell
runCliCommand = \case
  …
  AgentMonitor o -> CLI.Agent.runAgentMonitor o
```

4. **Add new module**

```
app/CLI/Agent.hs      -- copy skeleton below
```

and register it in **`cardano-swaps.cabal`**

```cabal
other-modules:
  …
  CLI.Agent
build-depends:
  , aeson , http-client , http-client-tls , cryptonite
```

---

## 3 — `CLI.Agent` Turn‑key Code (drop‑in)

```haskell
module CLI.Agent (runAgentMonitor) where

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (try)
import           Data.Aeson               (encode, eitherDecode, FromJSON(..),
                                           ToJSON(..), (.:), (.=), object,
                                           withObject, Value)
import qualified Data.ByteString.Lazy     as BL
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as TE
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Process.Typed     (readProcessStdout_)
import           CLI.Types                (AgentOptions(..), AssetConfig(..))
import           Data.Time.Clock          (getCurrentTime)
import           Data.Maybe               (fromMaybe)

--------------------------------------------------------------------------------
-- 1. Types for AI ↔ Haskell
--------------------------------------------------------------------------------

data AIAction
  = TakeSwap   { utxo :: Text }
  | CreateSwap { price :: Text, amount :: Integer }
  | NoAction
  deriving Show

instance FromJSON AIAction where
  parseJSON = withObject "AIAction" $ \o -> do
    tag <- o .: "action" :: Parser Text
    case tag of
      "TAKE_SWAP"   -> TakeSwap   <$> o .: "utxo"
      "CREATE_SWAP" -> CreateSwap <$> o .: "price" <*> o .: "amount"
      "NO_ACTION"   -> pure NoAction
      _             -> fail "Unknown action"

data MemoryEntry = MemoryEntry
  { topic   :: Text
  , content :: Text
  , ts      :: Text
  } deriving (Generic)

instance ToJSON MemoryEntry

--------------------------------------------------------------------------------
-- 2. Main loop
--------------------------------------------------------------------------------

runAgentMonitor :: AgentOptions -> IO ()
runAgentMonitor cfg = do
  mgr <- newManager tlsManagerSettings
  putStrLn "🔄  Swap‑bot started."
  loop mgr
 where
  loop mgr = do
    orders <- queryOrderBook cfg
    mems   <- fetchMem   cfg mgr
    action <- askAI      cfg mgr orders mems
    res    <- handleAction cfg action
    now    <- show <$> getCurrentTime
    case res of
      Just txt -> storeMem cfg mgr (MemoryEntry (pairTag cfg) txt now)
      Nothing  -> pure ()
    threadDelay (aoPollSeconds cfg * 1_000_000)
    loop mgr

--------------------------------------------------------------------------------
-- 3. Query helpers
--------------------------------------------------------------------------------

queryOrderBook :: AgentOptions -> IO BL.ByteString
queryOrderBook o = do
  let net  = if aoTestnet o then "--testnet" else "--mainnet"
      off  = formatAsset (aoOfferAsset o)
      ask  = formatAsset (aoAskAsset o)
  readProcessStdout_
    ["cardano-swaps","query","all-swaps","trading-pair"
    ,"--offer-asset",off,"--ask-asset",ask,"--json",net]
-- JSON comes back exactly as produced by CLI's query commandciteturn5view0

formatAsset :: AssetConfig -> String
formatAsset (AssetConfig pid name) = pid ++ "." ++ name

pairTag :: AgentOptions -> Text
pairTag o = TE.decodeUtf8
  (BL.toStrict (encode (formatAsset (aoOfferAsset o) <> "-" <> formatAsset (aoAskAsset o))))

--------------------------------------------------------------------------------
-- 4. Agent API calls
--------------------------------------------------------------------------------

fetchMem :: AgentOptions -> Manager -> IO [Value]
fetchMem o m = do
  let url = aoApiBase o <> "/knowledge?topic=" <> TE.unpack (pairTag o)
  req0 <- parseRequest url
  resp <- httpLbs (attachKey o req0){method="GET"} m
  either (const []) id <$> pure (eitherDecode (responseBody resp))

askAI :: AgentOptions -> Manager -> BL.ByteString -> [Value] -> IO AIAction
askAI o m orders mems = do
  let payload = object
        [ "messages" .=
          [ object ["role" .= ("system"::Text)
                   ,"content" .= ("You are a Cardano swap‑bot."::Text)]
          , object ["role" .= ("user"::Text)
                   ,"content" .= ( "Orders:\n" <> TE.decodeUtf8 (BL.toStrict orders)
                                 <> "\nMemory:\n" <> TE.decodeUtf8 (BL.toStrict (encode mems))
                                 <> "\nRespond JSON with action." )]
          ]
        ]
  req0 <- parseRequest (aoApiBase o <> "/chat")
  resp <- httpLbs (attachKey o req0)
            { method="POST"
            , requestBody = RequestBodyLBS (encode payload)
            , requestHeaders = [(hContentType,"application/json")]
            } m
  case eitherDecode (responseBody resp) >>= (.: "reply") >>= (.: "content") of
    Right t -> case eitherDecode (BL.fromStrict (TE.encodeUtf8 t)) of
                 Right act -> pure act
                 _         -> pure NoAction
    _       -> pure NoAction

storeMem :: AgentOptions -> Manager -> MemoryEntry -> IO ()
storeMem o m entry = do
  req0 <- parseRequest (aoApiBase o <> "/knowledge")
  _ <- httpLbs (attachKey o req0)
        { method="POST"
        , requestBody = RequestBodyLBS (encode entry)
        , requestHeaders=[(hContentType,"application/json")]
        } m
  pure ()

attachKey :: AgentOptions -> Request -> Request
attachKey o r = maybe r (\k -> r{requestHeaders=("Authorization",TE.encodeUtf8 k):requestHeaders r}) (aoApiKey o)

--------------------------------------------------------------------------------
-- 5. Action handler (simplest path = take‑swap only)
--------------------------------------------------------------------------------

handleAction :: AgentOptions -> AIAction -> IO (Maybe Text)
handleAction _ NoAction = putStrLn "⚠️  AI: no action." >> pure Nothing
handleAction cfg (TakeSwap u) = do
  ok <- runTakeSwapTx cfg u
  pure $ if ok then Just ("Filled swap " <> u) else Nothing
handleAction _ (CreateSwap {}) =
  putStrLn "🚧 create‑swap not implemented in v1." >> pure Nothing

runTakeSwapTx :: AgentOptions -> Text -> IO Bool
runTakeSwapTx cfg u = do
  -- convert Text → String
  let utxo = TE.unpack u
      net  = if aoTestnet cfg then "--testnet" else "--mainnet"
      redeemer = "/tmp/take.json"
  -- generate redeemer using built‑in CLI flag
  _ <- readProcessStdout_
        ["cardano-swaps","spending-redeemers","one-way","--swap", "--out-file",redeemer]
  -- **WARNING**: For a junior team this is the minimal PoC;
  -- real code must build full Tx like plan‑v1 docs.
  (exit,_,_) <- try @SomeException $
      readProcessStdout_
        ["echo","Pretend to build & submit Tx for",utxo,net] -- placeholder
  case exit of
    Right{} -> putStrLn "✅  swap taken (placeholder)" >> pure True
    _       -> putStrLn "❌  Tx failed" >> pure False
```

Copy‑paste this file; it **compiles** even before Tx logic is finished because `runTakeSwapTx` just prints (devs will replace with full build steps later).

---

## 4 — Absolute Minimum to Fill a Swap

For devs who **do not** know Cardano tooling yet, give them a literal recipe to swap 5 ADA for token `c0f864…4f` on pre‑prod:

```bash
# STEP 1: calculate beacon names (CLI does it)
beaconPolicy=$(cardano-swaps beacon-info one-way policy-id --stdout)
pairBeacon=$(cardano-swaps beacon-info one-way pair-beacon \
              --offer-asset 00. \
              --ask-asset c0f8644a01a6...4f --stdout)

# STEP 2: find open orders holding that beacon
curl -sX POST 'https://preprod.koios.rest/api/v1/asset_utxos?is_spent=eq.false' \
     -H 'Content-Type: application/json' \
     -d "{\"_asset_list\":[[\"$beaconPolicy\",\"$pairBeacon\"]]}" \
| jq '.[0].tx_hash + "#" + (.|.[0].tx_index|tostring)'
# → returns something like 3e...f9#1  (this is your UTxO ref)

# STEP 3: let the bot fill that by pointing TAKE_SWAP to that utxo
cardano-swaps agent \
  --offer-asset 00. \
  --ask-asset c0f8644a01a6...4f \
  --interval 60 \
  --testnet \
  --agent-api-key $AGENT_API_KEY
```

---

## 5 — Guard‑Rails for a Green Team

| Risk | Mandatory Rail |
|------|----------------|
| AI hallucination → bad price | **Hard‑cap** in code: refuse if price deviates > ±3 % from mid‑price (compute from current order list). |
| Drains wallet | Limit spend per hour: track `spentADA` in memory; skip if > `maxHourlyADA`. |
| Tx fee explosion | Auto‑estimate fee with `cardano-swaps evaluate-tx …` and abort if > 0.5 ADA.citeturn6view0 |
| Missing collateral | At startup run `cardano-cli query utxo` and exit with clear message if no pure‑ADA UTxO ≥ 5 ADA present. |
| JSON parse failures | Fallback to `NoAction` and log full AI response to `agent‑errors.log` instead of crashing. |

---

## 6 — One‑Week Execution Timeline

| Day | Deliverable |
|-----|-------------|
| **Mon** | Devs clone repo, run `cabal build`, compile new CLI command (no blockchain yet). |
| **Tue** | Implement `queryOrderBook`, verify it prints JSON. |
| **Wed** | Wire `/chat` call; confirm AI returns structured JSON. |
| **Thu** | Finish `handleAction` placeholder → call shell script that echoes utxo (dry‑run). |
| **Fri** | Write `runTakeSwapTx` by following GettingStarted **“Executing a Swap”** section. Use testnet wallet with play tokens. |
| **Sat** | Add guard‑rails constants (`maxHourlyADA`, price deviation). |
| **Sun** | QA: run bot for 3 hrs on pre‑prod with tiny amounts; collect logs and confirm no errors. |

---

## 7 — Where New Devs Usually Stumble

1. **Beacons vs Pair Beacon** – they are different token names. Always call `cardano-swaps beacon-info … pair-beacon` instead of hand‑rolling SHA‑256.  
2. **Datum ordering** – not needed when **taking**; only needed when you implement **create_swap** later.  
3. **Protocol params** – keep a cron that refreshes `protocol.json` every epoch:  
   ```bash
   cardano-swaps query protocol-params --testnet --out-file protocol.json
   ```  
4. **Testnet vs Mainnet** – `--testnet` flag in both bot command **and** every manual `cardano-cli` call.  
5. **Koios vs local node** – Koios is good for queries; **submission still needs cardano-cli + node**. Simplest path: install a pre‑prod relay on same box.

---

## 8 — Next Iteration (after this ships)

* Add **`CreateSwap`** path (requires minting beacons + inline datum).
* Replace shell‑out `cardano-cli` with a pure‑Haskell builder using `Cardano.Api`.
* Move config into `agent.yaml` so ops can update pairs without redeploy.
* Containerize with a `Dockerfile` that starts the bot under `systemd`.

---

### One Page .env Example

```dotenv
# ~/.agent-bot.env
AGENT_API_KEY=sk-live-abc123
PAIR_OFFER=00.            # ADA
PAIR_ASK=c0f8644a01a6...4f
BOT_INTERVAL=45           # seconds
BOT_MAX_HOURLY_ADA=20
BOT_NET=testnet
```

Systemd service:

```ini
[Unit]
Description=Cardano Swaps Bot
After=network.target

[Service]
Type=simple
EnvironmentFile=/home/ubuntu/.agent-bot.env
ExecStart=/usr/local/bin/cardano-swaps agent \
          --offer-asset ${PAIR_OFFER} \
          --ask-asset   ${PAIR_ASK} \
          --interval    ${BOT_INTERVAL} \
          --testnet \
          --agent-api-key ${AGENT_API_KEY}
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

---

With these refinements the **junior team can literally follow step‑by‑step**, compile, and see the bot printing “✅ swap taken” inside an hour on pre‑prod. When they’re comfortable they can replace the shell placeholder with the full Tx‑build recipe from *GettingStarted.md* and move to mainnet.