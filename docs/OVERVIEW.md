
# Integration Plan: Agent API with Cardano-Swaps

## Overview  
This plan outlines how to integrate the custom **Agent API** (Flux Point Studios) into the open-source **Cardano-Swaps** project. The goal is to create an autonomous “agent” module within Cardano-Swaps that can monitor the DEX’s swap order book, analyze market context using the Agent API’s AI (/chat endpoint), leverage the Agent API’s knowledge graph for memory, and **execute swap transactions** based on the AI’s decisions. We will modify the Cardano-Swaps codebase to add a new **Agent** component and connect it with the Agent API. The following sections break down the required file changes, new code additions, data flow, API call details, and setup steps for the development team.  

**Key Objectives:**  
- Add an **agent monitoring loop** that continually queries open swap orders (via Koios or local node) and triggers the AI analysis.  
- Enable the agent to **decide actions** (e.g., take an open swap or place a new swap) via Agent API’s `/chat` intelligence.  
- Integrate **knowledge graph memory**: fetch relevant past data before decisions and store outcomes after, using Agent API’s knowledge graph endpoints.  
- Ensure integration points (file paths, modules, and functions) are clearly identified for implementation.

## Repository Structure & Integration Points  
In the `fallen-icarus/cardano-swaps` repository, the primary integration will occur in the **CLI application code**, which is found under the `app/` directory and corresponding modules:  

- **`app/Main.hs`** – Entry point for the CLI executable. Likely uses `optparse-applicative` to parse commands and then calls into CLI logic.  
- **`app/CLI.Parsers.hs`** – Defines CLI subcommands and options (the parser combinators). We will add a new **`agent`** command here.  
- **`app/CLI.Types.hs`** – Defines data types for CLI commands/options. We will introduce a new constructor (e.g., `AgentRun`) to represent the agent operation.  
- **`app/CLI.Run.hs`** – Implements the handler for each CLI command. We will add logic to execute our agent loop when the new command is invoked.  
- **`app/CLI.Query.Koios.hs`** (and possibly `CLI.Query.hs`) – Contains functions to query blockchain data (using Koios or local node). We will use these to fetch open swap UTxOs. The Cardano-Swaps CLI already uses Koios for off-chain queries ([GitHub - fallen-icarus/cardano-swaps: A distributed order-book DEX using composable atomic swaps with full delegation control and endogenous liquidity.](https://github.com/fallen-icarus/cardano-swaps#:~:text=have%20the%20swappable%20asset,but%20only)), which we can leverage for the agent’s monitoring.  
- **`src/CardanoSwaps/...`** – Core library code (Plutus scripts, on-chain logic, etc.). We likely do not need to change on-chain logic, but we will reuse existing mechanisms (beacon policy IDs, etc.) from here.

Additionally, we will create a new module: **`app/CLI.Agent.hs`** (or similar) to encapsulate the agent’s functionality (API calls, decision logic, etc.). This module will be referenced in `CLI.Run`. We must also update the cabal file’s `other-modules` to include any new modules (e.g., `CLI.Agent`).

## Adding the Agent Command to the CLI  

**1. Update CLI Types:** In `CLI.Types`, extend the command type (let’s call it `CliCommand`) to include a new variant for the agent. For example: 

```haskell
-- In CLI.Types.hs
data CliCommand 
    = ...                -- existing constructors for other commands
    | AgentMonitor AgentOptions  -- new constructor for agent
```

We also define an `AgentOptions` record to hold any configuration for the agent. For instance: 

```haskell
data AgentOptions = AgentOptions 
  { aoOfferAsset  :: (String, String)   -- Policy ID and asset name of offer asset
  , aoAskAsset    :: (String, String)   -- Policy ID and asset name of ask asset
  , aoPollInterval :: Int              -- Polling interval in seconds
  , aoAgentApiUrl  :: String           -- Base URL for Agent API (e.g., https://api.fluxpointstudios.com)
  , aoAgentApiKey  :: Maybe String     -- API key or token if required (None if not)
  }
```

This `AgentOptions` will capture: the trading pair to monitor (each asset specified by policy ID and name; for ADA we use empty strings as policy and asset since ADA’s policy ID is empty), a polling interval, and configuration for connecting to the Agent API (base URL and possibly an API key). The API key can be optional if the Agent API is open or uses IP allowlist; if required, it can be provided via a config file or environment variable and passed in.  

**2. Extend CLI Parser:** In `CLI.Parsers`, add a new subparser for the `agent` command. For example, using **optparse-applicative** combinators:

```haskell
-- In CLI.Parsers.hs (within the parser for commands)
import Options.Applicative

agentOptsParser :: Parser AgentOptions
agentOptsParser = AgentOptions
    <$> option assetReader 
          ( long "offer-asset" 
            <> metavar "POLICY.ASSET" 
            <> help "Offer asset (policyID.assetName in hex; use \"\" for ADA)" )
    <*> option assetReader 
          ( long "ask-asset" 
            <> metavar "POLICY.ASSET" 
            <> help "Ask asset (policyID.assetName in hex; use \"\" for ADA)" )
    <*> option auto 
          ( long "interval" 
            <> short 'i' 
            <> metavar "SECONDS" 
            <> value 30 
            <> help "Polling interval for monitoring swaps (default 30s)" )
    <*> strOption 
          ( long "agent-api-url" 
            <> metavar "URL" 
            <> value "https://api.fluxpointstudios.com" 
            <> help "Base URL of Agent API (default uses fluxpointstudios endpoint)" )
    <*> optional (strOption 
          ( long "agent-api-key"
            <> metavar "KEY" 
            <> help "Agent API key if required (optional)" ))

-- 'assetReader' would parse "policy.asset" string into a (String,String) pair.
-- e.g., ""."" -> ("","") for ADA, or "c0f8644a... . 4f746865..." for other tokens.
-- We'll implement assetReader as a ReadM that splits on the dot.
```

Then integrate this into the main command parser, for example:

```haskell
cmdParser :: Parser CliCommand
cmdParser = hsubparser
  ( ... -- other command parsers
  <> command "agent" 
       (info (AgentMonitor <$> agentOptsParser) 
             (progDesc "Run the intelligent agent to monitor and trade on Cardano-Swaps")) 
  )
```

This setup means a user (or the system service) can start the agent by running a command like:  
```
cardano-swaps agent --offer-asset 00. --ask-asset c0f8644...4f746865... --interval 60 
```  
(for ADA -> TestDJED, as an example, where ADA is represented by `""` policy and asset, shown here as `00.` because in the one-way beacon formula ADA’s policyID is replaced by “00”).

**3. Handle the Agent Command:** In `CLI.Run.hs`, add logic to handle `AgentMonitor` command. For example:

```haskell
-- In CLI.Run.hs
runCliCommand :: CliCommand -> IO ()
runCliCommand cmd = case cmd of
    ...
    AgentMonitor opts -> runAgentMonitor opts
```

Here, `runAgentMonitor` will be a function we implement (likely in a new module `CLI.Agent`) that contains the main loop and integrates with the Agent API. We will now detail the implementation of this function.

## Implementing the Agent Monitoring Loop (CLI.Agent Module)  
We create `app/CLI/Agent.hs` to encapsulate the agent’s operation. This module will: 

- Query the **Cardano-Swaps order book** for the specified trading pair at regular intervals.  
- Retrieve any relevant memory from the **Agent API’s knowledge graph** before making a decision.  
- Use the **Agent API’s /chat endpoint** to get an AI analysis or recommendation based on current context (order book + memory).  
- Parse the AI’s response to decide on an action (e.g., take a swap offer, place a new offer, or do nothing).  
- If an action is to be taken, execute the corresponding **Cardano-Swaps transaction** (via cardano-cli or built transaction) and then record the outcome.  
- **Persist the outcome or any new knowledge** back to the Agent API’s knowledge graph for future runs.  
- Repeat the loop after sleeping for the specified interval.

We will use Haskell’s HTTP client libraries to call the Agent API (the project already depends on `http-client` and `http-client-tls`, and uses **Servant** for Koios calls, which we can reuse). We may also use concurrency utilities (like `threadDelay` from `Control.Concurrent`) to implement the polling delay.

**Pseudocode for `runAgentMonitor`:**

```haskell
import Network.HTTP.Client (newManager, defaultManagerSettings, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson (encode, eitherDecode, Value, object, (.=))
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime)

runAgentMonitor :: AgentOptions -> IO ()
runAgentMonitor opts = do
    -- Initialize HTTP manager for API calls
    manager <- newManager tlsManagerSettings

    putStrLn "Starting Cardano-Swaps agent monitoring..."
    loop manager
  where
    loop manager = do
      -- 1. Fetch open swaps for the target trading pair
      orders <- queryOpenOrders (aoOfferAsset opts) (aoAskAsset opts)
      -- If orders list is empty or no available swaps, we still proceed (AI might decide to create one)

      -- 2. Retrieve relevant memory from knowledge graph
      memory <- fetchAgentMemory manager

      -- 3. Compose context for AI
      let context = composeContext orders memory
      -- 4. Call Agent API /chat to analyze context
      recommendation <- callAgentChat manager context

      -- 5. Decide action based on AI recommendation
      let action = parseRecommendation recommendation

      -- 6. If action involves a transaction (e.g., TakeSwap or CreateSwap)
      case action of
        TakeSwap utxo -> do
            result <- executeSwapTransaction utxo
            -- 7. Persist outcome in knowledge graph
            storeAgentMemory manager (mkOutcomeEntry orders action result)
        CreateSwap params -> do
            result <- executeNewSwapTransaction params
            storeAgentMemory manager (mkOutcomeEntry orders action result)
        NoAction -> 
            putStrLn "No action taken this cycle."
      -- 8. Wait for the next cycle
      threadDelay (aoPollInterval opts * 1_000_000)  -- convert seconds to microseconds
      loop manager

    -- helper functions: queryOpenOrders, fetchAgentMemory, composeContext, callAgentChat, parseRecommendation, executeSwapTransaction, executeNewSwapTransaction, storeAgentMemory, mkOutcomeEntry...
```

Let’s break down each part in detail, including how they tie into specific files/functions:

### 1. Querying Open Swap Orders (Cardano-Swaps Query)  
We will use the Cardano-Swaps CLI’s capabilities (Koios or local node) to get the current open swaps for the specified pair. The repository already demonstrates how to find UTxOs carrying the swap beacons via Koios. For example, **Koios** can filter UTXOs by asset (beacon) and spent/unspent status ([GitHub - fallen-icarus/cardano-swaps: A distributed order-book DEX using composable atomic swaps with full delegation control and endogenous liquidity.](https://github.com/fallen-icarus/cardano-swaps#:~:text=have%20the%20swappable%20asset,but%20only)). We will implement `queryOpenOrders` to do this. Potential approaches:

- **Using Koios via Servant client:** Cardano-Swaps likely has a Servant client configured for Koios endpoints in `CLI.Query.Koios`. If so, we can call a function (or replicate logic) that queries `asset_utxos` for the beacon asset of our trading pair. We need to compute the **beacon Policy ID and asset name** for the given pair. 
  - *One-way swaps:* The beacon policy ID is fixed (`47cec2a...8fd` for one-way swaps), and asset name is `sha2_256( offerPolicy ++ offerName ++ askPolicy ++ askName )` (with ADA’s policy as `"00"` if applicable). Two-way swaps use a different beacon (two-way beacon script hash, found in the code or docs). We need to derive or look up the beacon name. 
  - We can either implement the hash calculation in Haskell (since we have the inputs from options) or reuse an existing function if provided in Cardano-Swaps library (search for where beacon names are computed). For simplicity, implement it: if `offerPolicyId` or `askPolicyId` is empty (meaning ADA), substitute `"00"`. Then concatenate `offerPolicyId ++ offerAssetName ++ askPolicyId ++ askAssetName` (all as hex strings), SHA-256 hash it (the library likely has Crypto or we can add a dependency on cryptonite for hashing), to get a 32-byte (64 hex chars) token name. That is our beacon asset name for one-way swaps in the **offer->ask direction**. We should do it for both directions (i.e., also ask->offer) if we want to catch all open orders in the pair. The two directions will have different asset names. So `queryOpenOrders` should actually perform **two queries** (one for each direction beacon) and combine the results.  
  - Use Koios endpoint: `POST /api/v1/asset_utxos` with JSON body `{"_asset_list": [ [policy_id, asset_name] ] }`. Koios can also filter by `is_spent=false` in query params (as shown in the docs) ([GitHub - fallen-icarus/cardano-swaps: A distributed order-book DEX using composable atomic swaps with full delegation control and endogenous liquidity.](https://github.com/fallen-icarus/cardano-swaps#:~:text=CLI%20uses%20Koios%20like%20this,but%20only)) to return only unspent UTxOs (open orders). The Cardano-Swaps documentation even provides an example curl with these filters. We might incorporate that filter to reduce post-processing.  

- **Alternative (Local node):** If a local cardano node is available, we could use `cardano-cli query utxo` on the beacon address or similar. However, this would require deriving the **script address** that holds swaps for that pair (which involves the on-chain script and the stake credential used). Since Cardano-Swaps encourages using Koios for convenience, we will proceed with Koios for now (to avoid the complexity of local ledger state). This keeps things simpler and matches the design (the CLI “uses Koios in all scenarios where a node is required” ([cardano-swaps/GettingStarted.md at main - GitHub](https://github.com/fallen-icarus/cardano-swaps/blob/main/GettingStarted.md#:~:text=Sign%20the%20transaction%20and%20submit,Querying%20protocol%20parameters))).

**Implementation in Haskell:** We can construct the HTTP request to Koios. If using raw http-client: 

```haskell
queryOpenOrders :: (String,String) -> (String,String) -> IO [SwapUTxO]
queryOpenOrders (offerPol, offerName) (askPol, askName) = do
    -- compute beacon asset names for both directions
    let beaconName1 = sha256Hex (normalizePid offerPol <> normalizeName offerName 
                               <> normalizePid askPol <> normalizeName askName)
        beaconName2 = sha256Hex (normalizePid askPol <> normalizeName askName 
                               <> normalizePid offerPol <> normalizeName offerName)
        beaconPolicy = "47cec2a1404ed91fc31124f29db15dc1aae77e0617868bcef351b8fd"  -- one-way beacon policy (from docs)
    -- prepare Koios request
    let assetJSON1 = object [ "policy_id" .= beaconPolicy, "asset_name" .= beaconName1 ]
        assetJSON2 = object [ "policy_id" .= beaconPolicy, "asset_name" .= beaconName2 ]
        reqBody = object [ "_asset_list" .= ([assetJSON1] :: [Value]) ]
        -- Note: We send two separate requests because Koios asset_utxos expects one asset at a time in _asset_list.
    resp1 <- httpPost "https://<network>.koios.rest/api/v1/asset_utxos?is_spent=eq.false" reqBody
    resp2 <- httpPost "https://<network>.koios.rest/api/v1/asset_utxos?is_spent=eq.false" 
                      (object [ "_asset_list" .= ([assetJSON2] :: [Value]) ])
    -- Parse JSON responses to extract UTxO list
    let utxos1 = decodeSwapUtxos resp1
        utxos2 = decodeSwapUtxos resp2
    return (utxos1 ++ utxos2)
```

Where `normalizePid` function returns `"00"` if input is empty (for ADA policy), otherwise the hex string as is; `normalizeName` returns the hex string (for ADA it would be empty string since ADA has no asset name, but our SHA input expects empty anyway). `sha256Hex` computes the SHA-256 and returns a hex string (we might use `cryptonite` package’s hashing or if Cardano-Swaps already has an util, but adding `cryptonite` to .cabal might be needed for hashing here). `httpPost` is a helper to do an HTTP POST given a URL and Aeson Value (we use `encode` to ByteString and `httpLbs`). `decodeSwapUtxos` will parse the response JSON into our `SwapUTxO` type (we can define a simple type with fields like utxo hash, index, assets, etc., or just parse minimally what we need, e.g., reference ID and maybe contained assets if needed for decision).

**Note:** We should ensure we use the correct Koios network (Mainnet vs Preprod). Possibly allow the base Koios URL or network to be configurable (for development it might be preprod as in examples). This can be an additional option or derived from whether the assets provided look like testnet tokens vs mainnet. For simplicity, could default to **mainnet Koios** in production.

**Using Cardano-Swaps library internals:** If time permits, check if Cardano-Swaps has internal helpers to get beacon information. For example, there might be a function to derive beacon names or even directly query via Koios (since `CLI.Query.Koios` exists). If such function exists (e.g., `getOneWaySwapUtxos offer ask`), use that directly instead of custom coding. The integration plan should mention we will search the code for any existing query utilities. Given the CLI uses Koios with Servant, perhaps they have a Servant API type for `asset_utxos`. We might integrate with that for more type-safe calls. However, implementing via raw HTTP as above is fine if no direct helper is present.

**Two-Way Swaps:** The above was for one-way swap offers. **Two-way swaps** use a slightly different mechanism: they have a separate beacon (the *pair beacon*) and possibly multiple beacons per UTxO (asset1Beacon, asset2Beacon). If the DEX uses two-way swaps (liquidity pools), we might also need to consider those. Two-way swap beacons might have a different policy (likely the script hash of the *two-way beacon policy*). For completeness, we should also query two-way swaps: typically, two-way swaps for a pair will have a “pair beacon” token unique to that pair. The Cardano-Swaps on-chain datum includes `beaconId` and `pairBeacon`. The pairBeacon token (with a specific policy) likely identifies all swaps of that pair (across both directions combined). If we can get that pair beacon policy and name, we could query Koios for all UTxOs containing that pair beacon (spent=false). That would yield all two-way swap offers for that pair. The knowledge of pair beacon derivation might be in the code or documentation. Possibly it is minted via a separate “beacon script” for two-way swaps. For now, if initial scope is one-way swaps (order-book style), we can proceed with one-way. But to future-proof, mention that extending to two-way is similar: just query the pair beacon token’s UTxOs. The **AgentOptions** could have a flag whether to include two-way swaps. For this plan, assume initial focus on one-way (limit-order style) swaps for simplicity.

### 2. Retrieving Memory from Knowledge Graph  
Before asking the AI for advice, the agent should gather **relevant past knowledge** from the Agent API’s knowledge graph. This allows it to recall previous experiences (e.g. last price it saw, last action it took, outcomes of prior trades, patterns identified, etc.) and provide them as additional context to the AI, improving decision-making over time.

We will use the Agent API’s **knowledge graph endpoints** for this. According to the user’s requirements, there are endpoints to **persist** and **retrieve** memory. While the exact endpoint names aren’t explicitly given, we will assume they are documented in the Agent API docs. Likely candidates: e.g., `GET /knowledge` to query memory, and `POST /knowledge` to add new memory nodes. It could also be structured with more specific paths (for example, some systems use `/memory/search` and `/memory/upsert`, or have the agent ID in the path). We will call them abstractly as needed:

- **`fetchAgentMemory`:** This function will call the knowledge graph API to retrieve memories relevant to the current context. We might query by a specific key or category. For example, we could tag all our agent’s entries in the knowledge graph with the trading pair (e.g., “ADA-TESTDJED”) or a unique agent identifier. Then `fetchAgentMemory` can ask for all memories for that tag or do a semantic search. If the API offers semantic search, we could send the current order book state or a summary as a query and get back memory entries. If a simpler key-based retrieval, we retrieve all or the last N memories. For now, assume we can fetch a list of recent memory **entries** as JSON objects. 

Pseudo-implementation:

```haskell
fetchAgentMemory :: Manager -> IO [MemoryEntry]
fetchAgentMemory manager = do
    let url = aoAgentApiUrl opts ++ "/knowledge?topic=" ++ topicKey
    req <- parseRequest url  -- GET request by default
    applyApiKey req (aoAgentApiKey opts) >>= httpLbs manager >>= \resp ->
        case eitherDecode (responseBody resp) of
            Right entries -> return entries
            Left err      -> do
                putStrLn $ "Memory fetch decode error: " ++ err
                return []
  where
    topicKey = (fst (aoOfferAsset opts)) <> "-" <> (fst (aoAskAsset opts))
    -- Example: use a topic combining policy IDs of the pair as a crude key
    -- Alternatively, a constant agent ID or name can be used if the API requires it.
```

Here `applyApiKey` would add an Authorization header if an API key is present. The response is expected to be JSON that can parse into `[MemoryEntry]`, where `MemoryEntry` is a type we define to match whatever structure the knowledge graph returns (maybe something like `{ "id": ..., "content": "...", "timestamp": ..., ... }`).

If the Agent API requires a POST for search (some graph systems use POST with a query in body), adjust accordingly. But likely a GET with query param or a specific route exists.

**Important:** We should clarify what “memory” to retrieve. Perhaps the agent wants to remember **past transactions it did** or **past analyses**. For example, memory could include: “On 2025-04-10, saw price X, took swap, gained Y” or “Last trade outcome was loss due to slippage”, etc. These could be stored previously. For initial integration, we might just fetch everything and let the AI filter relevant info.

### 3. Composing Context for AI  
Once we have the current open orders and the retrieved memory entries, we need to compose a **prompt** or context to send to the Agent’s AI via the `/chat` endpoint. We should structure this context clearly so the AI can understand it. Typically, the Agent API’s /chat likely expects a JSON payload with chat messages or a prompt. We will formulate a prompt that includes:

- A concise summary of the **current order book state** for the pair (e.g., best offer price, number of orders, etc., or even the full list of open orders if not too large).
- Relevant **memory snippets** retrieved (for example, “Previous outcome: sold too low last time”, or key numbers from past trades).
- Possibly a direct question or instruction for the AI, such as: *“Given the above market state and past experience, what is the best action now? Options: take an existing swap, create a new swap at a better price, or do nothing.”*

We can include a **system or user message** guiding the AI to output a structured recommendation (so parsing is easier). For example, instruct the AI: *“Respond with an action in JSON format. Use one of: TAKE_SWAP, CREATE_SWAP, or NO_ACTION. If TAKE_SWAP or CREATE_SWAP, include details.”*

For the API call, we create a JSON payload. The exact format depends on the Agent API spec; let’s assume it’s similar to OpenAI or Anthropic chat formats. We might do:

```haskell
composeContext :: [SwapUTxO] -> [MemoryEntry] -> ChatPrompt
composeContext orders memory = 
    let orderSummary = summarizeOrders orders
        memorySummary = summarizeMemory memory
        systemMsg = "You are an autonomous trading agent for a Cardano DEX. You have memory and current market data."
        userMsg = "Market State:\n" ++ orderSummary ++ "\nRelevant Memory:\n" ++ memorySummary 
                  ++ "\n\nInstruction: Based on the above, decide an action: TAKE_SWAP (and specify which order), CREATE_SWAP (and specify price/quantity), or NO_ACTION. Respond in JSON."
    in ChatPrompt [ Message "system" systemMsg
                  , Message "user" userMsg ]
```

Here `ChatPrompt` could be a type with a list of `Message` (role + content). Or simply we convert this to Aeson Value for the API call: 

```haskell
payload = object [ "messages" .= 
    [ object ["role" .= "system", "content" .= systemMsg]
    , object ["role" .= "user", "content" .= userMsg] 
    ]]
```

If the Agent API expects a different format (e.g., a single prompt string), we adjust accordingly. The key is that **we supply context (orders + memory) and ask for a recommendation**.

The `summarizeOrders` function should convert the list of open swap UTxOs into a human-readable summary. Perhaps: list top 5 offers sorted by price, or count of orders at each price. E.g., “There are 3 offers to sell ADA for TestDJED: best price 1 ADA = 3 DUST, next 1=3.1, ...; There are 2 offers to buy ADA with TestDJED: best price 1 ADA = 2.9 DUST, ...” etc. We want to show if there’s an arbitrage (if best ask < best bid, a cross exists). The memory summary might include last trade price or P&L.

Keep these summaries brief to avoid hitting context length issues, but informative enough for the AI. This is an area to iterate on during development/testing.

### 4. Calling the Agent API /chat Endpoint  
Using the composed prompt, we call the Agent API’s **`/chat`** endpoint. Implementation (`callAgentChat`):

```haskell
callAgentChat :: Manager -> ChatPrompt -> IO ChatResponse
callAgentChat manager prompt = do
    let url = aoAgentApiUrl opts ++ "/chat"
        body = encode prompt   -- prompt converted to JSON bytes
    initialReq <- parseRequest url
    let req = initialReq { method = "POST"
                         , requestBody = RequestBodyLBS body
                         , requestHeaders = [("Content-Type", "application/json")] }
    reqAuth <- applyApiKey req (aoAgentApiKey opts)
    resp <- httpLbs reqAuth manager
    -- Parse the JSON response to get the AI's answer
    case eitherDecode (responseBody resp) of
      Right chatResp -> return chatResp
      Left err -> error $ "Failed to parse /chat response: " ++ err
```

We expect `ChatResponse` to contain the model’s output. Often, this could look like: `{"content": "..."} ` or `{"messages": [..., {"role":"assistant","content":"..."}]}`. We parse out the assistant’s content (the answer). If the Agent API returns the final answer directly, `ChatResponse` could be a simple wrapper with a field for the output text. We adjust parsing accordingly based on the Agent API docs (to be verified during implementation). 

**Example payload (for illustration):**

```json
POST /chat  HTTP/1.1
Content-Type: application/json

{
  "messages": [
    { "role": "system", "content": "You are an autonomous trading agent for a Cardano DEX..." },
    { "role": "user", "content": "Market State:\n- Best offer ...\nMemory:\n- Last trade ...\nInstruction: decide action and respond in JSON." }
  ]
}
```

**Example response:** (the AI might return something like)

```json
{
  "reply": {
    "role": "assistant",
    "content": "{\"action\": \"TAKE_SWAP\", \"utxo\": \"abc123...#1\", \"reason\": \"Arbitrage opportunity detected\"}"
  }
}
```

Or simply:

```json
{
  "content": "{\"action\": \"TAKE_SWAP\", \"utxo\": \"abc123...#1\", \"reason\": \"Arbitrage opportunity detected\"}"
}
```

The exact format will depend on API; but here we assume the assistant’s content is a JSON string we asked for. We will parse that next.

### 5. Parsing the AI Recommendation  
Once we receive the chat response, we need to interpret it to determine the agent’s action. We design the prompt such that the response is in a structured format (JSON embedded in text). We can then do:

```haskell
parseRecommendation :: ChatResponse -> AgentAction
parseRecommendation chatResp =
    case chatResp.content of  -- assuming ChatResponse has 'content :: Text'
      Just text -> 
          case Data.Aeson.decode (BL.fromStrict (encodeUtf8 text)) of
            Just rec -> rec  -- rec would be AgentAction type if JSON matches
            Nothing  -> -- If not JSON, fall back to simple parsing
                if "TAKE_SWAP" `isInfixOf` text
                   then let utxoId = ... in AgentAction TakeSwap utxoId
                else if "CREATE_SWAP" `isInfixOf` text
                   then let params = ... in AgentAction CreateSwap params
                else AgentAction NoAction
      Nothing -> AgentAction NoAction
```

We define an `AgentAction` data type, for example:

```haskell
data AgentAction = TakeSwap TxOutRef | CreateSwap SwapParams | NoAction
data SwapParams = { assetOffered :: ..., amount :: ..., price :: ... }  -- details for creating a new swap offer
```

We try to decode the assistant’s content as JSON into our `AgentAction`. Ideally, we instruct the AI to use keys exactly matching our Haskell structure (e.g., `"action": "TAKE_SWAP", "utxo": "...", ...`). If decode fails, we do a fallback string search (to avoid the agent hanging if format is slightly off). 

During testing, we can refine the prompt to ensure consistent structured output. The dev team can also implement a more robust parser if needed (e.g., using regex or natural language processing if the output isn’t structured, but structured output is preferred).

### 6. Executing the Swap Transaction  
If the decision is **NoAction**, nothing more is done this cycle besides maybe printing a log. If it is **TakeSwap** or **CreateSwap**, the agent needs to construct and submit a transaction to the Cardano network. This is the most technical step, as it involves using the Cardano-Swaps on-chain scripts correctly.

**For TakeSwap (taking an existing order):**  
This means we will consume someone’s open swap UTxO and provide the requested assets in return. Cardano-Swaps defines specific redeemers for taking swaps (`TakeAsset1` or `TakeAsset2` for one-way swaps, depending on which side we take) ([GitHub - fallen-icarus/cardano-swaps: A distributed order-book DEX using composable atomic swaps with full delegation control and endogenous liquidity.](https://github.com/fallen-icarus/cardano-swaps#:~:text=,the%20swap%20conditions%20are%20met)). Also, since swaps use **Plutus scripts**, the transaction must include the script, correct datum, and the appropriate redeemer. Fortunately, the Cardano-Swaps CLI is “batteries included” with all needed scripts and information compiled in. We can leverage that: 

- The Plutus **spending script** for one-way swaps is likely embedded (e.g., `oneWaySwap.plutus` or in Haskell, an `Validator` or serialized script in the library). The **beacon policy script** for one-way is also embedded (for minting/burning the beacon tokens). For taking a one-way swap, we typically **burn the beacon token** from the UTxO, using the `TakeAssetX` redeemer on the spending script, and the `Burn` redeemer on the beacon policy (the CLI might have already minted the beacon at creation, and requires burning on take). The CLI likely provides a convenience to get the correct redeemer values.

- The agent needs to construct the transaction with:  
  - Input: the UTxO of the swap to take (from the orders list, identified by Tx hash and index).  
  - Input: an input from the agent’s own wallet providing the asset that the swap demander is asking for (plus any additional ADA to cover fees as needed). The agent’s wallet UTxO to use should be chosen – perhaps the agent can have a configured **payment address** which it monitors for funds. We should add to **setup** that the agent’s Cardano payment signing key and address must be provided (so it has authority to spend its funds).  
  - Output: the swap UTxO’s address gets some outputs: one output returning the asset that was offered by the swap (minus what the agent took), if not fully consumed, and any leftover beacon if not all taken (but in one-way swap, usually the beacon is fully consumed when taken, closing the order). The agent (taker) gets the portion of asset it wanted. If the agent fully satisfies the order, then the entire UTxO is consumed and no output remains at the swap address (so the order is closed). If partial (the protocol might allow partial fill?), then some output remains with leftover amount and the beacon remains (for partial fill, Cardano-Swaps might not allow partially taking one-way swaps; likely you either take all or none – because one-way was like a limit order for the full amount. Actually, the docs imply you can swap a portion as long as the ratio is met ([GitHub - fallen-icarus/cardano-swaps: A distributed order-book DEX using composable atomic swaps with full delegation control and endogenous liquidity.](https://github.com/fallen-icarus/cardano-swaps#:~:text=Since%20all%20prices%20are%20askedAsset%2FofferedAsset%2C,asset1Price)) – that suggests partial fills *are* allowed by returning change in the same address with beacon. That complicates the logic, but we can attempt only full fills for simplicity first).  
  - Mint/Burn: burn the beacon token (or if partial, maybe not burn? But I think if partial, beacon stays? Actually, if partial, they might use a “prevInput” in datum linking to previous order – in any case, for an automated agent it’s easier to take entire order).  
  - Required signers: the agent’s payment key (for spending its own input and to authorize transaction). The script validations will cover the swap conditions.  

Cardano-Swaps CLI might have subcommands to **build the transaction** or at least to generate parts:
  - For example, there might be a command to get the **spending redeemer** JSON for `TakeAsset1` or `TakeAsset2` (`cardano-swaps spending-redeemers one-way --take asset1` etc.), and to get the **beacon policy redeemer** for burn. In GettingStarted.md, they show usage of `cardano-swaps beacon-redeemers one-way --mint-or-burn` and `cardano-swaps spending-redeemers one-way --close`. By analogy, maybe `--take` exists. If not, they might treat “take” as just closing from the taker’s perspective. Or perhaps the taker just uses `--close` as well (closing the swap means take it if you are not the creator). We’d verify that in documentation: *Anyone can use the TakeAsset1 or TakeAsset2 redeemers as long as conditions are met* ([GitHub - fallen-icarus/cardano-swaps: A distributed order-book DEX using composable atomic swaps with full delegation control and endogenous liquidity.](https://github.com/fallen-icarus/cardano-swaps#:~:text=,the%20swap%20conditions%20are%20met)), which suggests these are indeed the redeemer constructors for the spending script. Possibly `spending-redeemers one-way --take1` etc. The development team should check `CLI/Parsers.hs` to see what options `spending-redeemers` supports. For our plan, assume we can programmatically get the correct redeemer. If not, we can hardcode the needed value (since redeemer might just be a constructor with no additional data, e.g., an enum indicating which side to take).

**Building transaction approach:**  
We have two choices: 
  1. **Use `cardano-cli` externally:** The agent Haskell code can call out to the `cardano-cli` binary via `System.Process` with the appropriate command-line parameters. This is simpler to implement initially (less Haskell code for building TX). We can use the CLI’s generated files or on-the-fly CLI calls: 
     - Use `cardano-swaps spending-redeemers` to output a file for the spending redeemer (or just get JSON string output).
     - Use `cardano-swaps beacon-redeemers` to get beacon policy redeemer (if needed).
     - Use `cardano-swaps datum` command if exists (for partial fills, maybe not needed if closing).
     - Then assemble a cardano-cli transaction build command with: 
       `cardano-cli transaction build \
         --tx-in <swap UTxO> \
         --tx-in <agent UTxO with offering asset> \
         --tx-in-collateral <agent collateral UTxO (for script failure)> \
         --tx-out "<agent address> <amount of asset from swap + leftover ADA>" \
         --tx-out "<swap address> <if any change back to swap>" \
         --mint "-1 <beaconPolicy>.<beaconName>" (to burn beacon) \
         --mint-script-file beacon.plutus \
         --mint-redeemer-file beaconRedeemer.json \
         --tx-in-script-file oneWaySwap.plutus \
         --tx-in-datum-inline <= we have inline datum in UTxO> \
         --tx-in-redeemer-file takeRedeemer.json \
         --required-signer-hash <agent payment pubkey hash> (if needed for reference input or something)
         --change-address <agent address> \
         --out-file swapTx.body \
         --alonzo-era` (or appropriate era)  

       Then sign it: `cardano-cli transaction sign --tx-body-file swapTx.body --signing-key-file agent.skey --out-file swapTx.signed`  
       Then submit: `cardano-cli transaction submit --tx-file swapTx.signed --mainnet`.  

     Many of these details (script files, redeemer JSON) can be obtained from the cardano-swaps compiled artifacts. For example, `oneWaySwap.plutus` might reside in the repository’s `aiken` directory or embedded. Possibly the CLI exposes them via commands. We might embed or copy these script files into the installation. (Cardano-Swaps might embed scripts using `file-embed` as indicated, meaning we could output them at runtime if needed by writing to a temp file, or use `cardano-api` to apply them directly).  

     **Pro:** Using `cardano-cli` ensures we leverage a tested method to build and submit the tx.  
     **Con:** It introduces an external dependency at runtime and requires the environment to have cardano-cli installed and configured (node socket or Koios integration for submit via some relay). Koios cannot submit transactions, so to submit we need either cardano-cli with a node connection or an API like Blockfrost for submission. For simplicity, if we assume the agent runs where `cardano-cli` can connect to a local node or has other submission method, that’s fine.

  2. **Use Haskell libraries to build and submit the TX:** If we wanted a pure Haskell approach, we could bring in `cardano-api` or use `cardano-node-emulator` for building. But that’s complex to implement from scratch in this plan, and likely beyond scope (and would add heavy dependencies or complexity). Given time constraints, using cardano-cli externally (option 1) is acceptable.  

**Our plan:** Initially implement using **system calls to cardano-cli**. We will create helper functions `executeSwapTransaction` and `executeNewSwapTransaction` that either directly call CLI subcommands or, at least, output instructions for the dev team to manually follow. But since the requirement is autonomous, we aim to automate it. We must ensure the agent has access to necessary keys and that these calls are secure (private keys should not be exposed beyond the environment variables or files).

Example for `executeSwapTransaction utxo` (TakeSwap):

```haskell
executeSwapTransaction :: TxOutRef -> IO Bool
executeSwapTransaction utxoToTake = do
    let utxoStr = formatUtxo utxoToTake  -- e.g., "abc123...#1"
        offerPolicy = fst (aoOfferAsset opts)
        askPolicy   = fst (aoAskAsset opts)
        swapScriptFile = "<path>/oneWaySwap.plutus"   -- ensure we have the script
        beaconScriptFile = "<path>/oneWayBeacons.plutus"
        takeRedeemerFile = "/tmp/takeRedeemer.json"
        beaconRedeemerFile = "/tmp/beaconRedeemer.json"
        agentAddr = agentAddress   -- from configuration (bech32)
        agentSkey = "agent.skey"   -- path to signing key
    -- Write redeemer JSON files using CLI subcommands or manually:
    callProcess "cardano-swaps" ["spending-redeemers", "one-way", "--take", determineTakeSide offerPolicy askPolicy, "--out-file", takeRedeemerFile]
    callProcess "cardano-swaps" ["beacon-redeemers", "one-way", "--mint-or-burn", "--out-file", beaconRedeemerFile]
    -- We need an input from agent with askAsset (if taking asset1, agent provides asset2, etc.). Assume agentAddr UTxOs known or single UTXO with enough funds:
    agentUtxo <- pickAgentUtxo (if takingAsset1 then askAsset else offerAsset)
    collateralUtxo <- pickCollateralUtxo 
    -- Build the tx
    let buildArgs = 
          [ "transaction","build"
          , "--tx-in", utxoStr
          , "--tx-in", agentUtxo
          , "--tx-in-collateral", collateralUtxo
          , "--tx-out", constructOutToAgent  -- agent receives the swapped asset
          , "--change-address", agentAddr    -- any excess ADA back to agent
          , "--mint", "-1 " ++ beaconPolicyId ++ "." ++ beaconNameForTakenUTxO
          , "--mint-script-file", beaconScriptFile
          , "--mint-redeemer-file", beaconRedeemerFile
          , "--tx-in-script-file", swapScriptFile
          , "--tx-in-inline-datum-present"  -- because datum is inline in the swap UTxO
          , "--tx-in-redeemer-file", takeRedeemerFile
          , "--protocol-params-file", "protocol.json"
          , "--out-file", "/tmp/agentSwap.body"
          ] ++ networkArgs
    callProcess "cardano-cli" buildArgs
    -- Sign and submit
    callProcess "cardano-cli" ["transaction","sign","--tx-body-file","/tmp/agentSwap.body","--signing-key-file", agentSkey, "--out-file","/tmp/agentSwap.signed"] 
    let submitArgs = ["transaction","submit","--tx-file","/tmp/agentSwap.signed"] ++ networkArgs
    (code, _out, err) <- readProcessWithExitCode "cardano-cli" submitArgs ""
    if code == ExitSuccess 
      then putStrLn "Swap taken successfully." >> return True
      else putStrLn ("Transaction failed: " ++ err) >> return False
```

The above is illustrative. In practice, we need to fill in:  
- `determineTakeSide` to decide if we use `TakeAsset1` or `TakeAsset2` redeemer. This depends on whether the UTxO we’re taking is offering asset1 or asset2. If the agent’s perspective is it wants asset1 from the swap, then it will use the corresponding redeemer. We can determine this by checking if the **offer asset** of the UTxO matches what agent is interested in or something. In a one-way swap, the UTxO’s value will contain the offered asset and the beacon. The “offer” vs “ask” roles might be determined by how we searched: if we got this UTxO from the “ADA->TestDJED” beacon query, it means ADA was offered and TestDJED asked (just as example). We might carry that info along with the UTxO in a structure (so `SwapUTxO` could include fields like offered asset, asked asset, prices, etc., maybe by reading the inline datum which has those details). Indeed, the inline datum of the UTxO can tell us `asset1Price`, `asset2Price` etc.. But reading Plutus datum in Haskell might require parsing the CBOR. Cardano-Swaps CLI might have a command to decode datum too, or since it’s all known Haskell type, maybe easier: If the CLI outputs the datum in JSON when querying Koios (the Koios query we do might not include inline datum, unless Koios returns the datum if requested. Actually the Koios asset_utxos might not return datum by default; it returned `is_spent` and `asset_list` in the example ([GitHub - fallen-icarus/cardano-swaps: A distributed order-book DEX using composable atomic swaps with full delegation control and endogenous liquidity.](https://github.com/fallen-icarus/cardano-swaps#:~:text=curl%20,d%20%27%7B%22_asset_list%22%3A%5B%20%5B%225c84e7433fdd13ca161bd)). We might need another step to fetch datum via Koios if needed. Alternatively, could just decide based on which asset the agent has: e.g., if the swap offers ADA for TestDJED, the agent needs to provide TestDJED to take it (so that means the agent needs to have TestDJED – which implies the agent is *buying* ADA with TestDJED). The AI’s decision likely inherently implies which side to take. If it said “TAKE_SWAP utxo X”, we should also know if it’s taking asset1 or asset2; perhaps instruct the AI to provide that, or we can infer by checking the UTxO’s assets vs our available asset. For simplicity, we can restrict to cases where the agent always takes the *first asset* being offered if profitable. Implementation detail: maybe always use `TakeAsset1` for one direction and `TakeAsset2` for the reverse, depending on how we set up the beacon query (e.g., if we label one query as asset1->asset2, treat those UTxOs with redeemer TakeAsset1, and the other query’s results with TakeAsset2). This should be validated with the actual definition: likely `TakeAsset1` means the taker is taking asset1 out of the swap (so providing asset2), and `TakeAsset2` vice versa. We can confirm from the code or simply test during dev.)

- `pickAgentUtxo` and `pickCollateralUtxo`: functions to choose UTxOs from the agent’s wallet. We might maintain a simple state or call `cardano-cli query utxo --address agentAddr` to list UTxOs then pick one that has the needed token for swap and one ADA-only UTxO for collateral (Plutus requires a collateral input of ADA ~5 ADA usually). Collateral UTxO must be a pure ADA UTxO belonging to agent’s address. We should instruct the devops that the agent’s wallet must have at least one collateral output. This can be set up beforehand.

- `protocol.json`: The agent will need current protocol parameters (especially for `transaction build`). We can obtain this by running `cardano-cli query protocol-parameters --mainnet` periodically. In fact, Cardano-Swaps CLI has a command to fetch protocol params via Koios as well. Possibly it even stores a pre-fetched copy. For reliability, we can have a step to retrieve them at startup (maybe the CLI already does if needed). We’ll include in setup that `protocol.json` must be provided or fetched. (Alternatively, we can omit `--protocol-params-file` if using `--alonzo-era` and let `cardano-cli` fetch from node, but if using Koios (no node), we need to supply it – Koios cannot provide protocol params easily outside another call.)

- `networkArgs`: either `--mainnet` or `--testnet-magic <id>`, depending on network. This could be an option or derived from context.

- `beaconPolicyId` & `beaconNameForTakenUTxO`: the beacon policy ID is known (one-way beacon script hash as given above). The beacon token name we are burning must match exactly the UTxO’s beacon. If we derived the beacon name ourselves earlier, we have it. If not, Koios output likely included the asset name of the beacon in that UTxO’s asset list. We should keep that from `SwapUTxO`. So design `SwapUTxO` type to hold `beaconToken :: (PolicyId, AssetName)` along with maybe the other asset present. That way when forming the burn, we know the name. If partial fill, perhaps we would only burn if fully closed; but to keep it simple, assume full take and burn.

Similar logic goes for **CreateSwap** (placing a new order):  
- The agent decides to place a new swap (maybe if it sees none favorable, it might become a maker by placing an order). Then it must create a UTxO at the swap script address with the appropriate datum and mint the beacon token.  
- This involves a transaction that **mints** a new beacon token (policy = one-way beacon policy) and sends it along with the offered asset to a newly created output locked by the swap script with an inline datum describing the order. The CLI steps to do this (from GettingStarted) are: create the necessary beacons, create the swap datum, and then a transaction to mint beacon and output the assets.  
- We would use `cardano-swaps beacon-redeemers one-way --mint-or-burn` (with `--mint` context) for the beacon policy redeemer, and `cardano-swaps spending-redeemers one-way --close` for the spending script? Actually, for *creating*, the redeemer on spending script is not used (we are not spending an existing swap, we are creating new, so the spending script will just guard the output but not run now). Instead, the **minting policy** (beacon script) is run with redeemer that likely indicates create. Possibly the beacon policy checks that the output goes to correct script address. It might require a specific redeemer, but maybe `--mint-or-burn` covers both mint and burn depending on context. The CLI likely uses the same redeemer type for both create and close (just a flag in redeemer data). We'll assume `--mint-or-burn` is the correct one.  
- We need to provide the **swap datum** for the output. The CLI might have a command to generate the swap datum given all parameters (prices, etc.). If not, we can construct it in Haskell: the datum is of type `SwapDatum` as in code. It includes the pair beacon token name, asset ids/names sorted, the price ratios, and maybe `prevInput` (which is Maybe TxOutRef, used possibly to link swaps in case of partial fill or chaining, but for a fresh order, prevInput = Nothing). We could either: 
  - Simplify and only allow full new orders (prevInput = Nothing). 
  - We may use Aiken or serialization to produce the CBOR of the datum. Possibly easier: if Cardano-Swaps CLI has built-in support, call it. The GettingStarted doesn’t show a direct CLI command for “datum”, but they compute pieces and likely rely on file-embeded script having a default datum? Actually, likely they rely on inline datum constructed offline. They gave a formula to compute the beacon name and said "Just need to construct the UTxO with that datum." Possibly they intended users to craft datum via a Haskell snippet or a separate tool. 
  - Since our agent is integrated in Haskell, we can create the datum via the Cardano API or cardano-swaps library. The `cardano-swaps` library (CardanoSwaps module) might have a Datum constructor that we can use and then serialize. If not, a quick route: if the CLI currently just instructs how to do it manually, we might embed our own solution. 
  - The simplest hack: we can reuse the *emulator* or test functions if any from `cardano-node-emulator` or library to create a Tx. But that’s heavy. Possibly better to again use `cardano-cli`: by providing `--tx-out-datum-embed-file` with a datum file. But we need the datum file. We can produce JSON for the datum (the Cardano CLI can accept JSON with a schema if the datum type is known). If cardano-swaps provided a JSON schema for the datum, we could craft it. Unclear if easier than doing ourselves in Haskell.

Given complexity, focus on enabling at least **taking swaps** first, which covers arbitrage use-case. Creating new swaps can be added similarly by following Cardano-Swaps steps, but if needed in this plan, we outline similarly:
- Use `cardano-swaps spending-redeemers one-way --open` (if exists) or just know that spending redeemer isn’t needed because not spending script now.
- Use `cardano-swaps beacon-redeemers one-way --mint-or-burn` (to mint beacon).
- Prepare a datum JSON. Possibly Cardano-Swaps *might* have a CLI to output a sample datum? If not, we manually build it as per spec:
  - Use the same beacon (policy and name) we compute,
  - asset1/asset2 sorted (where asset1 < asset2 in lexical, presumably),
  - assign asset1Price, asset2Price in the datum from the desired price ratio (the AI would have given us a price suggestion if it wants to place an order),
  - prevInput = Nothing (since new).
  - We then need to encode this datum in a format cardano-cli understands. Possibly use **`--tx-out-datum-embed-value`** where we provide a **script data value** in JSON with constructors? But since SwapDatum is a custom data type, we may need the corresponding Plutus Data form. Alternatively, use `--tx-out-datum-hash` with a provided datum hash, but since this script likely needs inline datum (the code says all UTxOs use inline datums, meaning the spending script probably expects an inline datum present, not just hash). So we should embed it.

We can consider writing the datum as CBOR manually or using Aiken. Actually, note: the repository has an **`aiken/`** directory, which likely contains the Plutus contracts in Aiken. Aiken might also allow constructing data easily. However, doing that in integration might be overkill.

Due to time, we might say: for initial development, the agent might limit to **taking existing swaps**, which is already a full cycle. Creating new swaps (liquidity provision) can be a next phase, implemented similarly with the above outline.

So, summarizing in the plan: the agent will use `cardano-cli` via Haskell’s `callProcess` to assemble and submit transactions for taking swaps. Provide explicit example commands in documentation for clarity.

### 7. Storing Outcome in Knowledge Graph  
After attempting a transaction, the agent should store the outcome as a memory entry via the knowledge graph API. This will allow it to recall success or failure and adjust strategy. For example, if a trade was successful, it could store something like: *“Executed swap selling X for Y at price P on [timestamp]. Outcome: gained Z profit.”* If it failed (transaction didn’t go through or was a bad decision), that can also be noted: *“Attempted swap at price P but it failed or was not profitable.”*

The `storeAgentMemory` function will call the knowledge graph endpoint to add a new memory node. Likely a `POST /knowledge` with a JSON body containing the memory content and maybe a tag. For example:

```haskell
storeAgentMemory :: Manager -> MemoryEntry -> IO ()
storeAgentMemory manager entry = do
    let url = aoAgentApiUrl opts ++ "/knowledge"
        body = encode entry  -- MemoryEntry should be serializable to the expected format
    req0 <- parseRequest url
    let req = req0 { method = "POST"
                   , requestBody = RequestBodyLBS body
                   , requestHeaders = [("Content-Type","application/json")] }
    reqAuth <- applyApiKey req (aoAgentApiKey opts)
    resp <- httpLbs reqAuth manager
    when (statusCode (responseStatus resp) /= 200) $
       putStrLn $ "Warning: memory not stored (HTTP " ++ show (responseStatus resp) ++ ")"
```

We define `MemoryEntry` to match the API’s expected input. Possibly it needs a `content` field and maybe some metadata (like a type or tag). Example:

```haskell
data MemoryEntry = MemoryEntry 
  { content :: String
  , topic   :: String  -- maybe use trading pair or "CardanoSwapsAgent"
  , timestamp :: String
  }
```

We populate `mkOutcomeEntry orders action result` to produce a summary string or structured data about the cycle’s decision and outcome. For instance:

- If `action = TakeSwap utxo` and `result = True (success)`: 
  - content: `"Took swap " ++ utxo ++ " successfully. Bought X with Y at rate R. Balance now ..."` 
- If failed: 
  - content: `"Attempted swap "++ utxo ++" but transaction failed."` 
- If `action = NoAction`: maybe store a note like `"No action taken at price P, waiting for better opportunity."` (though we might not store every no-action, or it could flood memory – could skip storing no-ops or do it less frequently).

We include a timestamp (get current time via `getCurrentTime` and format) to each memory entry for chronological tracking.

Over time, the knowledge graph will accumulate these entries which the agent can query. The agent might later query the knowledge graph with a question like *“what was my last trade outcome?”* or just include recent entries in context as we did.

### 8. Repeat Loop  
The agent then sleeps for the specified interval and repeats the monitoring cycle indefinitely (until the process is terminated). To avoid overlapping cycles, we use a simple loop with delay as shown. This is sufficient for a single-threaded agent. If we wanted multiple pairs or concurrency, we could multi-thread or run separate agent instances per pair.

## Data & Event Flow Diagram  

Below is a diagram illustrating how data and events flow between **Cardano-Swaps (Agent Module)**, the **Cardano blockchain** (for on-chain data and transactions), and the **Agent API** components (Chat and Knowledge Graph):

 ([image]()) *Flow of interactions between the Cardano-Swaps agent, the Cardano blockchain, and the Agent API (chat & knowledge graph).*  

The sequence is as follows (numbered to correspond with the diagram):  

1. **Agent queries open swap offers** for the target trading pair using Koios or local node. This retrieves current open orders (UTxOs with the swap beacon) ([GitHub - fallen-icarus/cardano-swaps: A distributed order-book DEX using composable atomic swaps with full delegation control and endogenous liquidity.](https://github.com/fallen-icarus/cardano-swaps#:~:text=have%20the%20swappable%20asset,but%20only)).  
2. The agent **fetches memory context** from the Agent API’s knowledge graph. For example, it GETs stored memories relevant to the trading pair or recent outcomes.  
3. The agent calls **Agent API’s `/chat` endpoint** (POST request) with a compiled context that includes the live order book data and the retrieved memory. The AI analyzes this and returns an intelligent recommendation.  
4. **AI Response Received:** The agent interprets the AI’s recommendation (e.g., “take swap X for profit” or “no action”). The agent’s internal logic decides on an action (or inaction) accordingly.  
5. If action requires a blockchain transaction (e.g., **take a swap or create a swap**), the agent **builds and submits a transaction** to the Cardano network. This interacts with the Cardano-Swaps on-chain scripts to either fill an order or create a new one.  
6. The agent receives the **transaction result** (success or failure) from the network (or node feedback). It then **POSTs an entry to the knowledge graph** recording this outcome. This persistence will inform future decisions.  
7. The cycle then repeats (back to monitoring step 1) after a delay, allowing continuous adaptation.

## Example API Call Details  
To ensure clarity for developers, here are explicit examples of the Agent API calls at each interaction point, with example payloads:

- **Knowledge Graph Retrieval (before AI analysis):**  
  **Request:** `GET /knowledge?topic=ADA-TESTDJED&limit=5`  
  *(Assuming the API allows filtering by topic (pair) and limiting number of entries).*  
  **Response (200 OK):**  
  ```json
  [
    {
      "id": "mem_169236", 
      "content": "Took swap tx abc...def, sold 100 ADA for 300 DUST at 3.0 rate, profit +5 DUST.",
      "topic": "ADA-TESTDJED",
      "timestamp": "2025-04-10T15:04:05Z"
    },
    {
      "id": "mem_169237",
      "content": "No action taken - price unfavorable.",
      "topic": "ADA-TESTDJED",
      "timestamp": "2025-04-10T15:14:05Z"
    }
  ]
  ```  
  The agent would take the contents (and possibly timestamp or other data) from these memory entries to include in the prompt.

- **Chat Analysis Request (AI decision):**  
  **Request:** `POST /chat`  
  ```json
  {
    "messages": [
      {"role": "system", "content": "You are a Cardano trading agent with memory. Analyze market and decide an action."},
      {"role": "user", "content": 
         "Order Book:\n- 100 ADA for 300 DUST (user wants 3 DUST/ADA)\n- 200 ADA for 620 DUST (3.1 DUST/ADA)\n- Bid: 300 DUST for 105 ADA (user bids 2.857 DUST/ADA)\nMemory:\n- Last trade: sold 100 ADA @ 3.0, +5 DUST profit\n- Last decision: held for better price\nQuestion: Should I take any current offer, place a new offer, or wait?"}
    ]
  }
  ```  
  **Response (200 OK):**  
  ```json
  {
    "reply": {
      "role": "assistant",
      "content": "{\"action\": \"TAKE_SWAP\", \"utxo\": \"abc123...#1\", \"reason\": \"Arbitrage: 3.0 vs 3.1 profit potential\"}"
    }
  }
  ```  
  *(The exact JSON structure may differ; essentially the AI suggests an action with justification.)*

- **Knowledge Graph Storage (after action):**  
  **If action was taken and succeeded:**  
  **Request:** `POST /knowledge`  
  ```json
  {
    "content": "Took swap abc123...#1, bought 100 ADA at 2.95 DUST/ADA from order (profit ~+15 DUST).",
    "topic": "ADA-TESTDJED",
    "timestamp": "2025-04-19T16:30:00Z"
  }
  ```  
  **Response:** `200 OK` (with maybe an ID of stored memory or just success status).  

  **If no action:** we might skip storing, or store a minimal note:
  ```json
  {
    "content": "No action - market not favorable at 3.1 DUST/ADA ask vs 2.9 bid.",
    "topic": "ADA-TESTDJED",
    "timestamp": "2025-04-19T16:30:00Z"
  }
  ```  

These are illustrative. The development team should adapt to the actual Agent API spec. The key is to maintain a consistent **topic or key** so that retrieval queries bring back relevant memory for the context (e.g., using the trading pair or a unique agent name as a filter).

## Required Code Modifications Summary  

To recap, the following **files and directories** in `cardano-swaps` need additions or changes:

- **`cardano-swaps.cabal`**:  
  - Add any new modules to `other-modules` (e.g., `CLI.Agent`).  
  - If new dependencies are needed (e.g., `cryptonite` for hashing, or other libraries), add them to `build-depends`. However, existing dependencies (`http-client`, `aeson`, etc.) are likely sufficient. Ensure `aeson` is available (the presence of `aeson-pretty` implies `aeson` is already a dependency via cardano-swaps library, but add explicitly if needed).  
  - Ensure `optparse-applicative` is already in build-depends (it is). We will use it for parsing new options.  
  - Example addition:  
    ```cabal
    other-modules: 
      ..., 
      CLI.Agent
    build-depends: 
      , aeson, aeson-pretty, cryptonite
    ```  
    (Add cryptonite if using it for SHA-256, else can use another hashing method.)

- **`app/CLI/Parsers.hs`**: Add the `agent` subcommand parser (as shown in section above) and integrate it into the command hierarchy.

- **`app/CLI/Types.hs`**: Define `AgentOptions` and extend command type.

- **`app/CLI/Run.hs`**: Pattern match on the new command and call `runAgentMonitor`.

- **`app/CLI/Agent.hs`** (new file): Implement the agent loop and helper functions:
  - `runAgentMonitor :: AgentOptions -> IO ()` (with logic described: query -> AI -> action -> tx -> memory).  
  - Helper functions inside or in sub-sections: 
    - Querying functions (possibly can be placed in `CLI.Query.Koios` if we want to integrate there, but local usage in CLI.Agent is fine initially).  
    - API call functions for chat and knowledge (could be separate section or even another module `Agent.API` if organizing code, but not necessary given size).  
    - Transaction execution helpers (could also be in `CLI.Agent` or a separate `Agent.Tx` module). Possibly use some existing utilities from Cardano-Swaps if available (like constants for policy IDs or script file paths – maybe the Aiken compiled scripts are accessible, need to verify how cardano-swaps CLI finds `oneWaySwap.plutus` and `oneWayBeacons.plutus`. They may be in `scripts/` folder or embedded. Actually, the repository’s `scripts/` directory contains shell scripts (likely examples). The actual Plutus scripts may be compiled by Aiken and either stored in `aiken/build` or embedded via TemplateHaskell. We might simply check in `aiken/` for .plutus files. If present, we can refer to them by relative path (assuming running from project root). Alternatively, run `cardano-swaps` CLI’s existing commands to get them as shown above. The development team should ensure the required script files are accessible (maybe via installing cardano-swaps, they are packaged or the CLI knows them). Possibly `cardano-swaps` CLI can output the script file path if asked. If not, we might include the Aiken build step in setup.)

## Setup & Configuration Instructions  

To successfully run the integrated system, follow these steps: 

1. **Obtain Agent API Credentials:** If the Agent API requires authentication (API key or token), acquire the key from Flux Point Studios. Then, either:  
   - Set an environment variable (e.g., `AGENT_API_KEY`) with this key, or  
   - Provide it via the CLI option `--agent-api-key` when running the agent command.  
   The code will attach this to the Authorization header if present. If no auth is required, this can be left blank.

2. **Configure Agent Wallet:** Prepare a Cardano wallet for the agent with the assets it will trade. Specifically:
   - Fund the agent’s address with enough **ADA for transaction fees** and at least one UTxO purely in ADA to act as **collateral** for Plutus scripts (typically ~5 ADA reserved).  
   - Ensure the agent’s address has the **asset** that might be needed to trade. For example, if the agent might take swaps offering ADA for DUST, the agent needs some DUST to pay in exchange for ADA (to take those orders). Conversely, if it will create orders offering ADA, it needs ADA, etc. 
   - Save the payment **signing key** (`.skey` file) for the agent’s address on the machine. Update the code or config to point to this key file. The integration might default to looking for `agent.skey` in the working directory or a path specified via an environment variable or config file. (For security, restrict permissions on this key.)

3. **Ensure Cardano Node / Koios Access:** The agent needs to submit transactions to the blockchain:
   - **Option 1:** **Local Cardano Node** – If available, running `cardano-cli transaction submit` with `--mainnet` will use the node socket (set `CARDANO_NODE_SOCKET_PATH`). Also, `transaction build` will query UTxOs from the node. In this mode, the Koios usage is just for querying swaps; submission is local.  
   - **Option 2:** **Koios/Blockfrost** – Koios does not support tx submission, so if no local node, consider using Blockfrost API or another submission method. Since our plan uses `cardano-cli` for simplicity, a local node or relay is recommended. (In future, we could integrate a direct submit via a POST to Blockfrost or a Cardano wallet backend.)
   - **Protocol Parameters:** If using `cardano-cli build` offline, ensure `protocol.json` is present and updated. You can fetch it with:  
     ```
     cardano-cli query protocol-parameters --mainnet --out-file protocol.json
     ```  
     whenever parameters change (e.g., at epoch boundaries or network upgrades). Alternatively, the agent could fetch this at startup via Koios (`/epoch_params` endpoint) and save to a file or use in-memory (advanced).

4. **Build & Run the Modified CLI:** After code changes:
   - Install any new dependencies (e.g., `cabal update && cabal install cryptonite` if added).  
   - Build the project: `cabal build cardano-swaps`.  
   - Once built, you should have an updated `cardano-swaps` executable.  

5. **Start the Agent:** Run the new command with proper arguments. For example:  
   ```
   ./cardano-swaps agent \
     --offer-asset 00. \                 # ADA (empty policy and name)
     --ask-asset c0f8644a01a6bf5d...4f74686572546f6b656e0a \  # TestDJED policy.asset
     --interval 60 \
     --agent-api-url https://api.fluxpointstudios.com \
     --agent-api-key <YOUR_KEY> \
     --network mainnet
   ```  
   (The `--network mainnet` flag could be something we add to specify network; alternatively, we might infer network by whether policy IDs are 56 hex chars (mainnet) or shorter. It’s safer to have an explicit flag or separate commands for testnet vs mainnet. We can extend `AgentOptions` to include a network identifier or use cardano-cli’s environment variable for network.)

   This command will launch the agent. It will begin polling for orders and you should see log outputs such as: 
   - “Fetched N open swaps for ADA-TestDJED”  
   - “Memory retrieved: last trade profit +5 DUST”  
   - “AI recommendation: TAKE_SWAP utxo abc123...#1”  
   - “Executing swap transaction...”  
   - “Swap taken successfully, tx hash <XYZ>” (or error message if failed)  
   - Then it will sleep for 60 seconds and repeat.

6. **Monitoring:** Keep an eye on the agent’s output logs. It’s wise to first run on a testnet (preprod) with a test API (if available) to ensure everything works end-to-end without risking real funds. Adjust parameters or fix any issues as they arise (for example, if the AI’s response format isn’t as expected, refine the prompt or parsing).

7. **Security & Reliability:** 
   - Make sure to handle exceptions (network failures, API downtime, etc.). The plan code above uses simple error handling (printing and continuing). In production, you might implement retries for API calls or skip a cycle if something fails. 
   - The agent’s private key must remain secure. Avoid logging it. If the code spawns `cardano-cli` processes, ensure the commands don’t expose the key on screen. 
   - Test the agent with small amounts to verify the logic doesn’t do unintended trades. The AI’s behavior should also be monitored; put reasonable constraints (for instance, do not allow it to create swaps at extreme prices or spend all funds at once without some limits, unless that’s desired).

By following this plan, the development team can implement a robust integration where Cardano-Swaps is enhanced with an intelligent agent that continuously monitors and interacts with the DEX. The integration touches on all required points: **monitoring** (Koios queries in `CLI.Agent`), **interaction** (transaction logic using Cardano-Swaps scripts), **AI analysis** (Agent API /chat), and **memory persistence** (Agent API knowledge graph). All major code insertions have been mapped to specific files in the repository, with examples to guide implementation. 

Finally, thorough testing should be done in a controlled environment to fine-tune the agent’s decision-making and ensure all technical components (Haskell code, external CLI calls, API calls) work together smoothly before deploying on mainnet.