# PROJECT_STATUS.md

## 1. Overview
The Cardano‑Swaps Autonomous Agent integrates an AI‑driven trading bot into the Cardano‑Swaps DEX. It continuously monitors swap order books, uses the OpenAI Responses API for reasoning, and executes on‑chain transactions to take or place swaps. This document consolidates setup, implementation details, and project status.

---

## 2. Quick Start

1. **Install prerequisites**
   ```bash
   # GHC 9.6.4 & cabal
   curl -sSf https://get-ghcup.haskell.org | sh
   ghcup install ghc 9.6.4 cabal 3.10.2.1

   # cardano-cli
   # Download from IOG release page

   # cardano-swaps CLI
   git clone https://github.com/fallen-icarus/cardano-swaps
   cd cardano-swaps
   cabal build exe:cardano-swaps
   sudo cp "$(cabal list-bin exe:cardano-swaps)" /usr/local/bin/
   ```
2. **Configure Agent Wallet & Keys**
   ```bash
   mkdir -p ~/.agent-wallet
   cp your-agent.skey ~/.agent-wallet/agent.skey
   cp your-agent.vkey ~/.agent-wallet/agent.vkey
   cp your-agent.addr ~/.agent-wallet/agent.addr
   ```
   Ensure you have:
   - ADA for fees + a pure-ADA UTxO ≥ 5 ADA as collateral
   - Token balances if you plan to take those swaps

3. **Set environment & API keys**
   ```bash
   export BLOCKFROST_PROJECT_ID=your_blockfrost_key
   export AGENT_API_KEY=your_openai_api_key
   export PROTOCOL_PARAMS=~/.agent-wallet/protocol-params.json
   ```

4. **Fetch protocol parameters**
   ```bash
   cardano-cli query protocol-parameters --mainnet --out-file $PROTOCOL_PARAMS
   ```

5. **Run the agent** (using the provided script)
   ```bash
   # For dry run (no transactions submitted)
   ./run-talos-agent.sh
   
   # For live trading
   ./run-talos-agent.sh --live
   ```

   Or manually with the command:
   ```bash
   cabal run cardano-swaps -- agent \
     --offer-asset lovelace \
     --ask-asset 97bbb7db0baef89caefce61b8107ac74c7a7340166b39d906f174bec.54616c6f73 \
     --interval 60 \
     --agent-api-url https://api.openai.com/v1/responses \
     --agent-api-key "$AGENT_API_KEY" \
     --signing-key ~/.agent-wallet/agent.skey \
     --verification-key ~/.agent-wallet/agent.vkey \
     --agent-address ~/.agent-wallet/agent.addr \
     --protocol-params "$PROTOCOL_PARAMS" \
     --mainnet \
     --dry-run
   ```
   Remove `--dry-run` for production execution.

---

## 3. Configuration Options
| Flag                         | Description                                 | Default                                   |
|------------------------------|---------------------------------------------|-------------------------------------------|
| `--offer-asset POLICY.ASSET`| Asset offered (use `lovelace` for ADA)      | (required)                                |
| `--ask-asset POLICY.ASSET`  | Asset requested                             | (required)                                |
| `--interval SECONDS`         | Polling interval (seconds)                  | `30`                                      |
| `--agent-api-url URL`        | Base URL for Agent API                      | `https://api.fluxpointstudios.com`       |
| `--agent-api-key KEY`        | Agent API key (env: `AGENT_API_KEY`)        | (env or flag)                             |
| `--signing-key FILE`         | Payment signing key file                    | `~/.agent-wallet/agent.skey`              |
| `--verification-key FILE`    | Verification key file                       | `~/.agent-wallet/agent.vkey`              |
| `--agent-address FILE|ADDR`| Payment address/file                        | `~/.agent-wallet/agent.addr`              |
| `--mainnet`                  | Use mainnet                                 | _(off=preprod)_                           |
| `--testnet-magic N`          | Testnet magic number                        | _(none)_                                  |
| `--protocol-params FILE`     | Protocol parameters file override           | `~/.agent-wallet/protocol-params.json`    |
| `--blockfrost-project-id ID` | Blockfrost Project ID (env: `BLOCKFROST_PROJECT_ID`)| _(none)_                         |
| `--dry-run`                  | Do not submit transactions                  | `false`                                   |
| `--maker-only`               | Only create swap offers                     | `false`                                   |

---

## 4. Agent Architecture

1. **Monitoring Loop** (in `app/CLI/Agent.hs`)
   - Rotate logs daily, exponential backoff on errors.
   - Query open swaps via Koios for both directions (beacon tokens).
   - Fetch memory from Agent API's knowledge graph (`GET /knowledge?topic=<pair>`).
   - Send context to either:
     - OpenAI Responses API (`/v1/responses`) for o4-mini model when `api.openai.com` is detected in `--agent-api-url`.
     - Standard Agent API `/chat` endpoint in other cases.
   - Parse structured JSON response (`action`: `TakeSwap` | `CreateSwap` | `NoAction`).
   - Execute on-chain txs via `cardano-cli` calls (take or create swap).
   - Record outcome back to knowledge graph (`POST /knowledge`).

2. **Key modules & files**
   - `app/CLI/Types.hs`: defines `AgentOptions` & `CliCommand` variant
   - `app/CLI/Parsers.hs`: subcommand `agent` parser
   - `app/CLI/Run.hs`: dispatch to `runAgentMonitor`
   - `app/CLI/Agent.hs`: main loop + helpers
   - `run-talos-agent.sh`: convenience script for running agent with Talos token

3. **AI & Memory**
   - For OpenAI, calls use the Responses API (`model=o4-mini`, `reasoning.effort=medium`).
   - Memory entries tagged by trading pair (e.g., `ADA-<TOKEN>`).
   - Automatic detection of API endpoint type based on URL.
   - When using OpenAI API, memory operations are handled locally (logged but not stored externally).

---

## 5. Transaction Flow

**Take Swap**:
1. Generate redeemers:
   ```bash
   cardano-swaps spending-redeemers one-way --swap --out-file take.json
   cardano-swaps beacon-redeemers  one-way --mint-or-burn --out-file beacon.json
   ```
2. Build & sign:
   ```bash
   cardano-cli transaction build \
     --tx-in {swapUtxo} \
     --tx-in {agentUtxoWithPaymentAsset} \
     --tx-in-collateral {collateralUtxo} \
     --tx-in-script-file one-way.plutus \
     --tx-in-inline-datum-present \
     --tx-in-redeemer-file take.json \
     --mint "-1 ${beaconPolicy}.${beaconName}" \
     --mint-script-file beacon.plutus \
     --mint-redeemer-file beacon.json \
     --change-address ${AGENT_ADDRESS} \
     --protocol-params-file ${PROTOCOL_PARAMS} \
     --out-file tx.body
   cardano-cli transaction sign --tx-body-file tx.body --signing-key-file ~/.agent-wallet/agent.skey --out-file tx.signed
   cardano-cli transaction submit --tx-file tx.signed --mainnet
   ```

**(Create Swap is analogous, minting beacon + embedding datum)**

---

## 6. Guard‑Rails & Best Practices
| Risk                   | Mitigation                                                                                   |
|------------------------|----------------------------------------------------------------------------------------------|
| AI hallucination       | Hard‑cap price deviation ±3 % from current mid‑price                                         |
| Draining wallet        | Track hourly ADA spent; skip if > maxHourlyADA                                              |
| Fee explosion          | `evaluate-tx` cost estimate; abort if fee > 0.5 ADA                                          |
| Missing collateral     | On startup, exit with error if no ADA-only UTxO ≥ 5 ADA                                      |
| JSON parse failures    | Fallback to `NoAction`; log full AI response to `agent-errors.log`                         |

---

## 7. Key Implementation Notes

1. **Token Names in Hex Format**
   - Always use hex-encoded token names when working with Cardano assets
   - For "Talos" token, use `54616c6f73` (hex for "Talos") rather than the UTF-8 string
   - Example: `97bbb7db0baef89caefce61b8107ac74c7a7340166b39d906f174bec.54616c6f73`

2. **OpenAI Responses API Integration**
   - The agent detects OpenAI API by checking if `api.openai.com` appears in the API URL
   - When using OpenAI, a different payload format is used:
   ```json
   {
     "model": "o4-mini",
     "reasoning": {"effort": "medium"},
     "input": [{"role": "user", "content": "..."}],
     "max_output_tokens": 1000
   }
   ```
   - The response is extracted from the `output_text` field instead of `choices[0].message.content`

3. **Debug Logging**
   - The agent now includes more verbose logging to help diagnose issues
   - Console output has `LineBuffering` to ensure immediate log visibility

4. **Command Line Specification**
   - All file paths should be specified explicitly via command line arguments rather than relying on environment variables
   - Specify absolute paths to avoid expansion issues with `~`

5. **Helper Script**
   - The `run-talos-agent.sh` script provides a convenient way to run the agent
   - It includes error checking, verifies required files, and offers a countdown when using live mode
   - The script accepts the `--live` flag to enable transaction submission

---

## 8. Next Steps
- **CreateSwap** implementation: full order placement flow
- **Pure‑Haskell TX builder** using `cardano-api`
- **Config file** (`agent.yaml`) for dynamic pair updates
- **Dockerize** + systemd service for CI/production deployment

---

_For support, file issues on GitHub or contact Flux Point Studios for API access._

