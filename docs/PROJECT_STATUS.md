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
   - ADA for fees **plus** a pure‑ADA UTxO ≥ 5 ADA for collateral.
   - Token balances (e.g. *Talos*) if you plan to **take** swaps.

3. **Set environment & API keys**
   ```bash
   export BLOCKFROST_PROJECT_ID=<blockfrost_key>
   export AGENT_API_KEY=<openai_api_key>
   export PROTOCOL_PARAMS=$HOME/.agent-wallet/protocol-params.json
   ```

4. **Fetch protocol parameters**
   ```bash
   cardano-cli query protocol-parameters --mainnet --out-file "$PROTOCOL_PARAMS"
   ```

5. **Run the agent** (wrapper script)
   ```bash
   # Dry run – NO tx submission
   ./run-talos-agent.sh

   # Live trading
   ./run-talos-agent.sh --live
   ```
   Or manually:
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
| `--offer-asset POLICY.ASSET` | Asset **offered** (`lovelace` → ADA)        | *(required)*                              |
| `--ask-asset POLICY.ASSET`   | Asset **requested**                         | *(required)*                              |
| `--interval SECONDS`         | Polling interval                            | `30`                                      |
| `--agent-api-url URL`        | Base URL for Agent API                      | `https://api.fluxpointstudios.com`        |
| `--agent-api-key KEY`        | Agent/OpenAI key (env `AGENT_API_KEY`)      | *(env or flag)*                           |
| `--signing-key FILE`         | Payment signing key                         | `~/.agent-wallet/agent.skey`              |
| `--verification-key FILE`    | Verification key                            | `~/.agent-wallet/agent.vkey`              |
| `--agent-address FILE\|ADDR` | Payment address or file                     | `~/.agent-wallet/agent.addr`              |
| `--mainnet`                  | Use main‑net                                | *(off = preprod)*                         |
| `--testnet-magic N`          | Test‑net magic                              |                                           |
| `--protocol-params FILE`     | Protocol‑params override                    | `~/.agent-wallet/protocol-params.json`    |
| `--blockfrost-project-id ID` | Blockfrost PID                              |                                           |
| `--dry-run`                  | Do **not** submit tx                        | `false`                                   |
| `--maker-only`               | Only *create* swaps                         | `false`                                   |

---

## 4. Agent Architecture

1. **Monitoring Loop** (`app/CLI/Agent.hs`)
   * Rotate logs daily, exponential back‑off on failures.
   * Query open swaps via **Koios** (beacon index).
   * Pull contextual memory from `/knowledge` when non‑OpenAI.
   * Ask LLM (OpenAI o4‑mini **Responses API** or proprietary `/chat`).
   * Parse JSON → `TakeSwap` | `CreateSwap` | `NoAction`.
   * Build & submit tx via `cardano-cli` helpers.
   * Persist outcome back to knowledge graph.

2. **Key Code Paths**
   * `app/CLI/Query/Koios.hs` – Koios client & swap discovery
   * `app/CLI/Agent.hs`       – main loop & AI integration
   * `run-talos-agent.sh`     – opinionated wrapper (Talos token)

3. **AI & Memory**
   * OpenAI call ⇒ `model=o4-mini`, `reasoning.effort=medium`, parse `output_text`.
   * Agent‑API call ⇒ streaming conversation, parse `reply` JSON.
   * Memory keyed as `${offer}-${ask}`.

---

## 5. Transaction Flow *(Take Swap)*
```bash
cardano-swaps spending-redeemers one-way --swap --out-file take.json
cardano-swaps beacon-redeemers  one-way --mint-or-burn --out-file beacon.json

cardano-cli transaction build \
  --tx-in            <swapUtxo> \
  --tx-in            <fundingUtxo> \
  --tx-in-collateral <collateralUtxo> \
  --tx-in-script-file one-way.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file take.json \
  --mint "-1 <beaconPolicy>.<beaconName>" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file beacon.json \
  --change-address ${AGENT_ADDRESS} \
  --protocol-params-file ${PROTOCOL_PARAMS} \
  --out-file tx.body

cardano-cli transaction sign   --tx-body-file tx.body   --signing-key-file ~/.agent-wallet/agent.skey --out-file tx.signed
cardano-cli transaction submit --tx-file tx.signed ${NET}
```
*(Create‑Swap is symmetric: mint + beacon redeemer, embed datum, pay to script address.)*

---

## 6. Guard‑Rails & Best Practices
| Risk                 | Mitigation                                              |
|----------------------|----------------------------------------------------------|
| LLM hallucination    | Hard‑cap ±3 % deviation from mid‑price before signing    |
| Wallet drain         | Track hourly ADA spent; abort once `maxHourlyADA` hit    |
| Fee explosion        | `evaluate-tx`; abort when > 0.5 ADA                      |
| Missing collateral   | Exit if no ADA‑only UTxO ≥ 5 ADA on startup              |
| JSON parse failures  | Fallback → `NoAction`; log raw AI output                 |

---

## 7. Implementation Notes

1. **Token Names in Hex** – e.g. *Talos* → `54616c6f73`.
2. **OpenAI Responses Payload** – bespoke schema (`model`, `reasoning`, `input`).
3. **Verbose Logging** – `LineBuffering` + tagged log levels.
4. **Explicit Paths** – never rely on `~`; pass absolute paths.
5. **Wrapper Script** – `run-talos-agent.sh` guards, countdown, `--live` flag.

---

## 8. Local Testing: Seeding Your Own Order‑Book (Pre‑prod)
A short recipe to publish a **one‑way ADA ⇄ Talos** swap so the agent has something to consume.

> **TL;DR** Mint the beacons ➜ lock the swap UTxO ➜ run the agent ➜ watch it fill.

### 8.1 Prerequisites
- **Wallet A (maker)** with ≥ 15 ADA.
- **Wallet B (agent)** with ADA + *Talos* supply & agent keys.
- *Talos* test token on pre‑prod (`policyid.54616c6f73`).
- Pre‑prod node or Blockfrost key.

```bash
export NET="--testnet-magic 1"
export MAKER_ADDR=$(cat maker.addr)
export MAKER_SKEY=maker.skey
export TALOS="<policyid>.54616c6f73"   # replace <policyid>
```

### 8.2 Mint (skip if token already exists)
```bash
# example mint 1 Talos to the maker
cardano-cli conway transaction build-raw ...
```
*(Standard native‑token mint – see Cardano docs.)*

### 8.3 Derive Beacons
```bash
beaconPid=$(cardano-swaps beacon-info one-way policy-id --stdout)

pairName=$(cardano-swaps beacon-info one-way pair-beacon \
            --offer-asset 00. --ask-asset ${TALOS} --stdout)

offerName=$(cardano-swaps beacon-info one-way offer-beacon --offer-asset 00. --stdout)
askName=$(cardano-swaps beacon-info one-way ask-beacon   --ask-asset   ${TALOS} --stdout)

pairBeacon="${beaconPid}.${pairName}"
offerBeacon="${beaconPid}.${offerName}"
askBeacon="${beaconPid}.${askName}"
```

### 8.4 Create Datum & Redeemers
```bash
# offer 3 ADA, ask 10 Talos  (price = 10 / 3)
cardano-swaps datums one-way \
  --offer-asset 00. --offer-price '3000000 / 10' \
  --ask-asset ${TALOS} \
  --out-file datum.json

# redeemers
cardano-swaps spending-redeemers one-way --create         --out-file spendCreate.json
cardano-swaps beacon-redeemers   one-way --mint-or-burn    --out-file beaconRedeemer.json
```

### 8.5 Export Scripts
```bash
cardano-swaps scripts one-way swap-script   --out-file swap.plutus
cardano-swaps scripts one-way beacon-script --out-file beacon.plutus
```

### 8.6 Build & Submit *Create‑Swap* Tx
```bash
OFFER_IN="<utxoFundADAAndTalos>"
COLL="<utxoAdaOnly>=5ADA"
SCRIPT_ADDR=$(cardano-swaps script-address one-way)

cardano-cli conway transaction build \
  --tx-in ${OFFER_IN} \
  --tx-in-collateral ${COLL} \
  --tx-out "${SCRIPT_ADDR}+3000000 lovelace + 10 ${TALOS} + 1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon}" \
  --tx-out-inline-datum-file datum.json \
  --mint "1 ${pairBeacon} + 1 ${offerBeacon} + 1 ${askBeacon}" \
  --mint-script-file beacon.plutus \
  --mint-redeemer-file beaconRedeemer.json \
  --tx-in-script-file swap.plutus \
  --tx-in-redeemer-file spendCreate.json \
  --change-address ${MAKER_ADDR} \
  ${NET} \
  --protocol-params-file protocol.json \
  --out-file create.body

cardano-cli conway transaction sign   --tx-body-file create.body --signing-key-file ${MAKER_SKEY} ${NET} --out-file create.signed
cardano-cli conway transaction submit --tx-file create.signed ${NET}
```
After 1‑2 confirmations Koios indexes the swap; the agent will discover it on its next poll.

### 8.7 Run the Agent Against Your Swap
```bash
./run-talos-agent.sh \
  --offer-asset lovelace \
  --ask-asset   ${TALOS} \
  --testnet-magic 1 \
  --poll 30 \
  --dry-run       # drop for live fill
```

### 8.8 One‑Liner Seeding Script
Create `seed_swap.sh` to automate steps 8.3‑8.6 – handy for repeated testing.

```bash
#!/usr/bin/env bash
# usage: ./seed_swap.sh <offerADA> <askTalos>
# … (script body elided for brevity) …
```

---

## 9. Road‑Map / Outstanding Work
- **CreateSwap** implementation (maker flow).
- Pure‑Haskell builder using `cardano-api` (no `cardano-cli` shell outs).
- YAML/JSON config for multiple trading pairs & dynamic parameters.
- Container + `systemd` unit for CI deployment.
- e2e regression tests (QuickCheck) with local Koios‑mock.

---

_For support, open GitHub issues or ping Flux Point Studios._

