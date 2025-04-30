#!/bin/bash

# Check for required environment variables
if [ -z "$AGENT_API_KEY" ]; then
  echo "Error: AGENT_API_KEY environment variable not set"
  echo "Please set it to your OpenAI API key with: export AGENT_API_KEY=your_openai_api_key"
  exit 1
fi

if [ -z "$PROTOCOL_PARAMS" ]; then
  # Default location if not specified
  export PROTOCOL_PARAMS="$HOME/.agent-wallet/protocol-params.json"
  echo "Using default protocol parameters location: $PROTOCOL_PARAMS"
fi

# Ensure protocol parameters exist
if [ ! -f "$PROTOCOL_PARAMS" ]; then
  echo "Protocol parameters file not found at $PROTOCOL_PARAMS"
  echo "Fetching current parameters..."
  cardano-cli query protocol-parameters --mainnet --out-file "$PROTOCOL_PARAMS"
fi

# Set up paths for agent wallet
AGENT_SKEY="$HOME/.agent-wallet/agent.skey"
AGENT_VKEY="$HOME/.agent-wallet/agent.vkey" 
AGENT_ADDR="$HOME/.agent-wallet/agent.addr"

# Check if files exist
for file in "$AGENT_SKEY" "$AGENT_VKEY" "$AGENT_ADDR"; do
  if [ ! -f "$file" ]; then
    echo "Error: Required file not found: $file"
    echo "Please set up your agent wallet files in ~/.agent-wallet/"
    exit 1
  fi
done

# Define Talos token
TALOS_POLICY="97bbb7db0baef89caefce61b8107ac74c7a7340166b39d906f174bec"
TALOS_NAME="54616c6f73"  # Hex for "Talos"
TALOS_ASSET="$TALOS_POLICY.$TALOS_NAME"

# Default to dry run mode unless --live is specified
DRY_RUN="--dry-run"
if [ "$1" == "--live" ]; then
  DRY_RUN=""
  echo "⚠️ LIVE MODE ENABLED - Transactions will be submitted to the blockchain"
  # Countdown
  for i in {5..1}; do
    echo -ne "\rStarting in $i seconds... (Ctrl+C to cancel)"
    sleep 1
  done
  echo -e "\rStarting agent in LIVE mode...                        "
else
  echo "🔍 DRY RUN MODE - Transactions will be prepared but not submitted"
  echo "Use './run-talos-agent.sh --live' to enable live transaction submission"
fi

echo ""
echo "======================================================================"
echo "Starting Cardano Swaps Agent"
echo "======================================================================"
echo "Trading pair: ADA <-> Talos"
echo "Using OpenAI o4-mini model"
echo "----------------------------------------------------------------------"

# Run the agent
cabal run cardano-swaps -- agent \
  --offer-asset lovelace \
  --ask-asset "$TALOS_ASSET" \
  --interval 60 \
  --agent-api-url https://api.openai.com/v1/responses \
  --agent-api-key "$AGENT_API_KEY" \
  --signing-key "$AGENT_SKEY" \
  --verification-key "$AGENT_VKEY" \
  --agent-address "$AGENT_ADDR" \
  --protocol-params "$PROTOCOL_PARAMS" \
  --mainnet \
  $DRY_RUN 