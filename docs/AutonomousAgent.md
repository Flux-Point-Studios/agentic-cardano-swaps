# Autonomous Agent for Cardano-Swaps

The autonomous agent module integrates the Agent API with Cardano-Swaps, creating an AI-powered trading bot that can monitor, analyze, and execute swaps automatically.

## Quick Start

1. **Set up environment**

   Make sure you have `cardano-cli` and `cardano-swaps` installed, and a local Cardano node or Blockfrost access.

   ```bash
   # Create agent wallet directory
   mkdir -p ~/.agent-wallet
   
   # Copy your agent wallet keys and address
   cp your-agent.skey ~/.agent-wallet/agent.skey
   cp your-agent.vkey ~/.agent-wallet/agent.vkey
   cp your-agent.addr ~/.agent-wallet/agent.addr
   
   # Ensure the wallet has ADA for fees and at least one UTxO with >= 5 ADA as collateral
   ```

2. **Agent API setup**

   Get your Agent API key from Flux Point Studios.

   ```bash
   export AGENT_API_KEY=your-api-key-here
   ```

3. **Run the agent**

   ```bash
   # For testnet with ADA and a test token
   cardano-swaps agent \
     --offer-asset "00." \
     --ask-asset "c0f8644a01a6...4f" \
     --interval 60 \
     --testnet-magic 1 \
     --dry-run  # Remove for real transactions
   
   # For mainnet with ADA and DJED
   cardano-swaps agent \
     --offer-asset "00." \
     --ask-asset "actual-djed-policy-id.token-name" \
     --interval 300 \
     --mainnet
   ```

## Configuration Options

| Flag | Description | Default |
|------|-------------|---------|
| `--offer-asset POLICY.ASSET` | Asset being offered (use "00." for ADA) | (required) |
| `--ask-asset POLICY.ASSET` | Asset being requested | (required) |
| `--interval SECONDS` | Polling interval | 30 |
| `--agent-api-url URL` | Base URL for Agent API | https://api.fluxpointstudios.com |
| `--agent-api-key KEY` | Agent API key | (env: AGENT_API_KEY) |
| `--signing-key FILE` | Payment signing key file | ~/.agent-wallet/agent.skey |
| `--verification-key FILE` | Payment verification key file | ~/.agent-wallet/agent.vkey |
| `--agent-address FILE\|ADDR` | Agent payment address or file | ~/.agent-wallet/agent.addr |
| `--mainnet` | Use mainnet | - |
| `--testnet-magic N` | Use testnet with magic number N | - |
| `--protocol-params FILE` | Protocol parameters file override | ~/.agent-wallet/protocol-params.json |
| `--node-socket FILE` | Node socket file | (env: CARDANO_NODE_SOCKET_PATH) |
| `--blockfrost-project-id ID` | Blockfrost project ID | (env: BLOCKFROST_PROJECT_ID) |
| `--dry-run` | Prepare but don't submit transactions | false |
| `--maker-only` | Allow CreateSwap actions | false |

## Advanced Features

- **Automatic protocol parameter refresh**: The agent refreshes protocol parameters every 18 hours.
- **Log rotation**: Log files are automatically rotated daily with gzip compression.
- **Exponential backoff**: The agent implements exponential backoff for API requests.
- **Order summarization**: When sending data to the AI, only the most relevant orders are included.
- **Graceful shutdown**: The agent handles SIGINT (Ctrl+C) gracefully, cleaning up resources.

## Example Use Cases

1. **Arbitrage Bot**: Monitor specific trading pairs for profitable opportunities.
2. **Market Making**: With `--maker-only` flag, create and maintain market-making orders.
3. **Order Book Monitor**: Run with `--dry-run` to test strategies without executing trades.

## Logs

Logs are stored in `~/.agent-wallet/logs/agent-YYYYMMDD.log` and rotated daily.

## Support

For issues with the agent, please file a GitHub issue. For Agent API access, contact Flux Point Studios. 