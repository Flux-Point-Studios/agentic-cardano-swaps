#!/usr/bin/env bash
set -euo pipefail

# (optional) export secrets injected by Demeter
if [[ -f /workspace/.env ]]; then
  export "$(grep -v '^#' /workspace/.env | xargs)"
fi

export CARDANO_NET_MAGIC=1   # main‑net magic
# OR let users override:
: "${CARDANO_NET_MAGIC:=1}"


# run the binary we just built
exec ./dist-newstyle/build/*/*/cardano-swaps-*/x/cardano-swaps/build/cardano-swaps/cardano-swaps --help
