#!/usr/bin/env bash
set -euo pipefail
VER=10.8.0.0
TAR="cardano-cli-${VER}-x86_64-linux.tar.gz"
URL="https://github.com/IntersectMBO/cardano-cli/releases/download/cardano-cli-${VER}/${TAR}"

echo "▶︎  Fetching ${URL}"
curl -L -o "${TAR}" "${URL}"

echo "📦  Unpacking…"
tar -xf "${TAR}"

echo "🚀  Installing to ~/.local/bin …"
install -Dm755 cardano-cli-x86_64-linux "$HOME/.local/bin/cardano-cli"

echo "✅  Installed:"
cardano-cli --version
