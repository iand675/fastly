#!/usr/bin/env bash
#
# Build script for Fastly Compute@Edge Haskell application
#
# This script builds a Haskell application to WebAssembly using
# GHC's wasm32-wasi backend, producing a .wasm file suitable for
# deployment to Fastly Compute.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Building Fastly Compute Haskell Application${NC}"
echo "=============================================="

# Check if wasm32-wasi-ghc is available
if ! command -v wasm32-wasi-ghc &> /dev/null; then
    echo -e "${RED}Error: wasm32-wasi-ghc not found!${NC}"
    echo ""
    echo "You need to install GHC with WebAssembly support."
    echo "See the README.md for installation instructions."
    echo ""
    echo "Quick start:"
    echo "  1. Install ghcup: https://www.haskell.org/ghcup/"
    echo "  2. Follow GHC Wasm backend setup: https://gitlab.haskell.org/ghc/ghc-wasm-meta"
    exit 1
fi

echo -e "${YELLOW}Step 1: Clean previous build${NC}"
rm -rf dist-newstyle
rm -f bin/main.wasm

echo -e "${YELLOW}Step 2: Configure with wasm32-wasi-ghc${NC}"
wasm32-wasi-cabal configure \
    --with-compiler=wasm32-wasi-ghc \
    --with-hc-pkg=wasm32-wasi-ghc-pkg \
    --with-hsc2hs=wasm32-wasi-hsc2hs

echo -e "${YELLOW}Step 3: Build the WebAssembly module${NC}"
wasm32-wasi-cabal build -O2

echo -e "${YELLOW}Step 4: Copy the .wasm file${NC}"
mkdir -p bin
WASM_FILE=$(find dist-newstyle -name "fastly-compute-hello" -type f | grep -v ".hi\|.o")
if [ -z "$WASM_FILE" ]; then
    echo -e "${RED}Error: Could not find built .wasm file${NC}"
    exit 1
fi
cp "$WASM_FILE" bin/main.wasm

echo -e "${GREEN}✓ Build successful!${NC}"
echo ""
echo "Output: bin/main.wasm"
echo ""
echo "Next steps:"
echo "  1. Test locally with Viceroy: viceroy bin/main.wasm"
echo "  2. Create a Fastly service and deploy the .wasm file"
echo ""
echo "File size:"
ls -lh bin/main.wasm
