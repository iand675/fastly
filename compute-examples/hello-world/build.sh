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
rm -rf ../../dist-newstyle
rm -f bin/main.wasm

echo -e "${YELLOW}Step 2: Configure with wasm32-wasi-ghc${NC}"
# Build from the repository root to use cabal.project
cd ../..
wasm32-wasi-cabal configure fastly-compute-hello \
    --with-compiler=wasm32-wasi-ghc \
    --with-hc-pkg=wasm32-wasi-ghc-pkg \
    --with-hsc2hs=wasm32-wasi-hsc2hs

echo -e "${YELLOW}Step 3: Build the WebAssembly module${NC}"
wasm32-wasi-cabal build fastly-compute-hello -O2
cd compute-examples/hello-world

echo -e "${YELLOW}Step 4: Copy the .wasm file${NC}"
mkdir -p compute-examples/hello-world/bin
WASM_FILE=$(find ../../dist-newstyle -name "fastly-compute-hello" -type f | grep -v ".hi\|.o")
if [ -z "$WASM_FILE" ]; then
    echo -e "${RED}Error: Could not find built .wasm file${NC}"
    exit 1
fi
cp "$WASM_FILE" compute-examples/hello-world/bin/main.wasm

echo -e "${YELLOW}Step 5: Package with Fastly metadata${NC}"
# Use fastly compute pack to create the deployable package
# This adds the necessary Fastly metadata to the Wasm binary
if command -v fastly &> /dev/null; then
    fastly compute pack --wasm-binary compute-examples/hello-world/bin/main.wasm
    echo -e "${GREEN}✓ Package created!${NC}"
else
    echo -e "${YELLOW}⚠ Fastly CLI not found - skipping packaging${NC}"
    echo "  Install with: brew install fastly/tap/fastly"
    echo "  The .wasm file is ready, but you'll need to package it later"
fi

echo -e "${GREEN}✓ Build successful!${NC}"
echo ""
echo "Output: bin/main.wasm"
echo ""
echo "Next steps:"
echo "  1. Test locally: fastly compute serve"
echo "     Or with Viceroy directly: viceroy bin/main.wasm"
echo "  2. Deploy: fastly compute deploy"
echo ""
echo "File size:"
ls -lh compute-examples/hello-world/bin/main.wasm
