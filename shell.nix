# Traditional Nix shell for users not using flakes
# For flake users, use: nix develop
#
# Usage:
#   nix-shell                    # Full development environment
#   nix-shell --arg apiOnly true # API client only (no Wasm)

{ pkgs ? import <nixpkgs> { config.allowUnfree = true; }
, apiOnly ? false
}:

let
  # Import ghc-wasm-meta for WebAssembly support
  # This requires adding the ghc-wasm-meta channel or using fetchTarball
  ghc-wasm-meta-src = builtins.fetchTarball {
    url = "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz";
    # Note: For reproducibility, you should pin to a specific commit
    # and provide a sha256 hash
  };

  ghc-wasm-flake = (import ghc-wasm-meta-src);

  # Try to get the wasm toolchain, fallback gracefully if not available
  ghc-wasm =
    if apiOnly then null
    else if builtins.hasAttr "defaultPackage" ghc-wasm-flake
    then ghc-wasm-flake.defaultPackage.${pkgs.system}
    else null;

  # Common Haskell development tools
  commonTools = [
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.git
    pkgs.pkg-config
    pkgs.zlib
  ];

  # Fastly-specific tools (available in nixpkgs)
  fastlyTools = [
    pkgs.fastly   # Fastly CLI
  ] ++ pkgs.lib.optionals (!apiOnly) [
    pkgs.viceroy  # Viceroy for local testing (only needed for Compute)
  ];

  # Standard GHC for API client development
  standardHaskell = [
    pkgs.ghc
    pkgs.haskell-language-server
  ];

in pkgs.mkShell {
  name = if apiOnly then "fastly-api-client" else "fastly-full-dev";

  buildInputs = commonTools ++ fastlyTools
    ++ (if apiOnly then standardHaskell else [])
    ++ (if ghc-wasm != null then [ ghc-wasm ] else []);

  shellHook = if apiOnly then ''
    echo "📦 Fastly API Client Development Environment"
    echo ""
    echo "Build and test the API client:"
    echo "  cd packages/fastly"
    echo "  cabal build"
    echo "  cabal repl"
    echo ""
  '' else ''
    echo "🚀 Fastly Haskell Development Environment"
    echo ""
    ${if ghc-wasm != null then ''
      echo "✓ GHC WebAssembly toolchain available"
      echo ""
      echo "Wasm tools:"
      echo "  • wasm32-wasi-ghc"
      echo "  • wasm32-wasi-cabal"
      echo "  • wasm32-wasi-ghc-pkg"
      echo ""
      echo "Build Compute application:"
      echo "  cd compute-examples/hello-world"
      echo "  ./build.sh"
      echo "  viceroy bin/main.wasm"
      echo ""
    '' else ''
      echo "⚠ GHC WebAssembly toolchain not available"
      echo ""
      echo "To enable WebAssembly support, use the flake:"
      echo "  nix develop"
      echo ""
      echo "Or manually install ghc-wasm-meta:"
      echo "  https://gitlab.haskell.org/ghc/ghc-wasm-meta"
      echo ""
    ''}
    echo "For API client development:"
    echo "  cd packages/fastly"
    echo "  cabal build"
    echo ""
  '';

  # Environment variables for Wasm toolchain
  WASM_GHC = if ghc-wasm != null then "${ghc-wasm}/bin/wasm32-wasi-ghc" else "";
  WASM_CABAL = if ghc-wasm != null then "${ghc-wasm}/bin/wasm32-wasi-cabal" else "";
  WASM_GHC_PKG = if ghc-wasm != null then "${ghc-wasm}/bin/wasm32-wasi-ghc-pkg" else "";
}
