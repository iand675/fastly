{
  description = "Fastly Haskell Libraries - API client and Compute@Edge support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # GHC WebAssembly backend from ghc-wasm-meta
    ghc-wasm-meta = {
      url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, ghc-wasm-meta, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          # Allow unfree packages (in case Fastly CLI has unfree components)
          config.allowUnfree = true;
        };

        # Get the GHC wasm toolchain from ghc-wasm-meta
        # This provides wasm32-wasi-ghc, wasm32-wasi-cabal, etc.
        ghc-wasm = ghc-wasm-meta.packages.${system}.default;

      in
      {
        # Development shell for working on Fastly projects
        devShells = {
          # Default shell: Full development environment with Wasm toolchain
          default = pkgs.mkShell {
            name = "fastly-haskell-dev";

            buildInputs = [
              # GHC WebAssembly toolchain from ghc-wasm-meta
              ghc-wasm

              # Standard Haskell development tools
              pkgs.cabal-install
              pkgs.haskell-language-server
              pkgs.ghcid

              # Fastly tools
              pkgs.fastly      # Fastly CLI
              pkgs.viceroy     # Local Compute testing

              # Build tools
              pkgs.pkg-config
              pkgs.zlib

              # Version control
              pkgs.git
            ];

            shellHook = ''
              echo "🚀 Fastly Haskell Development Environment"
              echo ""
              echo "Available tools:"
              echo "  • wasm32-wasi-ghc     - GHC WebAssembly compiler"
              echo "  • wasm32-wasi-cabal   - Cabal for WebAssembly"
              echo "  • fastly              - Fastly CLI"
              echo "  • viceroy             - Local Compute testing"
              echo "  • ghcid               - Fast recompilation"
              echo "  • haskell-language-server - LSP server"
              echo ""
              echo "Quick start:"
              echo "  cd compute-examples/hello-world"
              echo "  ./build.sh            # Build WebAssembly"
              echo "  viceroy bin/main.wasm # Test locally"
              echo ""
              echo "For API client development:"
              echo "  cabal build fastly"
              echo "  cabal repl fastly"
              echo ""
            '';

            # Set environment variables for the Wasm toolchain
            WASM_GHC = "${ghc-wasm}/bin/wasm32-wasi-ghc";
            WASM_CABAL = "${ghc-wasm}/bin/wasm32-wasi-cabal";
            WASM_GHC_PKG = "${ghc-wasm}/bin/wasm32-wasi-ghc-pkg";

            # Ensure the wasm toolchain is first in PATH
            shellHookAfter = ''
              export PATH="${ghc-wasm}/bin:$PATH"
            '';
          };

          # API client only shell (no Wasm toolchain needed)
          api-client = pkgs.mkShell {
            name = "fastly-api-client-dev";

            buildInputs = [
              # Standard Haskell tools
              pkgs.ghc
              pkgs.cabal-install
              pkgs.haskell-language-server
              pkgs.ghcid

              # Build dependencies
              pkgs.pkg-config
              pkgs.zlib

              # Fastly CLI (for testing API operations)
              pkgs.fastly
            ];

            shellHook = ''
              echo "📦 Fastly API Client Development"
              echo ""
              echo "Build the API client:"
              echo "  cabal build fastly"
              echo "  cabal repl fastly"
              echo ""
            '';
          };

          # Compute-only shell with Wasm toolchain
          compute = pkgs.mkShell {
            name = "fastly-compute-dev";

            buildInputs = [
              # GHC WebAssembly toolchain
              ghc-wasm

              # Fastly Compute tools
              pkgs.fastly
              pkgs.viceroy

              # Build tools
              pkgs.cabal-install
              pkgs.pkg-config
            ];

            shellHook = ''
              echo "⚡ Fastly Compute@Edge Development"
              echo ""
              echo "Build and test:"
              echo "  cd compute-examples/hello-world"
              echo "  ./build.sh"
              echo "  viceroy bin/main.wasm"
              echo ""
            '';

            WASM_GHC = "${ghc-wasm}/bin/wasm32-wasi-ghc";
            WASM_CABAL = "${ghc-wasm}/bin/wasm32-wasi-cabal";

            shellHookAfter = ''
              export PATH="${ghc-wasm}/bin:$PATH"
            '';
          };
        };

        # Package outputs for the Haskell libraries
        packages = {
          # Note: Building Haskell packages with the Wasm toolchain in Nix
          # is complex. For now, we provide the development shells.
          # Users can build manually with cabal inside the shell.
          default = pkgs.writeShellScriptBin "fastly-dev" ''
            echo "Use 'nix develop' to enter the development environment"
            echo "Or 'nix develop .#api-client' for API client only"
            echo "Or 'nix develop .#compute' for Compute@Edge only"
          '';
        };

        # Apps that can be run with 'nix run'
        apps = {
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/fastly-dev";
          };
        };
      }
    );
}
