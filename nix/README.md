# Nix Setup for Fastly Haskell Development

This directory contains Nix configuration for reproducible development environments.

## Quick Start

### Option 1: Using Flakes (Recommended)

```bash
# Enter the development environment
nix develop

# Or run a specific shell
nix develop .#api-client    # API client only
nix develop .#compute       # Compute@Edge only
nix develop .#default       # Full environment (default)
```

### Option 2: Using direnv (Automatic)

```bash
# Install direnv
# macOS: brew install direnv
# Linux: See https://direnv.net/

# Enable direnv for your shell
# Add to ~/.bashrc or ~/.zshrc:
eval "$(direnv hook bash)"  # or zsh

# Allow direnv in this directory
cd /path/to/fastly
direnv allow

# Now the environment loads automatically when you cd here!
```

### Option 3: Traditional shell.nix

```bash
# Full environment
nix-shell

# API client only (faster, no Wasm toolchain)
nix-shell --arg apiOnly true
```

## What's Included

### Full Development Environment (`nix develop`)

- **GHC WebAssembly Toolchain** (from ghc-wasm-meta)
  - `wasm32-wasi-ghc` - GHC compiler for WebAssembly
  - `wasm32-wasi-cabal` - Cabal build tool
  - `wasm32-wasi-ghc-pkg` - Package manager
  - `wasm32-wasi-hsc2hs` - FFI tool

- **Fastly Tools**
  - `fastly` - Fastly CLI for deployment
  - `viceroy` - Local Compute testing

- **Haskell Development**
  - `cabal-install` - Build tool
  - `haskell-language-server` - LSP server for editors
  - `ghcid` - Fast recompilation daemon

- **Build Tools**
  - `pkg-config`, `zlib`, `git`

### API Client Only (`nix develop .#api-client`)

Same as above but without the WebAssembly toolchain (faster to load).

### Compute Only (`nix develop .#compute`)

WebAssembly toolchain + Fastly tools, optimized for Compute development.

## Requirements

### For Flakes

Nix 2.4+ with flakes enabled:

```bash
# Check if flakes are enabled
nix flake metadata

# If not, enable in ~/.config/nix/nix.conf:
experimental-features = nix-command flakes
```

### For Traditional shell.nix

Any recent Nix installation.

## Architecture

### Flake Structure

```
flake.nix
├── inputs
│   ├── nixpkgs          # Nix packages
│   ├── ghc-wasm-meta    # GHC WebAssembly toolchain
│   └── flake-utils      # Multi-system support
└── outputs
    ├── devShells
    │   ├── default      # Full dev environment
    │   ├── api-client   # API client only
    │   └── compute      # Compute only
    └── packages
        └── default      # Helper script
```

### GHC WebAssembly from ghc-wasm-meta

The flake uses `ghc-wasm-meta` as an input, which provides:

- Pre-built GHC binaries with WebAssembly support
- wasm32-wasi cross-compilation tools
- Required WASM libraries (wasi-libc, etc.)
- LLVM with WebAssembly backend

This is much easier than building GHC from source or manually setting up the toolchain.

## Usage Examples

### Build the API Client

```bash
# Enter environment
nix develop .#api-client

# Build
cd packages/fastly
cabal build
cabal test
```

### Build Compute Example

```bash
# Enter environment
nix develop

# Build WebAssembly
cd compute-examples/hello-world
./build.sh

# Test locally
viceroy bin/main.wasm

# Test with curl
curl http://localhost:7878
```

### Editor Integration

#### VS Code with direnv

1. Install direnv: `brew install direnv`
2. Add to shell config: `eval "$(direnv hook bash)"`
3. Install VS Code extension: `mkhl.direnv`
4. Allow direnv: `direnv allow`
5. Reload VS Code

The environment (including `haskell-language-server`) loads automatically!

#### Emacs with direnv

```elisp
;; Add to your config
(use-package direnv
  :config
  (direnv-mode))
```

#### Vim/Neovim with direnv

```vim
" Add to your config
Plug 'direnv/direnv.vim'
```

## Troubleshooting

### "experimental features" error

Enable flakes:
```bash
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

### "ghc-wasm-meta not available"

This is expected with `shell.nix` (non-flake mode). Use flakes instead:
```bash
nix develop
```

Or manually install ghc-wasm-meta following:
https://gitlab.haskell.org/ghc/ghc-wasm-meta

### Slow first build

The first time you run `nix develop`, it downloads the GHC WebAssembly toolchain (~500MB).
This is cached for future use.

### Cache issues

Clear the Nix store entry for ghc-wasm-meta:
```bash
nix store delete /nix/store/*ghc-wasm-meta*
nix develop --refresh
```

## Advanced Usage

### Pin to Specific GHC Version

Edit `flake.nix`:

```nix
ghc-wasm-meta = {
  url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org&ref=ghc-9.10";
  # ... or pin to a specific commit
  # url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org&rev=abc123";
};
```

### Update Dependencies

```bash
nix flake update
```

### Build in Pure Mode

```bash
nix develop --pure
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Build
on: [push]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: cachix/install-nix-action@v22
        with:
          nix_path: nixpkgs=channel:nixos-unstable
          extra_nix_config: |
            experimental-features = nix-command flakes
      - run: nix develop --command cabal build all
```

### GitLab CI

```yaml
build:
  image: nixos/nix:latest
  script:
    - nix develop --command cabal build all
```

## Resources

- [Nix Flakes](https://nixos.wiki/wiki/Flakes)
- [direnv](https://direnv.net/)
- [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta)
- [Fastly CLI](https://developer.fastly.com/reference/cli/)
- [Viceroy](https://github.com/fastly/Viceroy)
