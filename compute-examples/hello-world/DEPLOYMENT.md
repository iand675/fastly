# Deployment Guide for Fastly Compute Haskell Applications

This guide explains how to deploy your Haskell WebAssembly application to Fastly Compute@Edge.

## Overview

Fastly Compute@Edge requires Wasm binaries to be packaged with specific metadata before deployment. For custom language implementations like this Haskell SDK, the workflow is:

1. **Build**: Compile Haskell to WebAssembly (`wasm32-wasi` target)
2. **Package**: Use `fastly compute pack` to add Fastly metadata
3. **Deploy**: Upload to Fastly with `fastly compute deploy`
4. **Test**: Use `fastly compute serve` for local testing

## Prerequisites

- Fastly CLI installed (`brew install fastly/tap/fastly`)
- Fastly account with API token
- Built WebAssembly binary (`bin/main.wasm`)

## Build and Package

Our build script (`build.sh`) handles both compilation and packaging:

```bash
./build.sh
```

This:
1. Compiles Haskell to WebAssembly with `wasm32-wasi-ghc`
2. Runs `fastly compute pack --wasm-binary bin/main.wasm`
3. Creates a deployable package with Fastly metadata

### Manual Packaging

If you build manually, package the Wasm binary:

```bash
# After building your .wasm file
fastly compute pack --wasm-binary bin/main.wasm
```

This creates a `.tar.gz` package ready for deployment.

## Local Testing

### Option 1: Using Fastly CLI (Recommended)

```bash
fastly compute serve
```

This:
- Starts Viceroy local testing server
- Loads your packaged application
- Serves on http://localhost:7676

### Option 2: Using Viceroy Directly

```bash
viceroy bin/main.wasm
```

This runs just the Wasm binary without packaging, on http://localhost:7878.

### Testing Your Application

```bash
# Test the service
curl http://localhost:7676  # or :7878 if using viceroy

# You should see the HTML response!
```

## Deployment

### Initial Setup

1. **Authenticate with Fastly**:
   ```bash
   fastly profile create
   # Or use an API token:
   fastly auth token
   ```

2. **Create a Service** (first time only):
   ```bash
   fastly compute publish
   # This will create a new service and deploy
   ```

### Deploying Updates

```bash
fastly compute deploy
```

This:
- Uploads your packaged application
- Creates a new version
- Activates it on the Fastly edge network

### Deployment Options

```bash
# Deploy to a specific service
fastly compute deploy --service-id YOUR_SERVICE_ID

# Deploy without activating (for testing)
fastly compute deploy --version-skip-activation

# Deploy with a comment
fastly compute deploy --version-comment "Fixed bug in routing"
```

## Workflow Examples

### Development Workflow

```bash
# 1. Make changes to src/Main.hs
vim src/Main.hs

# 2. Build and package
./build.sh

# 3. Test locally
fastly compute serve

# 4. Test with curl
curl http://localhost:7676

# 5. Deploy when ready
fastly compute deploy
```

### CI/CD Workflow

```yaml
# GitHub Actions example
name: Deploy to Fastly
on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - uses: cachix/install-nix-action@v22
        with:
          extra_nix_config: |
            experimental-features = nix-command flakes

      - name: Build
        run: |
          nix develop --command bash -c "cd compute-examples/hello-world && ./build.sh"

      - name: Deploy
        env:
          FASTLY_API_TOKEN: ${{ secrets.FASTLY_API_TOKEN }}
        run: |
          cd compute-examples/hello-world
          fastly compute deploy --service-id ${{ secrets.FASTLY_SERVICE_ID }}
```

## Service Management

### View Service Info

```bash
# List your services
fastly service list

# Get service details
fastly service describe --service-id YOUR_SERVICE_ID

# View service versions
fastly service-version list --service-id YOUR_SERVICE_ID
```

### Viewing Logs

```bash
# Real-time log tailing
fastly log-tail --service-id YOUR_SERVICE_ID

# Or use the UI at https://manage.fastly.com
```

### Monitoring

```bash
# View real-time stats
fastly stats realtime --service-id YOUR_SERVICE_ID

# Historical stats
fastly stats historical --service-id YOUR_SERVICE_ID
```

## Troubleshooting

### Build Issues

**Problem**: `wasm32-wasi-ghc not found`
```bash
# Solution: Enter Nix environment
nix develop
# Or add to PATH manually
export PATH="$HOME/.ghc-wasm/wasm32-wasi-ghc-9.10.1/bin:$PATH"
```

**Problem**: Package creation fails
```bash
# Solution: Ensure fastly.toml is valid
cat fastly.toml
# manifest_version = 3
# name = "your-service-name"
# language = "other"
```

### Deployment Issues

**Problem**: Authentication fails
```bash
# Solution: Create or refresh token
fastly auth token
# Enter your API token from https://manage.fastly.com/account/personal/tokens
```

**Problem**: Service not found
```bash
# Solution: Create service first
fastly compute publish  # Creates new service
# OR specify existing service
fastly compute deploy --service-id YOUR_SERVICE_ID
```

### Runtime Issues

**Problem**: Application crashes or returns 500
```bash
# Solution: Test locally first
fastly compute serve
# Check logs for errors

# Common issues:
# - Missing FFI imports (check Fastly.Compute.FFI)
# - Exception in handler (check error handling)
# - Invalid response format
```

**Problem**: Wasm instantiation fails
```bash
# Solution: Verify Wasm binary
wasm-objdump -h bin/main.wasm
# Check for required imports

# Ensure using wasm32-wasi target (not wasm32-unknown-unknown)
```

## Understanding the Package Structure

When you run `fastly compute pack`, it creates:

```
pkg/
├── fastly.toml           # Service configuration
├── bin/
│   └── main.wasm        # Your Wasm binary
└── .tar.gz              # Deployable archive
```

This package contains:
- Your compiled Wasm binary
- Service metadata from `fastly.toml`
- Manifest for Fastly's deployment system

## Custom Build Integration

If you're not using our `build.sh`:

```bash
# 1. Compile to Wasm
wasm32-wasi-cabal build your-app

# 2. Find the output
find dist-newstyle -name "*.wasm"

# 3. Copy to bin/
cp path/to/app.wasm bin/main.wasm

# 4. Package
fastly compute pack --wasm-binary bin/main.wasm

# 5. Deploy
fastly compute deploy
```

## Resources

- [Fastly Compute Documentation](https://developer.fastly.com/learning/compute/)
- [Fastly CLI Reference](https://developer.fastly.com/reference/cli/)
- [Compute Hostcalls](https://github.com/fastly/Viceroy/tree/main/wasm_abi/compute-at-edge-abi)
- [Viceroy Local Testing](https://github.com/fastly/Viceroy)

## Getting Help

- **Fastly Community**: https://community.fastly.com/
- **GitHub Issues**: https://github.com/iand675/fastly/issues
- **Documentation**: https://developer.fastly.com/

Remember: This is a custom SDK not officially supported by Fastly. For production use, consider the official Rust, JavaScript, or Go SDKs.
