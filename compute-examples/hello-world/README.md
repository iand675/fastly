# Haskell on Fastly Compute@Edge

This is a working example of running Haskell code on Fastly's Compute@Edge platform, compiled to WebAssembly using GHC's wasm32-wasi backend.

## Overview

This project demonstrates:

- ✅ Compiling Haskell to WebAssembly (wasm32-wasi target)
- ✅ FFI bindings to Fastly Compute hostcalls
- ✅ High-level Haskell API for request/response handling
- ✅ Complete build toolchain and deployment workflow

## Architecture

```
┌─────────────────┐
│  Haskell Code   │
│   (Main.hs)     │
└────────┬────────┘
         │
         │ imports
         ▼
┌─────────────────┐
│  High-level API │
│(Fastly.Compute) │
└────────┬────────┘
         │
         │ uses
         ▼
┌─────────────────┐
│   FFI Bindings  │
│    (FFI.hs)     │
└────────┬────────┘
         │
         │ foreign import
         ▼
┌─────────────────┐
│ Fastly Hostcalls│
│ (Wasm Imports)  │
└─────────────────┘
```

## Prerequisites

### Option 1: Using Nix (Easiest) 🎯

From the repository root:

```bash
# Enter development environment (includes everything!)
nix develop

# Verify tools are available
wasm32-wasi-ghc --version
fastly version
viceroy --version

# You're ready to build!
cd compute-examples/hello-world
./build.sh
```

See the [Nix Setup Guide](../../nix/README.md) for details.

### Option 2: Manual Installation

#### 1. Install GHC with WebAssembly Support

GHC's WebAssembly backend is available from GHC 9.6+. The easiest way to set it up is using `ghc-wasm-meta`:

```bash
# Clone the setup repository
git clone https://gitlab.haskell.org/ghc/ghc-wasm-meta.git
cd ghc-wasm-meta

# Run the setup script (this will take a while)
./setup.sh

# Add the wasm toolchain to your PATH
export PATH="$HOME/.ghc-wasm/wasm32-wasi-ghc-9.10.1/bin:$PATH"
```

This provides:
- `wasm32-wasi-ghc` - GHC compiler for WebAssembly
- `wasm32-wasi-cabal` - Cabal build tool for WebAssembly
- `wasm32-wasi-ghc-pkg` - Package manager for WebAssembly packages

#### 2. Install Fastly CLI and Viceroy

```bash
# Install Fastly CLI
brew install fastly/tap/fastly

# Or using npm
npm install -g @fastly/cli

# Install Viceroy (local Compute testing)
cargo install viceroy
```

#### 3. Verify Installation

```bash
# Check that wasm32-wasi-ghc is available
wasm32-wasi-ghc --version

# Check Fastly CLI
fastly version

# Check Viceroy
viceroy --version
```

## Building

### Option 1: Using the Build Script

```bash
./build.sh
```

This will:
1. Clean previous builds
2. Configure Cabal for wasm32-wasi
3. Build the WebAssembly module
4. Copy the output to `bin/main.wasm`

### Option 2: Manual Build

```bash
# Configure
wasm32-wasi-cabal configure \
    --with-compiler=wasm32-wasi-ghc \
    --with-hc-pkg=wasm32-wasi-ghc-pkg \
    --with-hsc2hs=wasm32-wasi-hsc2hs

# Build
wasm32-wasi-cabal build

# Find the output
find dist-newstyle -name "*.wasm"
```

## Testing Locally

Use Viceroy to test your Compute application locally:

```bash
# Run with Viceroy
viceroy bin/main.wasm

# In another terminal, test with curl
curl http://localhost:7878
```

You should see the HTML "Hello, World!" response!

## Deploying to Fastly

### 1. Create a Compute Service

```bash
# Login to Fastly
fastly auth token

# Create a new service
fastly compute init
```

### 2. Deploy

```bash
# Deploy the service
fastly compute publish

# Your service will be available at:
# https://your-service.edgecompute.app
```

## Project Structure

```
.
├── src/
│   └── Main.hs                  # Application entry point
├── bin/
│   └── main.wasm                # Compiled WebAssembly module
├── fastly-compute-hello.cabal   # Cabal build configuration
├── fastly.toml                  # Fastly service configuration
├── build.sh                     # Build script
└── README.md                    # This file

# The Compute library is in the fastly-compute package:
../../packages/fastly-compute/
├── src/
│   └── Fastly/
│       ├── Compute.hs           # High-level API
│       └── Compute/
│           └── FFI.hs           # Low-level FFI bindings
├── include/
│   ├── fastly_req.h             # Request hostcall declarations
│   ├── fastly_resp.h            # Response hostcall declarations
│   └── fastly_body.h            # Body hostcall declarations
└── fastly-compute.cabal         # Library package configuration
```

## How It Works

### 1. FFI Bindings (`Fastly.Compute.FFI`)

This module provides low-level Foreign Function Interface (FFI) bindings to Fastly's WebAssembly hostcalls:

```haskell
foreign import capi unsafe "fastly_resp.h fastly_resp_new"
  fastly_resp_new :: Ptr ResponseHandle -> IO FastlyStatus
```

These bindings use the `CApiFFI` extension, which requires C header files that declare the function signatures.

### 2. High-Level API (`Fastly.Compute`)

A type-safe, idiomatic Haskell interface:

```haskell
data Response = Response
  { responseStatus  :: Int
  , responseHeaders :: [(ByteString, ByteString)]
  , responseBody    :: ByteString
  }

runCompute :: (Request -> IO Response) -> IO ()
```

### 3. Application Code (`Main.hs`)

Your application logic:

```haskell
main :: IO ()
main = runCompute $ \req -> do
  return $ Response 200 [("Content-Type", "text/html")] "<h1>Hello!</h1>"
```

### 4. Compilation to WebAssembly

GHC compiles the Haskell code to WebAssembly:
- Target: `wasm32-wasi` (WebAssembly with WASI support)
- Entry point: Haskell `main` becomes WASI `_start`
- FFI imports are resolved by Fastly's runtime
- RTS (Runtime System) is included in the Wasm module

## API Reference

### Request Type

```haskell
data Request = Request
  { requestMethod  :: ByteString
  , requestPath    :: ByteString
  , requestHeaders :: [(ByteString, ByteString)]
  , requestBody    :: ByteString
  }
```

### Response Type

```haskell
data Response = Response
  { responseStatus  :: Int                        -- HTTP status code
  , responseHeaders :: [(ByteString, ByteString)] -- Response headers
  , responseBody    :: ByteString                 -- Response body
  }
```

### Handler Function

```haskell
type ComputeHandler = Request -> IO Response

runCompute :: ComputeHandler -> IO ()
```

## Troubleshooting

### Build Issues

**Problem**: `wasm32-wasi-ghc: command not found`
- **Solution**: Make sure you've added the wasm toolchain to your PATH:
  ```bash
  export PATH="$HOME/.ghc-wasm/wasm32-wasi-ghc-9.10.1/bin:$PATH"
  ```

**Problem**: FFI linking errors
- **Solution**: Ensure the `include/` directory contains all header files and is in the include path

### Runtime Issues

**Problem**: "Unknown import" errors with Viceroy
- **Solution**: This might indicate a mismatch between the hostcalls you're using and what Viceroy supports. Check the Viceroy version.

**Problem**: Large .wasm file size
- **Solution**: Use optimization flags: `-O2` and consider dead code elimination

## Performance Considerations

- **Cold Start**: Fastly Compute has very fast cold starts (~1ms) even for Haskell
- **Binary Size**: Expect 5-15 MB for a simple app (includes GHC RTS)
- **Memory**: Compute provides 128 MB per request
- **Execution Time**: 50ms CPU time limit per request

## Current Limitations

This is a proof-of-concept implementation. Current limitations:

1. **Request Parsing**: The `getDownstreamRequest` function returns a minimal request. A full implementation would need to:
   - Parse the request URL and method
   - Extract all headers
   - Read the request body

2. **Backend Requests**: Not yet implemented. You'll need to add FFI bindings for:
   - `fastly_req_send`
   - Backend configuration

3. **Error Handling**: Basic error handling is implemented, but could be improved

4. **Additional Features**: Many Fastly Compute features not yet exposed:
   - KV Store
   - Config Store
   - Geolocation
   - Logging
   - Edge rate limiting

## Extending This Example

### Adding Request Parsing

To parse the full request, you'll need to add FFI bindings for:

```haskell
-- Get request method
foreign import capi unsafe "fastly_req.h fastly_req_method_get"
  fastly_req_method_get
    :: RequestHandle -> Ptr Word8 -> CSize -> Ptr CSize -> IO FastlyStatus

-- Get request URI
foreign import capi unsafe "fastly_req.h fastly_req_uri_get"
  fastly_req_uri_get
    :: RequestHandle -> Ptr Word8 -> CSize -> Ptr CSize -> IO FastlyStatus
```

### Adding Backend Support

For making requests to backend origins:

```haskell
foreign import capi unsafe "fastly_req.h fastly_req_send"
  fastly_req_send
    :: RequestHandle -> BodyHandle -> Ptr Word8 -> CSize
    -> Ptr ResponseHandle -> Ptr BodyHandle -> IO FastlyStatus
```

## Resources

- [Fastly Compute Documentation](https://developer.fastly.com/learning/compute/)
- [GHC WebAssembly Backend Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [Fastly Compute ABI Specification](https://github.com/fastly/compute-at-edge-abi)
- [Viceroy Local Testing](https://github.com/fastly/Viceroy)

## Contributing

This is an experimental project. Contributions are welcome!

Potential improvements:
- Complete request parsing implementation
- Backend request support
- KV Store bindings
- Better error messages
- Smaller binary sizes
- More examples

## License

BSD-3-Clause License - see LICENSE file

## Acknowledgments

- Fastly for the Compute@Edge platform
- GHC team for the WebAssembly backend
- The Haskell community
