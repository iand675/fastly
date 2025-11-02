# fastly-compute

Haskell library for building [Fastly Compute@Edge](https://www.fastly.com/products/edge-compute/serverless) applications compiled to WebAssembly.

## Overview

This library provides everything you need to write type-safe Haskell applications that run at Fastly's global edge locations:

- ⚡ **Fast**: Sub-millisecond cold starts with WebAssembly
- 🔒 **Type-Safe**: Leverage Haskell's type system at the edge
- 🌍 **Global**: Deploy to 300+ edge locations worldwide
- 🚀 **Simple**: High-level API abstracts away FFI complexity

## Installation

### Prerequisites

#### Option 1: Using Nix (Recommended) 🎯

The easiest way to get all tools:

```bash
# From the repository root
nix develop

# Or just for Compute development
nix develop .#compute

# Everything is ready!
wasm32-wasi-ghc --version
```

See the [Nix Setup Guide](../../nix/README.md) for details.

#### Option 2: Manual Installation

Install GHC with WebAssembly support (9.6+):

```bash
# Install ghc-wasm toolchain
git clone https://gitlab.haskell.org/ghc/ghc-wasm-meta.git
cd ghc-wasm-meta
./setup.sh

# Add to PATH
export PATH="$HOME/.ghc-wasm/wasm32-wasi-ghc-9.10.1/bin:$PATH"
```

### Adding to Your Project

In your `cabal.project`:

```cabal
packages:
  .
  path/to/fastly-compute
```

Or in your package's `.cabal` file:

```cabal
build-depends:
  base,
  fastly-compute
```

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Fastly.Compute

main :: IO ()
main = runCompute $ \req -> do
  return $ Response
    { responseStatus  = 200
    , responseHeaders = [("Content-Type", "text/plain")]
    , responseBody    = "Hello from Haskell at the edge!"
    }
```

## Building for WebAssembly

Use `wasm32-wasi-ghc` instead of regular `ghc`:

```bash
# Configure for WebAssembly
wasm32-wasi-cabal configure

# Build
wasm32-wasi-cabal build

# The output will be a .wasm file
```

## API Reference

### Core Types

```haskell
-- HTTP Request from client
data Request = Request
  { requestMethod  :: ByteString
  , requestPath    :: ByteString
  , requestHeaders :: [(ByteString, ByteString)]
  , requestBody    :: ByteString
  }

-- HTTP Response to send
data Response = Response
  { responseStatus  :: Int
  , responseHeaders :: [(ByteString, ByteString)]
  , responseBody    :: ByteString
  }

-- Application handler type
type ComputeHandler = Request -> IO Response
```

### Main Entry Point

```haskell
runCompute :: ComputeHandler -> IO ()
```

Runs a Compute application by:
1. Retrieving the incoming HTTP request
2. Calling your handler function
3. Sending the response back to the client

### Low-Level FFI (Advanced)

For advanced use cases, `Fastly.Compute.FFI` provides direct access to Fastly hostcalls:

```haskell
import Fastly.Compute.FFI

-- Request operations
fastly_req_downstream_client_request :: ...
fastly_req_body_downstream_get :: ...

-- Response operations
fastly_resp_new :: ...
fastly_resp_status_set :: ...
fastly_resp_send_downstream :: ...

-- Body operations
fastly_http_body_new :: ...
fastly_http_body_write :: ...

-- Header operations
fastly_http_resp_header_insert :: ...
```

## Examples

### Simple HTML Response

```haskell
main = runCompute $ \req -> do
  return $ Response
    { responseStatus  = 200
    , responseHeaders = [("Content-Type", "text/html")]
    , responseBody    = "<h1>Hello, World!</h1>"
    }
```

### Routing Based on Path

```haskell
main = runCompute $ \req ->
  case requestPath req of
    "/hello" -> return $ Response 200
                  [("Content-Type", "text/plain")]
                  "Hello!"
    "/json"  -> return $ Response 200
                  [("Content-Type", "application/json")]
                  "{\"status\":\"ok\"}"
    _        -> return $ Response 404
                  [("Content-Type", "text/plain")]
                  "Not Found"
```

### With Error Handling

```haskell
import Control.Exception (try, SomeException)

main = runCompute $ \req -> do
  result <- try (processRequest req) :: IO (Either SomeException Response)
  case result of
    Right resp -> return resp
    Left err   -> return $ Response 500
                    [("Content-Type", "text/plain")]
                    ("Error: " <> BS.pack (show err))
```

## Architecture

```
┌─────────────────────────────┐
│   Your Application Code     │
│   (uses runCompute)         │
└──────────┬──────────────────┘
           │
           │ imports
           ▼
┌─────────────────────────────┐
│   Fastly.Compute API        │
│   (Request/Response types)  │
└──────────┬──────────────────┘
           │
           │ uses
           ▼
┌─────────────────────────────┐
│   Fastly.Compute.FFI        │
│   (Foreign imports)         │
└──────────┬──────────────────┘
           │
           │ runtime linkage
           ▼
┌─────────────────────────────┐
│   Fastly Runtime            │
│   (Wasmtime + hostcalls)    │
└─────────────────────────────┘
```

## How It Works

1. **Compilation**: Your Haskell code is compiled to WebAssembly using GHC's wasm32-wasi backend
2. **FFI**: Foreign Function Interface connects to Fastly's hostcalls (WASI imports)
3. **Runtime**: Fastly's Wasmtime-based runtime provides the hostcall implementations
4. **Execution**: Each HTTP request instantiates your Wasm module and calls `main`

## Current Features

✅ Basic HTTP request/response handling
✅ Setting status codes and headers
✅ Writing response bodies
✅ Error handling with exceptions
✅ Type-safe FFI bindings

## Roadmap

🚧 Full request parsing (method, URL, query params)
🚧 Backend HTTP requests
🚧 KV Store support
🚧 Config Store support
🚧 Geolocation API
🚧 Edge rate limiting

## Performance

- **Cold Start**: ~1ms (Fastly's Wasm instantiation)
- **Binary Size**: 5-15 MB (includes GHC runtime)
- **Memory Limit**: 128 MB per request
- **CPU Time Limit**: 50ms per request

## Testing

Use [Viceroy](https://github.com/fastly/Viceroy) for local testing:

```bash
# Build your app
wasm32-wasi-cabal build

# Find the .wasm file
find dist-newstyle -name "*.wasm"

# Test locally
viceroy path/to/your-app.wasm

# In another terminal
curl http://localhost:7878
```

## Deployment

```bash
# Install Fastly CLI
brew install fastly/tap/fastly

# Deploy
fastly compute publish
```

## Troubleshooting

### "wasm32-wasi-ghc: command not found"

Make sure the wasm toolchain is in your PATH:
```bash
export PATH="$HOME/.ghc-wasm/wasm32-wasi-ghc-9.10.1/bin:$PATH"
```

### FFI Linking Errors

Ensure the `include/` directory is in your include path and all header files are present.

### Large Binary Size

This is expected with GHC's runtime. Use `-O2` optimization and consider:
- Dead code elimination
- Stripping debug symbols
- Using `wasm-opt` for further optimization

## Contributing

Contributions welcome! This library needs:
- Additional hostcall bindings
- More complete request parsing
- Backend request support
- KV Store integration
- Better error messages
- More examples

## License

BSD-3-Clause

## Building Custom SDKs

This library is built using Fastly's Compute hostcalls, which are defined as `.witx` files in the Viceroy repository:

**Hostcalls Location**: https://github.com/fastly/Viceroy/tree/main/wasm_abi/compute-at-edge-abi

The `.witx` files define the WebAssembly interface for:
- `fastly_http_req` - HTTP request operations
- `fastly_http_resp` - HTTP response operations
- `fastly_http_body` - Body read/write operations
- Additional modules for KV store, backends, etc.

Our FFI bindings in `Fastly.Compute.FFI` provide Haskell interfaces to these hostcalls.

## Resources

- [Fastly Compute Documentation](https://developer.fastly.com/learning/compute/)
- [GHC WebAssembly Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- [Compute Hostcalls (WITX)](https://github.com/fastly/Viceroy/tree/main/wasm_abi/compute-at-edge-abi)
- [Compute ABI Repository](https://github.com/fastly/compute-at-edge-abi)
- [Example Applications](../../compute-examples/)

## See Also

- [fastly](../fastly/) - Fastly API client for managing CDN services
- [compute-examples](../../compute-examples/) - Example Compute applications
