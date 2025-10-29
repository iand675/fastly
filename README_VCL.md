# Fastly VCL Parser, Renderer, and Validator

A comprehensive Haskell library for working with Fastly VCL (Varnish Configuration Language).

## Features

✅ **Full VCL Parsing** - Parse VCL code into a strongly-typed AST using megaparsec
✅ **Pretty Rendering** - Render VCL AST back to formatted code using prettyprinter
✅ **Programmatic Generation** - Build VCL using a convenient builder API with operators
✅ **Semantic Validation** - Comprehensive validation beyond syntax checking
✅ **Type Safety** - Strongly typed AST ensures correctness at compile time
✅ **Round-trip Support** - Parse and render while preserving semantics

## Quick Start

```haskell
import Network.Fastly.VCL

-- Parse VCL
let vclCode = "sub vcl_recv { set req.http.Host = \"example.com\"; return(pass); }"
case parseVCL vclCode of
  Left err -> print err
  Right vcl -> do
    -- Validate
    case validateVCL vcl of
      Left errors -> mapM_ print errors
      Right _ -> do
        -- Render
        putStrLn $ renderVCL vcl
```

## What Can Be Validated?

### ✓ Variable Scope
- Ensures variables are used in correct subroutine contexts
- Example: `req.*` only in `vcl_recv`, `resp.*` only in `vcl_deliver`
- Prevents writing to read-only variables like `client.ip`

### ✓ Type Checking
- Validates type compatibility in all operations
- Ensures boolean conditions in if statements
- Checks type compatibility in assignments
- Validates function argument types

### ✓ Reference Validation
- Detects undefined subroutine calls
- Validates backend references exist
- Checks ACL references
- Prevents duplicate definitions

### ✓ Control Flow
- Detects unreachable code after return/error/restart
- Ensures proper return statements in predefined subroutines
- Validates return actions for each subroutine context

### ✓ Return Actions
- `vcl_recv`: lookup, pass, pipe, error, synth, hash
- `vcl_deliver`: deliver, restart
- `vcl_fetch`: deliver, deliver_stale, restart, error
- And more...

## Examples

### Example 1: Parse and Validate

```haskell
let vclCode = unlines
      [ "backend origin {"
      , "  .host = \"example.com\";"
      , "}"
      , "sub vcl_recv {"
      , "  set req.http.Host = \"example.com\";"
      , "  return(pass);"
      , "}"
      ]

case parseVCL vclCode >>= validateVCL of
  Left errors -> mapM_ print errors
  Right vcl -> putStrLn "Valid VCL!"
```

### Example 2: Generate VCL Programmatically

```haskell
let vcl = VCL
      [ subroutine VclRecv
        [ ifStmt (var ["req", "http", "Host"] .==. stringLit "example.com")
          [ setVar ["req", "backend"] (var ["origin"])
          , returnWith "pass"
          ]
        , returnWith "lookup"
        ]
      ]

-- Validate before rendering
case validateVCL vcl of
  Right _ -> putStrLn $ renderVCL vcl
  Left errors -> mapM_ print errors
```

### Example 3: Catch Common Errors

```haskell
-- Error: Wrong variable context
let badVCL1 = "sub vcl_deliver { set req.http.Host = \"x\"; }"
-- ValidationError: InvalidVariableContext (Variable ["req","http","Host"]) DeliverContext

-- Error: Type mismatch
let badVCL2 = "sub vcl_recv { if (42) { return(pass); } }"
-- ValidationError: TypeMismatch TBool TInteger "if condition"

-- Error: Invalid return action
let badVCL3 = "sub vcl_recv { return(deliver); }"
-- ValidationError: InvalidReturnAction (Identifier "deliver") RecvContext

-- Error: Unreachable code
let badVCL4 = "sub vcl_recv { return(pass); log \"unreachable\"; }"
-- ValidationError: UnreachableCode
```

## Modules

- `Network.Fastly.VCL` - Main module with all functionality
- `Network.Fastly.VCL.Types` - AST data types
- `Network.Fastly.VCL.Parser` - Parser implementation
- `Network.Fastly.VCL.Pretty` - Pretty-printer
- `Network.Fastly.VCL.Validation` - Semantic validation

## Builder API

Convenient operators for building VCL:

```haskell
-- Comparisons
(.==.), (./=.), (.<.), (.<=.), (.>.), (.>=.)

-- Logical
(.&&.), (.||.), notExpr

-- Arithmetic
(.+.), (.-.), (.*.), (./.), (.%.)

-- Regex
(.~.), (.!~.)

-- Literals
stringLit, intLit, floatLit, boolLit, durationLit

-- Variables
var ["req", "http", "Host"]

-- Statements
setVar, unsetVar, ifStmt, ifElse, returnWith, callSub, logStmt
```

## Validation Error Types

```haskell
data ValidationError
  = UndefinedVariable Variable
  | InvalidVariableContext Variable SubroutineContext
  | ReadOnlyVariable Variable
  | TypeMismatch VCLType VCLType Text
  | UndefinedSubroutine SubroutineName
  | UndefinedBackend Identifier
  | UndefinedACL Identifier
  | InvalidReturnAction Identifier SubroutineContext
  | DuplicateDefinition Text
  | InvalidOperation Text
  | UnreachableCode
  | MissingReturn SubroutineName
```

## Testing

Comprehensive test suites included:

```bash
cabal test
```

Tests cover:
- Parser correctness
- Pretty-printer formatting
- Round-trip parse/render
- Builder API
- All validation rules
- Edge cases and error conditions

## Documentation

Full documentation available in `docs/VCL.md` including:
- Complete API reference
- Usage examples
- Variable scope rules
- Return action compatibility
- Type system details
- Common patterns

## Examples

See `examples/` directory:
- `VCLExample.hs` - Parsing, rendering, and generation
- `ValidationExample.hs` - Validation features and error handling

## Requirements

- GHC >= 9.4
- megaparsec >= 9.0
- prettyprinter >= 1.7
- containers >= 0.6

## Installation

```cabal
build-depends: fastly
```

## Why Validate?

VCL code that parses correctly can still have semantic errors:

- Using `req.*` variables in `vcl_deliver` (not available)
- Returning `deliver` from `vcl_recv` (invalid action)
- Type errors like `if (42)` instead of `if (true)`
- Calling undefined subroutines
- Code after return statements that will never execute

The validation system catches these errors before deployment!

## License

BSD3 - See LICENSE file

## Author

Ian Duncan (ian@iankduncan.com)

## Contributing

Contributions welcome! Please:
1. Add tests for new features
2. Update documentation
3. Follow existing code style
4. Ensure validation rules are consistent with Fastly VCL semantics
