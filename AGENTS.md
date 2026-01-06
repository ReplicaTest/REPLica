<!-- markdownlint-disable -->

# AGENTS.md - REPLica Development Guide

This document provides essential information for agentic coding assistants working on the REPLica codebase.

## Project Overview

REPLica is a CLI golden testing framework written in Idris2. It allows testing command-line interfaces by comparing actual output against "golden values" (expected outputs stored in files). The project supports test organization via suites and tags, test dependencies, and file-based expectations.

## Build & Test Commands

### Prerequisites

- **Idris2** v0.8.0 or later
- **Dhall** and **dhall-to-json** (for configuration processing)
- **Git** (for version control)
- Recommended: **nix flakes** for reproducible development environment

### Core Commands

```bash
# Full development setup
nix develop                    # Drop into dev environment with all dependencies

# Build the project
make build                     # Compile Idris2 project -> build/exec/replica

# Install locally
make install                   # Copy replica binary to ~/.local/bin/

# Run all tests (using current build)
make test                      # Runs all tests from tests.dhall against current version

# Update test golden values
make generate                  # Run tests in interactive mode to update expectations

# Clean build artifacts
make clean                     # Remove build/ directory
make clean-test               # Remove generated .json test files

# Run tests with stable replica version
REPLICA_EXE=/path/to/replica make test    # Use specific replica binary

# Run nix checks (linting, testing)
nix flake check               # Complete CI-like validation
```

### Running a Single Test

```bash
# Generate JSON from Dhall test file
dhall-to-json --file tests/replica/simple/tests.dhall --output tests/replica/simple/tests.json

# Run specific test file
./build/exec/replica run tests/replica/simple/tests.json

# Run with filtering
./build/exec/replica run -t tag_name tests/replica/simple/tests.json    # Include tests with tag
./build/exec/replica run -T tag_name tests/replica/simple/tests.json    # Exclude tests with tag
./build/exec/replica run -s suite_name tests/replica/simple/tests.json  # Run specific suite

# Interactive mode (updates golden values)
./build/exec/replica run --interactive tests/replica/simple/tests.json

# Get help
./build/exec/replica help
```

## Code Structure

```
src/Replica/
├── Replica.idr              # Entry point, arg parsing, exit codes
├── App/                     # Application logic (run, info, new, set commands)
├── Command/                 # Command parsers and handlers
├── Core/                    # Core types and test parsing
├── Option/                  # CLI option parsing and filtering
└── Other/                   # Utilities (Validation, Decorated, Free, String)

tests/                       # Test files
├── replica/                 # Replica's own test suite (Dhall)
└── Meta/                    # Common test utilities and types

.replica/test/               # Golden test expectations and outputs
```

## Code Style Guidelines

### Language & Type System

- **Language**: Idris2 (functional with dependent types)
- **Default Totality**: `%default total` is used in most modules
  - Functions must be proven total (terminating, covering all cases)
  - Use `covering` for partial functions (with explicit justification)
  - Pattern matching must be exhaustive
- **Type Safety**: Leverage the type system for correctness (use `So`, dependent pairs, etc.)

### Module Organization

1. **Top-level comments**: Use triple-pipe comments (`|||`) for module documentation
2. **Imports**: Group by purpose (standard library → local)
   ```idris
   module Replica.Core.Types
   
   import Data.String
   import Data.List
   import Language.JSON
   
   import Replica.Core.Types
   ```
3. **Exports**: Use `public export` for public API, `export` for helper functions
4. **Module re-exports**: Use `import public` to expose submodule APIs (see `Replica.App`)

### Naming Conventions

- **Types/Records**: `CamelCase` (e.g., `Test`, `Expectation`, `ReplicaError`)
- **Functions**: `camelCase` (e.g., `parseArgs`, `runTest`, `displayExit`)
- **Type constructors**: `CamelCase` (e.g., `Ordered`, `StdOut`, `Generated`)
- **Record fields**: `camelCase` with dot notation (e.g., `test.name`, `ctx.global`)
- **Module names**: `CamelCase` with dot hierarchy (e.g., `Replica.App.Run`)

### Type Definitions

Use **record syntax** for complex data with named fields:
```idris
record Test where
  constructor MkTest
  name: String
  pending : Bool
  description: Maybe String
  -- ... other fields
```

Use **union types** (sum types) for variants:
```idris
data Expectation
   = Exact String
   | StartsWith String
   | Partial OrderSensitive (List String)
   | Generated
```

Use **dependent types** to encode invariants (e.g., `So (not $ n == 0)` ensures non-zero).

### Error Handling

1. **Validation Accumulation**: Use `Validation err a` from `Replica.Other.Validation` for collecting multiple errors:
   ```idris
   validateField : String -> Validation (List String) Field
   validateField x = if valid x 
     then Valid (field x)
     else Error ["Field invalid"]
   ```

2. **Custom Error Types**: Define error sum types and implement `Show`:
   ```idris
   data ReplicaError = CantAccessTestFile String | InvalidJSON (List String)
   
   Show ReplicaError where
     show (CantAccessTestFile s) = "Can't access: \{s}"
     show (InvalidJSON errs) = "JSON errors: \{show errs}"
   ```

3. **Exit Codes**: Map errors to explicit exit codes in `exitCode` functions (see `Replica.idr`)

4. **Error Propagation**: Use `Either` and `Validation` for recoverable errors; reserve exceptions for unrecoverable failures

### Formatting & Style

- **Line length**: Target 100 characters (soft limit)
- **Indentation**: 2 spaces (consistent throughout codebase)
- **Comments**: Use triple-pipe (`|||`) for top-level documentation, `--` for inline
- **String interpolation**: Use `\{var}` syntax for embedding values
- **Whitespace**: Single blank lines between definitions, double for major sections

### API Conventions

1. **Function composition**: Chain operations with `>>`, `<*>`, etc. when appropriate
2. **Record updates**: Use `{field := value}` syntax for non-destructive updates
3. **Effect handling**: Use `Control.App` monads (see `runReplica` in `Replica.idr`)
4. **First-class handlers**: Use continuation-passing with `handle` for effect handling

### Test Writing

- Use **Dhall** for test specifications (see `tests.dhall`)
- Convert Dhall to JSON with `dhall-to-json` before running
- Use **golden file mode** (`Generated True`) for output comparison
- Structure complex expectations with ordered/unordered fragments:
  ```dhall
  stdOut = Replica.Expectation ::
    { consecutive = ["hello", "world"]
    , end = Some "!"
    }
  ```

## Key Files to Know

- `Replica.idr`: Main entry point, arg parsing, exit handling
- `Replica/Core/Types.idr`: Core data structures (`Test`, `Expectation`, `Part`)
- `Replica/Core/Parse.idr`: JSON parsing for test specifications
- `Replica/App/Run.idr`: Test execution logic
- `Replica/Option/Parse.idr`: CLI argument parsing
- `Replica/Other/Validation.idr`: Error accumulation pattern
- `tests.dhall`: Primary test suite definition
- `Makefile`: Build and test automation

## Important Patterns

1. **Total functions**: Always prove totality unless using `covering` with explicit coverage
2. **String handling**: Use `String` type; leverage `Data.String` utilities
3. **Applicative validation**: Combine validators to collect all errors at once
4. **Effect monad**: Use `Control.App` for I/O operations with structured error handling
5. **Custom operators**: Sparingly; prefer explicit function names for clarity

## Running Checks Before Committing

```bash
make clean build test        # Full clean build + test
nix flake check              # Comprehensive CI checks
```

Ensure all tests pass with the current version before committing.

## References

- [Idris2 Documentation](https://idris2.readthedocs.io/)
- [REPLica User Guide](./README.md)
- [Test Specification](./documentation/TestSpecification.md)
- [Contributing Guidelines](./CONTRIBUTING.md)
