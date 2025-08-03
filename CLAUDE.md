# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Athena is a comprehensive Prolog engine with dual implementations in Scheme and Common Lisp. It supports multiple Scheme implementations (Racket, Gauche, Chez, Guile, Chibi, Sagittarius, Gambit, Chicken) and Common Lisp implementations (SBCL, ABCL, CLISP, ECL).

## Development Commands

### Testing
```bash
# Run all tests across all Scheme implementations
make all

# Test specific Scheme implementations
make test-racket      # Racket
make test-gauche      # Gauche with R7RS
make test-chicken     # Chicken Scheme with R7RS
make test-guile       # Guile with R7RS
make test-chibi       # Chibi Scheme
make test-sagittarius # Sagittarius Scheme
make test-chez        # Chez Scheme with R6RS
make test-gambit      # Gambit with R7RS

# Common Lisp testing
make test-sbcl        # SBCL
make test-ecl         # ECL  
make test-clisp       # CLISP
make test-abcl        # ABCL

# Or use ASDF directly (in REPL)
asdf:test-system :prolog
```

### Code Formatting
```bash
make format          # Format all source files using schemat
```

### Development Environment
```bash
devbox shell         # Enter Nix development shell with all implementations
nix develop          # Alternative Nix development shell
lefthook install     # Install git hooks (auto-runs after setup)
```

### Building and Cleanup
```bash
./script/build.sh    # Build Gambit version for web deployment
make clean           # Remove log files
```

## Architecture

### Core Components
- **`src/prolog-core.scm`**: Portable Scheme implementation of core engine
- **`src/prolog-lib.scm`**: Standard library predicates
- **`src/prolog.lisp`**: Common Lisp main package
- **`src/prolog/core.lisp`**: Common Lisp core engine
- **`src/prolog/primitive.lisp`**: Common Lisp built-in predicates
- **`src/prolog/stdlib.lisp`**: Common Lisp standard library

### Implementation Wrappers
- **`src/prolog.rkt`**: Racket-specific wrapper
- **`src/prolog.sld`**: R7RS library wrapper with conditional compilation
- **`src/prolog.sls`**: R6RS library wrapper

### Engine Features
- **Unification**: Variable binding and term unification
- **Backtracking**: Choice points using exceptions and continuations
- **Clause Database**: Dynamic predicate storage with arity-based indexing
- **Built-in Predicates**: Standard Prolog predicates (arithmetic, control flow, lists)
- **Debugging Support**: Spy/trace functionality

### Testing Framework
- **Scheme**: SRFI-64 with implementation-specific runners
  - `test/test.scm`: Main test suite
  - `test/test.7.scm`: R7RS runner
  - `test/test.6.scm`: R6RS runner
  - `test/test.rkt`: Racket-specific tests
- **Common Lisp**: FiveAM framework
  - `test/prolog/`: Modular test files with ASDF integration
  - Supports SBCL, ECL, CLISP, and ABCL implementations

## Key Development Patterns

### Cross-Implementation Compatibility
- Use `cond-expand` in R7RS wrapper for implementation-specific code
- Careful handling of differences between Scheme implementations
- Separate packages for Common Lisp components

### Automated Formatting
- Git hooks automatically format code using `schemat` before commits
- All Scheme/Lisp files are formatted consistently

### Testing Strategy
- All implementations must pass the same core test suite
- Implementation-specific test runners handle differences
- Common Lisp tests use FiveAM with custom utilities

## Common Development Tasks

### Adding New Built-in Predicates
1. Add to `src/prolog-lib.scm` for Scheme version
2. Add to `src/prolog/primitive.lisp` for Common Lisp version
3. Add tests to appropriate test files
4. Update documentation in README.md

### Running Single Tests
```bash
# Scheme (example with Racket)
racket -e '(require "test/test.rkt")' -e '(test-name)'

# Common Lisp (example with SBCL)
sbcl --eval "(require 'asdf)" --eval "(asdf:test-system :prolog)" --quit
```

### Web Deployment
The project includes web deployment capability through Gambit Scheme compilation to JavaScript. Use `./script/build.sh` to build the web version.