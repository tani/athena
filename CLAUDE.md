# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Athena is a comprehensive Prolog engine with dual implementations in Scheme and Common Lisp. The project is organized into separate language-specific directories:

- `scheme/` - Scheme implementation supporting multiple implementations (Racket, Gauche, Chez, Guile, Chibi, Sagittarius, Gambit, Chicken)
- `common-lisp/` - Common Lisp implementation supporting multiple implementations (SBCL, ABCL, CLISP, ECL)

## Development Commands

### Testing
```bash
# Run all tests across all implementations (Scheme and Common Lisp)
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
nix develop          # Nix development shell with all dependencies
lefthook install     # Install git hooks (auto-runs after setup)
```

### Building and Cleanup
```bash
./script/build.sh    # Build Gambit version for web deployment
make clean           # Remove log files
```

## Architecture

### Scheme Implementation (`scheme/`)
**Core Components:**
- **`scheme/src/core.scm`**: Portable Scheme implementation of core engine
- **`scheme/src/primitive.scm`**: Built-in predicates and meta-predicates
- **`scheme/src/stdlib.scm`**: Standard library clauses

**Implementation Wrappers:**
- **`scheme/src/prolog.rkt`**: Racket-specific wrapper
- **`scheme/src/prolog.sld`**: R7RS library wrapper with conditional compilation
- **`scheme/src/prolog.sls`**: R6RS library wrapper

### Common Lisp Implementation (`common-lisp/`)
**Core Components:**
- **`common-lisp/src/core.lisp`**: Common Lisp core engine
- **`common-lisp/src/primitive.lisp`**: Common Lisp built-in predicates
- **`common-lisp/src/stdlib.lisp`**: Common Lisp standard library
- **`common-lisp/src/all.lisp`**: Main package combining all components
- **`common-lisp/prolog.asd`**: ASDF system definition

### Engine Features
- **Unification**: Variable binding and term unification
- **Backtracking**: Choice points using exceptions and continuations
- **Clause Database**: Dynamic predicate storage with arity-based indexing
- **Built-in Predicates**: Standard Prolog predicates (arithmetic, control flow, lists)
- **Debugging Support**: Spy/trace functionality

### Testing Framework
- **Scheme**: SRFI-64 with implementation-specific runners
  - `scheme/test/test.scm`: Main test suite
  - `scheme/test/test.7.scm`: R7RS runner
  - `scheme/test/test.6.scm`: R6RS runner
  - `scheme/test/test.rkt`: Racket-specific tests
  - `scheme/test/core.scm`, `scheme/test/helpers.scm`, `scheme/test/lib.scm`: Test utilities
- **Common Lisp**: FiveAM framework
  - `common-lisp/test/`: Modular test files with ASDF integration
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
1. Add to `scheme/src/primitive.scm` for Scheme version
2. Add to `common-lisp/src/primitive.lisp` for Common Lisp version
3. Add tests to appropriate test files in `scheme/test/` and `common-lisp/test/`
4. Update documentation in README.md

### Running Single Tests
```bash
# Scheme (example with Racket)
racket scheme/test/test.rkt

# Common Lisp (example with SBCL)
sbcl --eval "(require 'asdf)" --eval "(push #P\"./common-lisp/\" asdf:*central-registry*)" --eval "(asdf:test-system :prolog)" --quit
```

### Web Deployment
The project includes web deployment capability through Gambit Scheme compilation to JavaScript. Use `./script/build.sh` to build the web version. The build script automatically copies the Scheme source files from `scheme/src/` to the Gambit build directory.

## Important Instructions

- Do what has been asked; nothing more, nothing less
- NEVER create files unless they're absolutely necessary for achieving your goal
- ALWAYS prefer editing an existing file to creating a new one
- NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User
