# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Athena is a comprehensive Prolog engine implemented in Scheme, designed for embedding logic programming capabilities in Racket and various Scheme environments. The project provides seamless integration between Prolog's pattern-matching/backtracking and Scheme's functional programming.

## Architecture

### Core Components

- **`src/prolog.scm`**: Core Prolog engine implementation (highly commonized)
- **`src/prolog.sld`**: R7RS library wrapper with conditional compilation for different Scheme implementations
- **`src/prolog.rkt`**: Racket-specific wrapper  
- **`src/prolog.sls`**: R6RS library wrapper

### Multi-Implementation Support

The codebase supports multiple Scheme implementations through conditional compilation:
- Racket, Gambit, Chez Scheme, Gauche, Sagittarius, Chibi Scheme, Guile, Chicken Scheme, MIT Scheme

Each implementation has specific import/include patterns handled in `prolog.sld` using `cond-expand`.

### Key Architectural Patterns

- **Unification Engine**: Core logic in `prolog.scm` handles variable binding and term unification
- **Choice Points**: Backtracking implemented using exceptions and continuations  
- **Clause Database**: Dynamic predicate storage with arity-based indexing
- **Built-in Predicates**: Standard Prolog predicates (arithmetic, control flow, list operations)
- **Debugging Support**: Spy/trace functionality with configurable indentation

## Development Commands

### Testing
Run tests across all supported Scheme implementations:
```bash
make all
```

Test specific implementations:
```bash
make racket    # Test with Racket
make gauche    # Test with Gauche  
make chicken   # Test with Chicken Scheme
make chez      # Test with Chez Scheme
make guile     # Test with Guile
make chibi     # Test with Chibi Scheme
make sagittarius # Test with Sagittarius
make gambit    # Test with Gambit
```

### Clean
```bash
make clean     # Remove log files
```

### Development Environment
Use Nix flake for development environment with all Scheme implementations:
```bash
nix develop    # Enters shell with all Scheme interpreters
```

### Build for Web (Gambit)
```bash
./script/build.sh   # Builds Gambit version for web deployment
```

## Testing Framework

Tests use SRFI-64 (Scheme Testing Framework):
- **`test/test.scm`**: Main test suite 
- **`test/test.seven.scm`**: R7RS test runner
- **`test/test.chez.scm`**: Chez Scheme specific tests
- **`test/test.rkt`**: Racket specific tests

## Key APIs for Development

### Defining Predicates
- `<-`: Define Prolog clause
- `<--`: Define clause, removing existing clauses for same predicate/arity
- `define-predicate`: Define predicate using Scheme procedure

### Querying
- `?-`: Interactive query macro
- `run-query`: Programmatic query execution
- `make-solution-stream`: Returns stream of all solutions

### Built-in Predicates
Core predicates include unification (`=`), arithmetic (`is`), control flow (`if`, `not`, `!`), and list operations (`member`, `append`, `maplist`, `findall`, `bagof`).

## Development Notes

- The core engine (`prolog.scm`) is implementation-agnostic
- Wrapper files handle implementation-specific features and imports
- Variable names starting with `?` are Prolog variables
- Anonymous variables use `?` (replaced with unique symbols during processing)
- Cut operator (`!`) implemented via exceptions with choice point tags
- Debugging controlled via `current-spy-predicates` and `current-spy-indent?` parameters