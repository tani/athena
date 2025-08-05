# Athena Prolog

Athena is a comprehensive Prolog engine with dual implementations in Scheme and Common Lisp. The project provides robust logic programming capabilities that can be embedded directly within various Scheme and Common Lisp environments. It is designed for seamless integration, allowing developers to leverage Prolog's powerful pattern-matching and backtracking features alongside functional programming.

## Features

  - **Full Prolog Engine**: Implements a core Prolog engine with support for rules, facts, and queries.
  - **Seamless Scheme Integration**: Define Prolog predicates using Scheme procedures and evaluate Scheme code from within Prolog.
  - **Interactive Querying**: Use the `?-` macro for running interactive Prolog queries directly in your REPL.
  - **Debugging Tools**: Includes spy functionality for tracing predicate execution.
  - **Cross-Implementation Support**: Compatible with multiple Scheme standards (R6RS/R7RS) and implementations:
    - Racket
    - Gambit
    - Chez Scheme
    - Gauche
    - Sagittarius
    - Chibi Scheme
    - Guile
    - Chicken Scheme

## Getting Started

### Installation

To use this library, include the appropriate wrapper for your implementation:

**Scheme implementations** (from `scheme/src/` directory):
- **Racket**: Use `scheme/src/prolog.rkt`
- **R7RS implementations**: Use `scheme/src/prolog.sld`
- **R6RS implementations**: Use `scheme/src/prolog.sls`

**Common Lisp implementations** (using ASDF):
- Add the `common-lisp/` directory to your `asdf:*central-registry*`
- Load with `(asdf:load-system :prolog)`

For development, use Nix to set up an environment with all supported implementations:

```bash
nix develop     # Enters shell with all Scheme and Common Lisp interpreters
```

Try Athena online at [https://tani.github.io/athena](https://tani.github.io/athena).

### Quick Start

To start running queries, you can use the interactive `?-` macro. Here’s how you can define a simple knowledge base and query it:

```scheme
(import (prolog))

;; Define facts
(<-- (parent john mary))
(<- (parent susan mary))

;; Run a query to find the children of mary
(?- (parent ?x mary))
```

This will output:

```
?x = john
Continue ? (y/n) y
?x = susan
Continue ? (y/n) n
```

## API Reference

### Defining Clauses

  - `<-`: Defines a new Prolog clause.
  - `<--`: Defines a clause, removing any existing clauses for the same predicate with the same arity.
  - `define-predicate`: Defines a new predicate using a Scheme procedure.

### Running Queries

  - `?-`: Executes an interactive query and displays solutions.
  - `run-query`: A programmatic way to run queries.
  - `solve`: Executes queries with callback-based solution handling.

## Built-in Predicates

The engine provides a rich set of standard built-in predicates for control flow, arithmetic, list manipulation, and term testing.

| Predicate | Description |
| --- | --- |
| `(true)` / `true` | Always succeeds. |
| `(fail)` / `fail` | Always fails. |
| `(!)` / `!` | The "cut" operator. Commits to all choices made so far and prunes other alternatives. |
| `(if ?test ?then ?else)` | If `?test` succeeds, executes `?then`; otherwise, executes `?else`. |
| `(if ?test ?then)` | If `?test` succeeds, executes `?then`. |
| `(not ?goal)` | Negates a goal. Succeeds if `?goal` fails, and fails if `?goal` succeeds. |
| `(and ?goal1 ...)` | Succeeds if all sub-goals succeed. |
| `(or ?goal1 ...)` | Succeeds if any sub-goal succeeds. |
| `(repeat)` / `repeat` | Creates a choice point that will always succeed on backtracking. |
| `(= ?x ?y)` | Unifies term `?x` with term `?y`. |
| `(== ?x ?y)` | Succeeds if terms `?x` and `?y` are structurally identical, without performing unification. |
| `(is ?result ?expr)` | Evaluates a Scheme expression `?expr` and unifies the result with `?result`. |
| `(lisp ?result ?expr)` | Evaluates a Scheme expression `?expr` and unifies the result with `?result`. |
| `(lisp ?expr)` | Evaluates a Scheme expression `?expr` without unifying the result. |
| `(atom ?x)` | Checks if term `?x` is a Prolog atom (a non-variable symbol). |
| `(atomic ?x)` | Checks if term `?x` is an atomic value (i.e., not a variable or a pair). |
| `(var ?x)` | Checks if term `?x` is an unbound variable. |
| `(ground ?x)` | Checks if term `?x` is fully instantiated with no unbound variables. |
| `(number ?x)` | Checks if term `?x` is a number. |
| `(member ?elem ?list)` | Succeeds if `?elem` is a member of `?list`. |
| `(append ?list1 ?list2 ?list3)` | Succeeds if `?list3` is the result of appending `?list1` and `?list2`. |
| `(maplist ?pred ?list1 ...)` | Applies `?pred` to corresponding elements of one or more lists. |
| `(findall ?template ?goal ?results)` | Collects all solutions for `?goal` that match `?template` into the list `?results`. |
| `(bagof ?template ?goal ?results)` | Groups solutions of `?goal` by the bindings of any free variables and collects the corresponding `?template` values into `?results`. |
| `(dynamic-get ?name ?var)` | Retrieves the value associated with the dynamic variable `?name` and unifies it with `?var`. |
| `(dynamic-put ?name ?value)` | Sets the value of the dynamic variable `?name` to `?value`. |

## Debugging

The Prolog engine includes a simple spy functionality for debugging.

  - `current-spy-predicates`: A parameter that holds a list of predicates to spy on.
  - **Spy Commands**:
      - `l` (leap): Continue execution without spying.
      - `c` (creep): Step through the current goal.
      - `n` (nodebug): Disable spying.

## Development

### Testing

Run tests across all supported Scheme implementations:

```bash
make all
```

Test specific implementations:

```bash
# Scheme implementations
make test-racket      # Test with Racket
make test-gauche      # Test with Gauche
make test-chicken     # Test with Chicken Scheme
make test-chez        # Test with Chez Scheme
make test-guile       # Test with Guile
make test-chibi       # Test with Chibi Scheme
make test-sagittarius # Test with Sagittarius
make test-gambit      # Test with Gambit

# Common Lisp implementations
make test-sbcl        # Test with SBCL
make test-ecl         # Test with ECL
make test-clisp       # Test with CLISP
make test-abcl        # Test with ABCL
```

### Code Formatting

Format all source and test files:

```bash
make format      # Format using schemat
```

### Building for Web

Build the Gambit version for web deployment:

```bash
./script/build.sh
```

### Git Hooks

The project uses lefthook for automated code formatting:

```bash
lefthook install # Install git hooks (runs automatically after setup)
```

Git hooks will automatically run `make format` before each commit and stage any formatting changes.

### Clean

```bash
make clean       # Remove log files
```

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

## Architecture

The project is organized into language-specific directories:

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

## License

This project is released under the **GNU General Public License v3.0**.

