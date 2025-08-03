# Athena Prolog Test Suite

This directory contains the test suite for the Athena Prolog engine.

## Test Files

### Common Lisp Tests

- **`core.lisp`** - Core engine tests including:
  - Unification and variable binding
  - Clause database operations
  - Backtracking and choice points
  - Cut (!) behavior
  - Anonymous variables
  - Variadic predicates
  - Zero-arity predicates
  - Spy/debugging functionality

- **`primitive.lisp`** - Built-in predicate tests including:
  - Equality predicates (=/2, ==/2)
  - Type checking predicates (atom, var, ground, number, string)
  - Meta-call predicates (call, !)
  - Control flow (if-then-else)
  - Solution collection (findall, bagof, setof)
  - Arithmetic evaluation (is/2)
  - Direct Lisp evaluation (lisp/2)
  - Dynamic variable storage

- **`stdlib.lisp`** - Standard library tests including:
  - Control flow predicates (and, or, not)
  - List operations (member, append, maplist)
  - repeat/0 predicate
  - Helper predicates

- **`helpers.lisp`** - Test helper functions and macros

- **`run-all-tests.lisp`** - Main test runner that executes all test suites

### Scheme Tests

- **`core.scm`** - Core engine tests (Scheme version)
- **`lib.scm`** - Library tests (Scheme version)
- **`helpers.scm`** - Test helpers (Scheme version)
- **`test.scm`**, **`test.rkt`**, **`test.6.scm`**, **`test.7.scm`** - Implementation-specific test runners

## Running Tests

To run all Common Lisp tests:
```bash
sbcl --script test/run-all-tests.lisp
```

To run Scheme tests, use the appropriate implementation's test runner.

## Test Status

As of the latest run, the test suite has:
- ✅ Most tests passing
- ❌ A few known failures:
  - "Cut inside 'or'" - Complex cut/disjunction interaction
  - "bagof fails with no solutions" - Should fail but returns empty list
  - "--all-null empty list" - Minor test issue with helper predicate

The vast majority of functionality is well-tested and working correctly.