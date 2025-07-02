# Athena Prolog

This project provides a comprehensive Prolog engine implemented in Scheme, offering a robust wrapper for embedding logic programming capabilities directly within Racket and various Scheme environments. It is designed for seamless integration, allowing developers to leverage Prolog's powerful pattern-matching and backtracking features alongside the flexibility of a functional programming language.

## Features

  - **Full Prolog Engine**: Implements a core Prolog engine with support for rules, facts, and queries.
  - **Seamless Scheme Integration**: Define Prolog predicates using Scheme procedures and evaluate Scheme code from within Prolog.
  - **Interactive Querying**: Use the `?-` macro for running interactive Prolog queries directly in your REPL.
  - **Debugging Tools**: Includes a spy and break functionality for tracing and debugging predicate execution.
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

To use this library, include the `prolog.scm` file along with the appropriate wrapper for your Scheme implementation in your project. See `src/` directory.
Try athena at [https://tani.github.io/athena](https://tani.github.io/athena).

### Running Queries

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
  - `make-solution-stream`: Returns a stream of all possible solutions for a given goal.

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
| `(setof ?template ?goal ?results)` | Like `bagof`, but each group of solutions is deduplicated and sorted before being unified with `?results`. |
| `(dynamic-get ?name ?var)` | Retrieves the value associated with the dynamic variable `?name` and unifies it with `?var`. |
| `(dynamic-put ?name ?value)` | Sets the value of the dynamic variable `?name` to `?value`. |

## Debugging

The Prolog engine includes a simple spy and break functionality for debugging.

  - `current-spy-predicates`: A parameter that holds a list of predicates to spy on.
  - **Spy Commands**:
      - `l` (leap): Continue execution without spying.
      - `c` (creep): Step through the current goal.
      - `n` (nodebug): Disable spying.
      - `b` (break): Enter a Scheme REPL for debugging.

## License

This project is released under the **GNU General Public License v3.0**.

