# Athena Reasoner

[![CI](https://github.com/tani/athena/actions/workflows/ci.yml/badge.svg)](https://github.com/tani/athena/actions/workflows/ci.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/norvig/paip-lisp/blob/main/LICENSE)

**A lightweight Prolog-inspired logic programming engine for multiple Scheme implementations.**

> Athena is a portable and extensible logic programming engine inspired by Prolog. It is designed to run on a wide range of Scheme systems, providing a consistent and powerful reasoning toolkit for your Scheme projects. The design is heavily influenced by the Prolog implementation in Peter Norvig's "Paradigms of Artificial Intelligence Programming" (PAIP) and incorporates conveniences from AllegroProlog.

## 🚀 Live Playground

Experience Athena directly in your browser! No installation required.

1.  **Visit the [Athena Playground](https://tani.github.io/athena/)**
2.  Load the engine: `(load "src/prolog.biwa.scm")`
3.  Start querying! `(?- (member ?x '(a b c)))`

## ✨ Features

*   **Portable:** Runs on 10 different Scheme implementations.
*   **Unification:** Powerful pattern matching with occurs check.
*   **Backtracking:** Automatic search for multiple solutions.
*   **Rich Built-in Library:** Includes `cut`, `bagof`, `findall`, `setof`, `if`, `not`, and more.
*   **Interactive REPL:** A `?-` macro for interactive queries.
*   **Scheme Integration:** Call Scheme code from Prolog and vice-versa.
*   **Debugging:** Built-in spy and trace capabilities.
*   **Extensible:** Easily define your own predicates in Scheme.

## ⚙️ Getting Started

### Requirements

*   [Nix](https://nixos.org/) with flakes and `nix-command` enabled.

### Installation

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/tani/athena.git
    cd athena
    ```

2.  **Enter the development shell:**
    This command sets up a complete development environment with all supported Scheme implementations.
    ```bash
    nix develop
    ```

### Running Tests

You can run tests for a specific Scheme implementation or for all of them.

*   **Run all tests:**
    ```bash
    make all
    ```
*   **Run tests for a specific implementation (e.g., Gauche):**
    ```bash
    make gauche
    ```
    (See the `Makefile` for all available targets: `chicken`, `racket`, `guile`, etc.)

## 💡 Usage

Here's a quick example of how to use Athena.

**1. Load the library and define rules:**

```scheme
(import (prolog))

;; Define facts
(<- (parent alice bob))
(<- (parent bob carol))

;; Define a rule
(<- (ancestor ?x ?y)
    (parent ?x ?y))

(<- (ancestor ?x ?y)
    (parent ?x ?z)
    (ancestor ?z ?y))
```

**2. Run a query:**

Use the `?-` macro to find out who Alice is an ancestor of.

```scheme
(?- (ancestor alice ?descendant))
```

Athena will respond with the first solution:

```
(ancestor alice bob)
?descendant = bob
More? (;)
```

Press `;` to see more solutions, or `.` to stop.

```
(ancestor alice carol)
?descendant = carol
More? (;)
.
```

## 📜 API Reference

<details>
<summary>Click to expand API Reference</summary>

### Variables and Bindings

- `(variable? term)`: Checks if a term is a variable (e.g., `?x`).
- `(named-variable? term)`: Like `variable?`, but excludes the anonymous `?`.
- `(atom? term)`: True if the term is not a pair.
- `(substitute-bindings bindings term)`: Applies bindings to a term.
- `(variables-in term)`: Lists all named variables in a term.
- `(replace-anonymous-variables term)`: Replaces `?` with fresh variables.
- `(unify x y bindings)`: Unifies two terms.

### Clause Database

- `(current-clause-database)`: A parameter holding the active clauses.
- `(add-clause! clause)`: Adds a clause to the database.
- `(get-clauses symbol)`: Retrieves clauses for a predicate.
- `(<- (head ...) body ...)`: Macro to define a clause.
- `(<-- (head ...) body ...)`: Macro that replaces existing clauses for the same predicate.
- `(define-predicate (name . args) body ...)`: Defines a Scheme predicate callable from Prolog.

### Query Interface

- `(prove-all goals bindings)`: The low-level entry point for the engine.
- `(prolog goal ...)`: Runs goals and returns a success/failure object.
- `(?- goal ...)`: Starts an interactive query.

### Built‑in Predicates

A rich set of built-in predicates is provided:

- **Control:** `cut`, `call`, `not`, `and`, `or`, `if`, `repeat`, `fail`
- **Evaluation:** `lisp`, `is`
- **List operations:** `member`, `append`
- **Solution gathering:** `bagof`, `findall`, `setof`
- **Term manipulation:** `=`, `==`
- **State management:** `dynamic-put`, `dynamic-get`

</details>

## 🐞 Debugging

Athena includes a simple `spy` mechanism for tracing predicate calls.

1.  **Set the spy points:**
    `current-spy-predicates` holds a list of predicate names to trace.

2.  **Run your query:**
    When a spied predicate is called, Athena will prompt you for an action.

**Example:**

```scheme
(parameterize ((current-spy-predicates '(ancestor)))
  (?- (ancestor alice ?x)))
```

This will result in:

```
Spy on (ancestor alice ?x)? [l=leap c=creep n=nodebug b=break]
```

*   `c` (creep): Show this call and continue prompting.
*   `l` (leap): Show all spy messages without prompting.
*   `n` (nodebug): Skip spying for this call.
*   `b` (break): Open a Scheme REPL.

## 📦 Supported Scheme Implementations

Athena is tested against a wide array of Scheme implementations:

*   Gauche
*   Guile
*   Gambit
*   Chicken
*   Chibi
*   Sagittarius
*   ChezScheme
*   Racket

## 🤝 Contributing

Contributions are welcome! Please feel free to open an issue or submit a pull request.

## 📄 License

This project is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE) for details.

Note: Parts derived from PAIP Prolog are licensed under the [MIT License](https://github.com/norvig/paip-lisp/blob/main/LICENSE).
