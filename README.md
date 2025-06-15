# Athena Reasoner

Athena Reasoner is a lightweight Prolog-inspired logic programming engine implemented in Scheme. It supports multiple Scheme implementations and provides a consistent interface for defining predicates, running queries, and extending the clause database. The project uses Nix for development environments and GitHub Actions for continuous integration. This project draws inspiration from the PAIP Prolog implementation and, in homage to AllegroProlog, ports its convenient functions.

## Features: Prolog-style logic engine

- Unification, backtracking, occurs–check
- Anonymous-variable renaming
- Built-in predicates (`=`, `==`, `not`, `if`, `bagof`, `setof`, `cut`, etc.)

## Requirements

- Nix (with flakes and nix-command enabled)
- One or more of the supported Scheme implementations:
  - Gauche
  - Guile
  - Chicken
  - Chibi
  - Sagittarius
  - Gambit

## Getting Started

1. Clone the repository

```bash
   git clone https://github.com/tani/athena-reasoner.git
   cd athena-reasoner
````

2. Enter Nix development shell

   ```bash
   nix develop
   ```

3. Run tests

   From within the Nix shell, choose an implementation:

   ```bash
   make IMPLS=gauche
   ```

   Or run with all implementations:

   ```bash
   make all
   ```

## Usage

Load the library into your Scheme session:

```scheme
(import (prolog))
```

Define your own clauses:

```scheme
(<- (parent alice bob))
(<- (parent bob carol))
(<- (ancestor ?x ?y)
    (parent ?x ?z)
    (ancestor ?z ?y))
```

Run a query:

```scheme
(?- (ancestor alice ?descendant))
```

Results will be displayed with variable bindings; press `;` for more solutions or `.` to stop.

## License

This project is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE) for full terms.

PAIP Prolog: [MIT License](https://github.com/norvig/paip-lisp/blob/main/LICENSE)
