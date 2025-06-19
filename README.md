# Athena Reasoner

Athena Reasoner is a lightweight Prolog-inspired logic programming engine implemented in Scheme. It provides a consistent interface across multiple Scheme implementations while keeping the core engine portable. The project draws inspiration from the PAIP Prolog implementation and ports several AllegroProlog conveniences. Development is managed via Nix and continuous integration is run through GitHub Actions.

## Features

- Unification with optional occurs check and backtracking
- Anonymous variable renaming to avoid clashes
- Built-in predicates such as `=`, `==`, `not`, `if`, `bagof`, `setof`, `cut` and more
- Interactive query syntax with the `?-` macro

## Requirements

- Nix with flakes and `nix-command` enabled
- One or more of the supported Scheme implementations:
  - Gauche
  - Guile
  - Chicken
  - Chibi
  - Sagittarius
  - ChezScheme
  - Racket

## Getting Started

1. Clone the repository

   ```bash
   git clone https://github.com/tani/athena-reasoner.git
   cd athena-reasoner
   ```

2. Enter the development shell

   ```bash
   nix develop
   ```

3. Run tests

   Choose an implementation:

   ```bash
   make IMPLS=gauche
   ```

   Or run the full matrix:

   ```bash
   make all
   ```

## Usage

Load the library and define some clauses:

```scheme
(import (prolog))

(<- (parent alice bob))
(<- (parent bob carol))
(<- (ancestor ?x ?y)
    (parent ?x ?z)
    (ancestor ?z ?y))
```

Run a query interactively:

```scheme
(?- (ancestor alice ?descendant))
```

Results appear with variable bindings; press `;` for more solutions or `.` to stop.

## API Reference

### Variables and Bindings

- `(variable? term)` – predicate for Scheme symbols beginning with `?`.
- `(named-variable? term)` – like `variable?` but excludes the anonymous `?`.
- `(atom? term)` – true if the term is not a pair.
- `(substitute-bindings bindings term)` – apply bindings to a term.
- `(variables-in term)` – list of named variables contained in `term`.
- `(replace-anonymous-variables term)` – substitute all `?` with fresh symbols.
- `(unify x y bindings)` – attempt to unify two terms returning updated bindings or `failure`.

### Clause Database

- `(current-clause-database)` – parameter holding the active clause list.
- `(primitive-clause-database)` – snapshot of the database containing only primitives.
- `(standard-clause-database)` – snapshot with the default standard clauses.
- `(add-clause! clause)` – append a clause to the database.
- `(get-clauses symbol)` – retrieve clauses for a predicate.
- `(remove-clauses-with-arity! symbol arity)` – delete clauses with matching arity.
- `(<- (head ...) body ...)` – macro for adding a clause.
- `(<-- (head ...) body ...)` – macro that replaces previous clauses of the same arity.
- `(define-predicate (name . args) body ...)` – define a pure Scheme predicate callable from Prolog.

### Query Interface

- `(prove-all goals bindings)` – low level engine entry point.
- `(prolog goal ...)` – run goals and return a success/failure object.
- `(?- goal ...)` – interactive REPL style query.
- `(success-bindings obj)` / `(success-continuation obj)` – accessors for success values.

### Built‑in Predicates

The library provides a number of predicates implemented in Scheme:

- `=` and `==` – unification and equality check
- `cut` – prune choice points
- `call` – invoke a goal dynamically
- `not`, `and`, `or`, `if` – logical combinators
- `lisp` / `is` – evaluate Scheme expressions
- `repeat` – backtracking loop
- `member`, `append` – common list utilities
- `bagof`, `setof` – collect solutions
- `fail` – force failure
- `dynamic-put`, `dynamic-get` – store and retrieve dynamic variables

## License

This project is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE) for the full terms.

Parts derived from PAIP Prolog are under the [MIT License](https://github.com/norvig/paip-lisp/blob/main/LICENSE).
