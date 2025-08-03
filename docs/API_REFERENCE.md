# Athena Prolog API Reference

## Overview

This document provides a comprehensive reference for the Athena Prolog engine API, covering both Scheme and Common Lisp interfaces.

## Table of Contents

1. [Core API](#core-api)
2. [Built-in Predicates](#built-in-predicates)
3. [Meta-predicates](#meta-predicates)
4. [Type Testing Predicates](#type-testing-predicates)
5. [Arithmetic Predicates](#arithmetic-predicates)
6. [Control Flow Predicates](#control-flow-predicates)
7. [Database Operations](#database-operations)
8. [Debugging and Introspection](#debugging-and-introspection)
9. [Error Handling](#error-handling)
10. [Host Language Integration](#host-language-integration)

---

## Core API

### Clause Definition

#### `<-` (Define Clause)
**Syntax**: `(<- head body...)`  
**Description**: Define a Prolog clause (fact or rule).

```prolog
% Fact
(<- (parent john mary))

% Rule
(<- (grandparent ?gp ?gc) (parent ?gp ?p) (parent ?p ?gc))
```

**Parameters**:
- `head`: The head of the clause (what it proves)
- `body...`: Zero or more goals that must be satisfied

**Returns**: Clause added to database

---

#### `<--` (Replace Clauses)
**Syntax**: `(<-- head body...)`  
**Description**: Define a clause, first removing all existing clauses for the same predicate/arity.

```prolog
% Replace all existing clauses for max/3
(<-- (max ?x ?y ?x) (>= ?x ?y))
(<-- (max ?x ?y ?y) (< ?x ?y))
```

**Use Case**: When you want to redefine a predicate completely.

---

### Query Execution

#### `?-` (Interactive Query)
**Syntax**: `(?- goal...)`  
**Description**: Execute an interactive query with user-friendly output.

```prolog
(?- (parent john ?child))
% Output: ?child = mary
%         ?child = mike
```

**Interactive Features**:
- Displays solutions one by one
- Prompts for continuation (`;` for more, `Enter` to stop)
- Pretty-prints variable bindings

---

#### `run-query` (Programmatic Query)
**Syntax**: `(run-query goals)`  
**Description**: Execute a query programmatically, returning all solutions.

```scheme
;; Scheme
(run-query '((parent john ?child)))
;; Returns: ((?child . mary) (?child . mike))
```

```lisp
;; Common Lisp
(run-query '((parent john ?child)))
;; Returns: ((?child . mary) (?child . mike))
```

**Parameters**:
- `goals`: List of goals to prove

**Returns**: List of solution binding lists

---

#### `solve` (Low-level Query Interface)
**Syntax**: `(solve goals success-callback failure-callback)`  
**Description**: Low-level query interface with explicit continuation handling.

```scheme
;; Scheme
(solve '((parent john ?child))
       (lambda (bindings)
         (display (cdr (assoc '?child bindings)))
         (newline))
       (lambda ()
         (display "No more solutions")))
```

```lisp
;; Common Lisp
(solve '((parent john ?child))
       (lambda (bindings)
         (format t "~A~%" (cdr (assoc '?child bindings))))
       (lambda ()
         (format t "No more solutions~%")))
```

**Parameters**:
- `goals`: List of goals to prove
- `success-callback`: Function called for each solution with bindings
- `failure-callback`: Function called when no more solutions exist

---

## Built-in Predicates

### Unification Predicates

#### `=/2` (Unification)
**Syntax**: `(= Term1 Term2)`  
**Description**: Unify two terms, potentially binding variables.

```prolog
(?- (= ?x foo))              % Binds ?x to foo
(?- (= (f ?x) (f a)))        % Binds ?x to a
(?- (= foo bar))             % Fails
```

**Mode**: `(= ?Term1, ?Term2)`  
**Deterministic**: Yes  
**Errors**: None

---

#### `==/2` (Strict Equality)
**Syntax**: `(== Term1 Term2)`  
**Description**: Test strict equality without unification.

```prolog
(?- (== foo foo))            % Succeeds
(?- (== ?x foo))             % Fails if ?x unbound
(?- (= ?x foo) (== ?x foo))  % Succeeds (first unifies, then tests)
```

**Mode**: `(== +Term1, +Term2)`  
**Deterministic**: Yes  
**Errors**: None

---

#### `\\=/2` (Not Unifiable)
**Syntax**: `(\\= Term1 Term2)`  
**Description**: Test that two terms cannot be unified.

```prolog
(?- (\\= foo bar))           % Succeeds
(?- (\\= ?x ?x))             % Fails
(?- (\\= f(?x) g(?y)))       % Succeeds
```

**Mode**: `(\\= ?Term1, ?Term2)`  
**Deterministic**: Yes

---

#### `\\==/2` (Not Strictly Equal)
**Syntax**: `(\\== Term1 Term2)`  
**Description**: Test that two terms are not strictly equal.

```prolog
(?- (\\== foo bar))          % Succeeds  
(?- (\\== foo foo))          % Fails
(?- (\\== ?x foo))           % Succeeds if ?x unbound
```

**Mode**: `(\\== ?Term1, ?Term2)`  
**Deterministic**: Yes

---

## Type Testing Predicates

### Basic Type Tests

#### `var/1` (Variable Test)
**Syntax**: `(var Term)`  
**Description**: Test if term is an unbound variable.

```prolog
(?- (var ?x))                % Succeeds
(?- (= ?x foo) (var ?x))     % Fails (bound after unification)
```

**Mode**: `(var ?Term)`  
**Deterministic**: Yes  
**Performance**: O(1)

---

#### `nonvar/1` (Non-variable Test)
**Syntax**: `(nonvar Term)`  
**Description**: Test if term is not an unbound variable.

```prolog
(?- (nonvar foo))            % Succeeds
(?- (nonvar 123))            % Succeeds
(?- (nonvar ?x))             % Fails if ?x unbound
```

**Mode**: `(nonvar +Term)`  
**Deterministic**: Yes

---

#### `atom/1` (Atom Test)
**Syntax**: `(atom Term)`  
**Description**: Test if term is an atom (symbol).

```prolog
(?- (atom foo))              % Succeeds
(?- (atom 123))              % Fails
(?- (atom (f a)))            % Fails
```

**Mode**: `(atom +Term)`  
**Deterministic**: Yes

---

#### `atomic/1` (Atomic Test)
**Syntax**: `(atomic Term)`  
**Description**: Test if term is atomic (not compound).

```prolog
(?- (atomic foo))            % Succeeds
(?- (atomic 123))            % Succeeds
(?- (atomic (f a)))          % Fails
```

**Mode**: `(atomic +Term)`  
**Deterministic**: Yes

---

#### `compound/1` (Compound Test)
**Syntax**: `(compound Term)`  
**Description**: Test if term is a compound term.

```prolog
(?- (compound (f a)))        % Succeeds
(?- (compound foo))          % Fails
(?- (compound [a,b]))        % Succeeds (lists are compound)
```

**Mode**: `(compound +Term)`  
**Deterministic**: Yes

---

#### `number/1` (Number Test)
**Syntax**: `(number Term)`  
**Description**: Test if term is a number.

```prolog
(?- (number 123))            % Succeeds
(?- (number 3.14))           % Succeeds
(?- (number foo))            % Fails
```

**Mode**: `(number +Term)`  
**Deterministic**: Yes

---

#### `ground/1` (Ground Term Test)
**Syntax**: `(ground Term)`  
**Description**: Test if term is ground (contains no variables).

```prolog
(?- (ground foo))            % Succeeds
(?- (ground (f a b)))        % Succeeds
(?- (ground (f ?x)))         % Fails
```

**Mode**: `(ground +Term)`  
**Deterministic**: Yes  
**Performance**: O(n) where n is term size

---

## Arithmetic Predicates

### Arithmetic Evaluation

#### `is/2` (Arithmetic Evaluation)
**Syntax**: `(is Result Expression)`  
**Description**: Evaluate arithmetic expression and unify with result.

```prolog
(?- (is ?x (+ 2 3)))         % ?x = 5
(?- (is ?y (* 4 ?x)))        % Requires ?x to be bound to number
(?- (is 5 (+ 2 3)))          % Succeeds
(?- (is 6 (+ 2 3)))          % Fails
```

**Mode**: `(is ?Result, +Expression)`  
**Errors**: 
- `instantiation_error`: If expression contains unbound variables
- `type_error`: If expression contains non-numeric terms  
- `evaluation_error`: If arithmetic operation fails (e.g., division by zero)

**Supported Operators**:
- `+`, `-`, `*`, `/`: Basic arithmetic
- `mod`: Modulo operation
- `abs`: Absolute value
- `max`, `min`: Maximum/minimum of two values

---

### Arithmetic Comparison

#### `>/2` (Greater Than)
**Syntax**: `(> Expression1 Expression2)`  
**Description**: Test if first expression is arithmetically greater than second.

```prolog
(?- (> 5 3))                 % Succeeds
(?- (> (+ 2 3) 4))           % Succeeds (5 > 4)
(?- (> ?x 10))               % Requires ?x bound to number
```

**Mode**: `(> +Expression1, +Expression2)`  
**Deterministic**: Yes  
**Errors**: Same as `is/2` for expression evaluation

---

#### `</2` (Less Than)
**Syntax**: `(< Expression1 Expression2)`  
**Description**: Test if first expression is arithmetically less than second.

```prolog
(?- (< 3 5))                 % Succeeds
(?- (< (+ 1 1) 3))           % Succeeds (2 < 3)
```

**Mode**: `(< +Expression1, +Expression2)`

---

#### `>=/2` (Greater Than or Equal)
**Syntax**: `(>= Expression1 Expression2)`  
**Description**: Test if first expression is greater than or equal to second.

```prolog
(?- (>= 5 5))                % Succeeds
(?- (>= 5 3))                % Succeeds
(?- (>= 3 5))                % Fails
```

---

#### `=</2` (Less Than or Equal)
**Syntax**: `(=< Expression1 Expression2)`  
**Description**: Test if first expression is less than or equal to second.

```prolog
(?- (=< 3 5))                % Succeeds
(?- (=< 5 5))                % Succeeds
(?- (=< 5 3))                % Fails
```

---

#### `=:=/2` (Arithmetic Equality)
**Syntax**: `(=:= Expression1 Expression2)`  
**Description**: Test arithmetic equality of two expressions.

```prolog
(?- (=:= (+ 2 3) 5))         % Succeeds
(?- (=:= (* 2 3) (+ 3 3)))   % Succeeds (6 = 6)
```

---

#### `=\\=/2` (Arithmetic Inequality)
**Syntax**: `(=\\= Expression1 Expression2)`  
**Description**: Test arithmetic inequality of two expressions.

```prolog
(?- (=\\= 3 5))              % Succeeds
(?- (=\\= 5 5))              % Fails
```

---

## Control Flow Predicates

### Basic Control

#### `true/0` (Always Succeeds)
**Syntax**: `(true)`  
**Description**: Always succeeds. Used for unconditional success.

```prolog
(?- (true))                  % Always succeeds
(<- (always-true ?x) (true)) % Succeeds for any ?x
```

---

#### `fail/0` (Always Fails)
**Syntax**: `(fail)`  
**Description**: Always fails. Used to force backtracking.

```prolog
(?- (fail))                  % Always fails
(<- (never-succeeds ?x) (fail)) % Never succeeds
```

---

#### `!/0` (Cut)
**Syntax**: `(!)`  
**Description**: Commit to current choice point, preventing backtracking.

```prolog
% Without cut - generates multiple solutions
(<- (max ?x ?y ?x) (>= ?x ?y))
(<- (max ?x ?y ?y))

% With cut - deterministic
(<- (max ?x ?y ?x) (>= ?x ?y) !)
(<- (max ?x ?y ?y))
```

**Effect**: 
- Removes choice points created since entering current clause
- Makes predicate deterministic at cut point
- Cannot undo variable bindings

---

### Meta-call Predicates

#### `call/1` (Meta-call)
**Syntax**: `(call Goal)`  
**Description**: Call goal as if it were written directly.

```prolog
(?- (call (parent john ?x)))  % Equivalent to (?- (parent john ?x))
(?- (= ?goal (atom foo)) (call ?goal)) % Dynamic goal construction
```

**Mode**: `(call +Goal)`

---

#### `call/N` (Meta-call with Arguments)
**Syntax**: `(call Goal Arg1 Arg2 ...)`  
**Description**: Call goal with additional arguments appended.

```prolog
(?- (call parent john ?x))    % Equivalent to (?- (parent john ?x))
(?- (call + 2 3 ?result))     % If + is defined as a predicate
```

---

### Advanced Control

#### `once/1` (Succeed At Most Once)
**Syntax**: `(once Goal)`  
**Description**: Succeed with goal at most once (commit after first success).

```prolog
(?- (once (member ?x [a,b,c]))) % ?x = a only (no backtracking)
(?- (member ?x [a,b,c]))        % ?x = a; ?x = b; ?x = c
```

**Mode**: `(once +Goal)`

---

#### `ignore/1` (Ignore Failure)
**Syntax**: `(ignore Goal)`  
**Description**: Execute goal but always succeed (ignore failure).

```prolog
(?- (ignore (fail)) (write success)) % Prints "success"
(?- (ignore (unknown-predicate)))     % Succeeds despite error
```

**Mode**: `(ignore +Goal)`

---

## Meta-predicates

### Solution Collection

#### `findall/3` (Find All Solutions)
**Syntax**: `(findall Template Goal SolutionList)`  
**Description**: Find all solutions to goal, collecting template instances.

```prolog
(<- (parent john mary))
(<- (parent john mike))
(<- (parent mary susan))

(?- (findall ?child (parent john ?child) ?children))
% Result: ?children = [mary, mike]

(?- (findall (?parent ?child) (parent ?parent ?child) ?pairs))
% Result: ?pairs = [(john mary), (john mike), (mary susan)]
```

**Mode**: `(findall +Template, +Goal, -SolutionList)`  
**Deterministic**: Yes  
**Behavior**: Always succeeds, produces empty list if no solutions

---

#### `bagof/3` (Bag of Solutions)
**Syntax**: `(bagof Template Goal SolutionBag)`  
**Description**: Collect solutions with grouping by free variables.

```prolog
(?- (bagof ?child (parent ?parent ?child) ?children))
% Groups by ?parent:
% ?parent = john, ?children = [mary, mike]
% ?parent = mary, ?children = [susan]
```

**Mode**: `(bagof +Template, +Goal, -SolutionBag)`  
**Deterministic**: No (can generate multiple solutions for different groupings)  
**Behavior**: Fails if no solutions exist

---

#### `setof/3` (Set of Solutions)
**Syntax**: `(setof Template Goal SolutionSet)`  
**Description**: Like bagof/3 but removes duplicates and sorts results.

```prolog
(?- (setof ?x (member ?x [3,1,2,1,3]) ?set))
% Result: ?set = [1, 2, 3]
```

**Mode**: `(setof +Template, +Goal, -SolutionSet)`

---

## Database Operations

### Clause Management

#### `add-clause!` (Add Clause)
**Syntax**: `(add-clause! predicate-symbol clause)`  
**Description**: Add a clause to the database.

```scheme
;; Scheme
(add-clause! 'parent '((parent john mary) . ()))
```

```lisp
;; Common Lisp
(add-clause! 'parent '((parent john mary) . ()))
```

---

#### `get-clauses` (Retrieve Clauses)
**Syntax**: `(get-clauses predicate-symbol)`  
**Description**: Retrieve all clauses for a predicate.

```scheme
;; Returns list of clauses
(get-clauses 'parent)
```

---

#### `remove-clauses-with-arity!` (Remove Clauses)
**Syntax**: `(remove-clauses-with-arity! predicate-symbol arity)`  
**Description**: Remove all clauses for predicate with specified arity.

```scheme
(remove-clauses-with-arity! 'parent 2)  ; Remove parent/2 clauses
```

---

### Database Introspection

#### `database-statistics` (Database Stats)
**Syntax**: `(database-statistics)`  
**Description**: Return statistics about current database.

```lisp
;; Common Lisp
(database-statistics)
;; Returns: (:predicate-count 10 :total-clauses 25 ...)
```

---

## Debugging and Introspection

### Tracing

#### `spy` (Enable Tracing)
**Syntax**: `(spy predicate-name)`  
**Description**: Enable tracing for predicate calls.

```prolog
(?- (spy parent))
(?- (parent john ?x))
% Output:
% Call: parent(john, ?X)
%   Exit: parent(john, mary)
```

---

#### `nospy` (Disable Tracing)
**Syntax**: `(nospy predicate-name)`  
**Description**: Disable tracing for predicate.

```prolog
(?- (nospy parent))
```

---

### Performance Monitoring

#### `time` (Time Execution)
**Syntax**: `(time Goal)`  
**Description**: Execute goal and report execution time.

```prolog
(?- (time (factorial 100 ?result)))
% Output: Goal succeeded in 0.023 seconds
```

---

## Error Handling

### Error Types

#### `prolog-error`
Base class for all Prolog-related errors.

#### `unification-error`
Signaled when unification fails in strict mode.

#### `instantiation-error`
Signaled when a variable needs to be instantiated but isn't.

#### `evaluation-error`
Signaled during arithmetic evaluation errors.

#### `type-error`
Signaled when a term has wrong type for operation.

### Error Handling Predicates

#### `catch/3` (Exception Handling)
**Syntax**: `(catch Goal Catcher Recovery)`  
**Description**: Execute goal with exception handling.

```prolog
(?- (catch (is ?x (/ 1 0)) error (= ?x infinity)))
% Catches division by zero, sets ?x to infinity
```

---

## Host Language Integration

### Scheme Integration

#### `lisp/2` (Scheme Evaluation)
**Syntax**: `(lisp Expression Result)`  
**Description**: Evaluate Scheme expression and unify with result.

```prolog
(?- (lisp (+ 2 3) ?result))     % ?result = 5
(?- (lisp (length '(a b c)) ?len)) % ?result = 3
```

### Common Lisp Integration

#### `cl-eval/2` (Common Lisp Evaluation)
**Syntax**: `(cl-eval Expression Result)`  
**Description**: Evaluate Common Lisp expression and unify with result.

```prolog
(?- (cl-eval (+ 2 3) ?result))  % ?result = 5
(?- (cl-eval (length '(a b c)) ?len)) % ?result = 3
```

### Foreign Function Interface

#### `call-foreign/3` (Foreign Function Call)
**Syntax**: `(call-foreign Function Args Result)`  
**Description**: Call foreign function with arguments.

```prolog
% Platform-specific implementation
(?- (call-foreign 'sqrt [16] ?result))  % ?result = 4.0
```

---

## Performance Notes

### Optimization Guidelines

1. **First-argument indexing**: Vary first arguments for better performance
2. **Tail recursion**: Use accumulator patterns for recursive predicates
3. **Cut placement**: Use cuts to make predicates deterministic when appropriate
4. **Goal ordering**: Place most restrictive goals first
5. **Memory management**: Avoid creating unnecessary choice points

### Performance Predicates

#### `benchmark/2` (Benchmark Execution)
**Syntax**: `(benchmark Goal Time)`  
**Description**: Execute goal and measure execution time.

```prolog
(?- (benchmark (factorial 20 ?result) ?time))
% ?time = 0.045 (seconds)
```

---

This API reference provides comprehensive documentation for all public interfaces of the Athena Prolog engine. For implementation-specific details, consult the architecture documentation and source code comments.