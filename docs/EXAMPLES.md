# Athena Prolog Examples

This document provides comprehensive examples demonstrating the capabilities of the Athena Prolog engine across different use cases and complexity levels.

## Table of Contents

1. [Basic Facts and Queries](#basic-facts-and-queries)
2. [Rules and Logic Programming](#rules-and-logic-programming)
3. [Recursive Predicates](#recursive-predicates)
4. [List Processing](#list-processing)
5. [Arithmetic and Computation](#arithmetic-and-computation)
6. [Meta-predicates and Higher-Order Logic](#meta-predicates-and-higher-order-logic)
7. [Control Flow and Cut](#control-flow-and-cut)
8. [Advanced Examples](#advanced-examples)
9. [Host Language Integration](#host-language-integration)
10. [Performance Examples](#performance-examples)

## Basic Facts and Queries

### Simple Facts
```prolog
% Define basic facts about family relationships
(<- (parent john mary))
(<- (parent john mike))
(<- (parent mary susan))
(<- (parent mary tom))

% Query for John's children
(?- (parent john ?child))
% Results: ?child = mary, ?child = mike

% Query for Mary's parents
(?- (parent ?parent mary))
% Results: ?parent = john
```

### Type Testing
```prolog
% Test variable types
(?- (var ?X))           % Succeeds - ?X is unbound
(?- (atom foo))         % Succeeds - foo is an atom
(?- (number 42))        % Succeeds - 42 is a number
(?- (compound (f a)))   % Succeeds - (f a) is compound

% Practical type checking
(<- (positive-number ?x) (number ?x) (> ?x 0))
(?- (positive-number 5))    % Succeeds
(?- (positive-number -3))   % Fails
```

## Rules and Logic Programming

### Basic Rules
```prolog
% Define grandparent relationship
(<- (grandparent ?gp ?gc) 
    (parent ?gp ?p) 
    (parent ?p ?gc))

% Query for grandparents
(?- (grandparent john ?grandchild))
% Results: ?grandchild = susan, ?grandchild = tom
```

### Multiple Rule Definitions
```prolog
% Define ancestor relationship (multiple clauses)
(<- (ancestor ?x ?y) (parent ?x ?y))
(<- (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))

% Sibling relationship
(<- (sibling ?x ?y) 
    (parent ?p ?x) 
    (parent ?p ?y) 
    (\\== ?x ?y))

% Query for all of Mary's siblings
(?- (sibling mary ?sib))
% Results: ?sib = mike
```

### Complex Logic
```prolog
% Define family relationships with gender
(<- (male john))
(<- (male mike))
(<- (male tom))
(<- (female mary))
(<- (female susan))

% Father and mother relationships
(<- (father ?f ?c) (parent ?f ?c) (male ?f))
(<- (mother ?m ?c) (parent ?m ?c) (female ?m))

% Uncle relationship
(<- (uncle ?u ?n) 
    (parent ?p ?n) 
    (sibling ?u ?p) 
    (male ?u))

(?- (uncle ?uncle susan))
% Results: ?uncle = mike
```

## Recursive Predicates

### List Processing Recursion
```prolog
% Define list membership
(<- (member ?x (?x . ?_)))
(<- (member ?x (?_ . ?rest)) (member ?x ?rest))

% Example queries
(?- (member 3 (1 2 3 4)))     % Succeeds
(?- (member 5 (1 2 3 4)))     % Fails
(?- (member ?x (a b c)))      % ?x = a, ?x = b, ?x = c
```

### Tree Traversal
```prolog
% Binary tree representation: tree(Value, Left, Right) or leaf(Value)
(<- (tree-member ?x (leaf ?x)))
(<- (tree-member ?x (tree ?x ?_ ?_)))
(<- (tree-member ?x (tree ?_ ?left ?_)) (tree-member ?x ?left))
(<- (tree-member ?x (tree ?_ ?_ ?right)) (tree-member ?x ?right))

% Example tree
(<- (sample-tree (tree 5 
                      (tree 3 (leaf 1) (leaf 4))
                      (tree 8 (leaf 7) (leaf 9)))))

(?- (sample-tree ?tree) (tree-member 7 ?tree))  % Succeeds
```

### Path Finding
```prolog
% Graph representation
(<- (edge a b))
(<- (edge b c))
(<- (edge b d))
(<- (edge c e))
(<- (edge d e))

% Path finding with cycle detection
(<- (path ?from ?to (?from ?to)) (edge ?from ?to))
(<- (path ?from ?to (?from . ?rest)) 
    (edge ?from ?middle) 
    (path ?middle ?to ?rest)
    (\\= ?middle ?from))  % Prevent immediate cycles

(?- (path a e ?route))
% Results: Various paths from a to e
```

## List Processing

### Basic List Operations
```prolog
% List concatenation
(<- (append () ?list ?list))
(<- (append (?head . ?tail) ?list (?head . ?result))
    (append ?tail ?list ?result))

% Examples
(?- (append (1 2) (3 4) ?result))
% Result: ?result = (1 2 3 4)

(?- (append ?x ?y (1 2 3 4)))
% Multiple solutions for splitting the list
```

### List Utilities
```prolog
% List length
(<- (length () 0))
(<- (length (?_ . ?tail) ?n) 
    (length ?tail ?n1) 
    (is ?n (+ ?n1 1)))

% List reversal
(<- (reverse ?list ?reversed) (reverse-acc ?list () ?reversed))
(<- (reverse-acc () ?acc ?acc))
(<- (reverse-acc (?head . ?tail) ?acc ?result)
    (reverse-acc ?tail (?head . ?acc) ?result))

% Examples
(?- (length (a b c d) ?len))      % ?len = 4
(?- (reverse (1 2 3) ?rev))       % ?rev = (3 2 1)
```

### Advanced List Operations
```prolog
% List sorting (insertion sort)
(<- (sorted ()))
(<- (sorted (?x)))
(<- (sorted (?x ?y . ?rest)) 
    (=< ?x ?y) 
    (sorted (?y . ?rest)))

(<- (insert ?x () (?x)))
(<- (insert ?x (?h . ?t) (?x ?h . ?t)) (=< ?x ?h))
(<- (insert ?x (?h . ?t) (?h . ?result)) 
    (> ?x ?h) 
    (insert ?x ?t ?result))

(<- (sort () ()))
(<- (sort (?h . ?t) ?sorted) 
    (sort ?t ?sorted-tail) 
    (insert ?h ?sorted-tail ?sorted))

(?- (sort (3 1 4 1 5 9) ?result))
% Result: ?result = (1 1 3 4 5 9)
```

## Arithmetic and Computation

### Basic Arithmetic
```prolog
% Arithmetic evaluation with is/2
(?- (is ?result (+ 2 3)))         % ?result = 5
(?- (is ?result (* 4 5)))         % ?result = 20
(?- (is ?result (- 10 3)))        % ?result = 7

% Arithmetic comparisons
(?- (> 5 3))                      % Succeeds
(?- (< 10 5))                     % Fails
(?- (=:= (+ 2 3) 5))             % Succeeds
```

### Mathematical Functions
```prolog
% Factorial
(<- (factorial 0 1))
(<- (factorial ?n ?result) 
    (> ?n 0) 
    (is ?n1 (- ?n 1)) 
    (factorial ?n1 ?result1) 
    (is ?result (* ?n ?result1)))

(?- (factorial 5 ?result))        % ?result = 120

% Fibonacci sequence
(<- (fib 0 0))
(<- (fib 1 1))
(<- (fib ?n ?result) 
    (> ?n 1) 
    (is ?n1 (- ?n 1)) 
    (is ?n2 (- ?n 2)) 
    (fib ?n1 ?f1) 
    (fib ?n2 ?f2) 
    (is ?result (+ ?f1 ?f2)))

(?- (fib 10 ?result))             % ?result = 55
```

### Constraint Solving
```prolog
% Simple equation solving
(<- (solve-linear ?a ?b ?x) 
    (\\= ?a 0) 
    (is ?x (/ (- 0 ?b) ?a)))

(?- (solve-linear 2 -6 ?x))       % ?x = 3 (solving 2x - 6 = 0)

% Range generation
(<- (range ?from ?to (?from)) (=:= ?from ?to))
(<- (range ?from ?to (?from . ?rest)) 
    (< ?from ?to) 
    (is ?next (+ ?from 1)) 
    (range ?next ?to ?rest))

(?- (range 1 5 ?list))            % ?list = (1 2 3 4 5)
```

## Meta-predicates and Higher-Order Logic

### Solution Collection
```prolog
% Using findall/3
(<- (parent john mary))
(<- (parent john mike))
(<- (parent mary susan))

(?- (findall ?child (parent john ?child) ?children))
% Result: ?children = (mary mike)

(?- (findall ?parent (parent ?parent mary) ?parents))
% Result: ?parents = (john)
```

### Advanced Meta-programming
```prolog
% Dynamic predicate creation
(<- (assert-fact ?fact) (call (<- ?fact)))

% Predicate existence testing
(<- (predicate-exists ?pred) 
    (findall ?x (call ?pred) ?solutions) 
    (\\= ?solutions ()))

% Higher-order predicates
(<- (apply-to-all ?pred ()))
(<- (apply-to-all ?pred (?head . ?tail)) 
    (call ?pred ?head) 
    (apply-to-all ?pred ?tail))

% Example: check if all elements are numbers
(?- (apply-to-all number (1 2 3 4)))     % Succeeds
(?- (apply-to-all number (1 a 3 4)))     % Fails
```

### Maplist Implementation
```prolog
% Map a predicate over lists
(<- (maplist ?pred () ()))
(<- (maplist ?pred (?x . ?xs) (?y . ?ys)) 
    (call ?pred ?x ?y) 
    (maplist ?pred ?xs ?ys))

% Example: increment all numbers
(<- (increment ?x ?y) (is ?y (+ ?x 1)))
(?- (maplist increment (1 2 3) ?result))
% Result: ?result = (2 3 4)
```

## Control Flow and Cut

### Basic Cut Usage
```prolog
% Deterministic maximum
(<- (max ?x ?y ?x) (>= ?x ?y) !)
(<- (max ?x ?y ?y))

(?- (max 5 3 ?result))            % ?result = 5 (deterministic)

% First solution only
(<- (first-child ?parent ?child) 
    (parent ?parent ?child) !)

(?- (first-child john ?child))    % ?child = mary (first one only)
```

### If-Then-Else Pattern
```prolog
% Conditional execution
(<- (if-then-else ?condition ?then ?else) 
    (call ?condition) ! 
    (call ?then))
(<- (if-then-else ?condition ?then ?else) 
    (call ?else))

% Absolute value
(<- (abs ?x ?result) 
    (if-then-else (>= ?x 0) 
                  (= ?result ?x) 
                  (is ?result (- 0 ?x))))

(?- (abs -5 ?result))             % ?result = 5
(?- (abs 3 ?result))              % ?result = 3
```

### Complex Control Structures
```prolog
% Multiple condition checking
(<- (classify-number ?n positive) (> ?n 0) !)
(<- (classify-number ?n negative) (< ?n 0) !)
(<- (classify-number ?n zero))

(?- (classify-number -3 ?class))  % ?class = negative

% Exception handling simulation
(<- (safe-divide ?x ?y ?result) 
    (\\= ?y 0) 
    (is ?result (/ ?x ?y)) !)
(<- (safe-divide ?x ?y error))

(?- (safe-divide 10 2 ?result))   % ?result = 5
(?- (safe-divide 10 0 ?result))   % ?result = error
```

## Advanced Examples

### Expert System
```prolog
% Simple animal identification expert system
(<- (has-fur mammal))
(<- (has-feathers bird))
(<- (has-scales fish))
(<- (has-scales reptile))

(<- (lives-in-water fish))
(<- (lives-in-water whale))
(<- (flies bird))
(<- (flies bat))

(<- (mammal dog))
(<- (mammal cat))
(<- (mammal whale))
(<- (mammal bat))

(<- (identify ?animal mammal) (mammal ?animal) !)
(<- (identify ?animal bird) (flies ?animal) (has-feathers bird) !)
(<- (identify ?animal fish) (lives-in-water ?animal) (has-scales fish) !)
(<- (identify ?animal unknown))

(?- (identify dog ?type))         % ?type = mammal
(?- (identify eagle ?type))       % ?type = unknown
```

### State Space Search
```prolog
% 8-puzzle representation and solving
(<- (goal-state (1 2 3 4 5 6 7 8 0)))

(<- (move (1 2 3 4 5 6 7 8 0) (1 2 3 4 5 0 7 8 6)))  % Move blank up
(<- (move (1 2 3 4 5 6 7 8 0) (1 2 3 4 5 6 7 0 8)))  % Move blank left

% Breadth-first search implementation
(<- (solve-puzzle ?start ?solution) 
    (bfs ((?start ())) ?solution))

(<- (bfs ((?state ?path) . ?_) ?path) (goal-state ?state))
(<- (bfs ((?state ?path) . ?queue) ?solution) 
    (findall (?next (?move . ?path)) 
             (move ?state ?next) 
             ?successors) 
    (append ?queue ?successors ?new-queue) 
    (bfs ?new-queue ?solution))
```

### Parser Implementation
```prolog
% Simple arithmetic expression parser
(<- (expr ?tokens ?result ?rest) 
    (term ?tokens ?result ?rest))
(<- (expr ?tokens ?result ?rest) 
    (term ?tokens ?term1 ?tokens2) 
    (op-plus ?tokens2 ?tokens3) 
    (expr ?tokens3 ?term2 ?rest) 
    (is ?result (+ ?term1 ?term2)))

(<- (term ?tokens ?result ?rest) 
    (factor ?tokens ?result ?rest))
(<- (term ?tokens ?result ?rest) 
    (factor ?tokens ?factor1 ?tokens2) 
    (op-mult ?tokens2 ?tokens3) 
    (term ?tokens3 ?factor2 ?rest) 
    (is ?result (* ?factor1 ?factor2)))

(<- (factor ((?num) . ?rest) ?num ?rest) (number ?num))
(<- (factor (('(') . ?tokens) ?result ?rest) 
    (expr ?tokens ?result ?tokens2) 
    (op-close ?tokens2 ?rest))

(<- (op-plus (('+') . ?rest) ?rest))
(<- (op-mult (('*') . ?rest) ?rest))
(<- (op-close ((')') . ?rest) ?rest))

% Example: parse "3 + 4 * 5"
(?- (expr ((3) ('+') (4) ('*') (5)) ?result ()))
% Result: ?result = 23
```

## Host Language Integration

### Scheme Integration
```scheme
;; Scheme-specific predicates
(define-predicate (scheme-eval ?expr ?result)
  (let ((evaluated (eval (substitute-bindings (current-bindings) ?expr))))
    (prove-all (list `(= ,?result ,evaluated)) (current-bindings))))

;; Usage in Prolog
(?- (scheme-eval (+ 2 3) ?result))  % ?result = 5
```

### Common Lisp Integration
```lisp
;; Common Lisp integration
(define-primitive-predicate (cl-eval expression result)
  :documentation "Evaluate Common Lisp expression"
  (let* ((expr (substitute-bindings *current-bindings* expression))
         (value (eval expr))
         (new-bindings (unify result value *current-bindings*)))
    (if (failure-p new-bindings)
        (make-failure)
        (prove-all *current-remaining-goals* new-bindings))))

;; Usage
(?- (cl-eval (+ 2 3) ?result))     % ?result = 5
```

### Foreign Function Interface
```prolog
% System integration predicates
(<- (file-exists ?filename) 
    (lisp (probe-file ?filename) ?result) 
    (\\= ?result nil))

(<- (current-time ?time) 
    (lisp (get-universal-time) ?time))

% Database integration
(<- (sql-query ?query ?result) 
    (lisp (execute-sql ?query) ?result))
```

## Performance Examples

### Tail Recursion Optimization
```prolog
% Inefficient recursive sum (not tail recursive)
(<- (sum-list () 0))
(<- (sum-list (?head . ?tail) ?sum) 
    (sum-list ?tail ?tail-sum) 
    (is ?sum (+ ?head ?tail-sum)))

% Efficient tail-recursive sum
(<- (sum-list-fast ?list ?sum) (sum-acc ?list 0 ?sum))
(<- (sum-acc () ?acc ?acc))
(<- (sum-acc (?head . ?tail) ?acc ?sum) 
    (is ?new-acc (+ ?acc ?head)) 
    (sum-acc ?tail ?new-acc ?sum))

% Performance comparison
(?- (time (sum-list-fast (1 2 3 ... 1000) ?sum)))  % Fast
(?- (time (sum-list (1 2 3 ... 1000) ?sum)))       % Slow
```

### Indexing and Optimization
```prolog
% First-argument indexing helps performance
% Good: varied first arguments
(<- (process person ?name ?data))
(<- (process company ?name ?data))
(<- (process product ?name ?data))

% Bad: same first argument
(<- (bad-process data person ?name))
(<- (bad-process data company ?name))
(<- (bad-process data product ?name))

% Memoization pattern
(<- (expensive-calc ?input ?result) 
    (memo ?input ?result) !)
(<- (expensive-calc ?input ?result) 
    (complex-computation ?input ?result) 
    (assert-fact (memo ?input ?result)))
```

### Memory-Efficient Patterns
```prolog
% Use difference lists for efficient concatenation
(<- (dlist-concat ?list1-?hole1 ?hole1-?hole2 ?list1-?hole2))

% Example: efficient list building
(<- (build-list ?n ?list) 
    (build-dlist ?n ?list-() ?list))

(<- (build-dlist 0 ?acc-?acc ?acc))
(<- (build-dlist ?n (?n . ?rest)-?hole ?result) 
    (> ?n 0) 
    (is ?n1 (- ?n 1)) 
    (build-dlist ?n1 ?rest-?hole ?result))

(?- (build-list 5 ?list))         % ?list = (5 4 3 2 1)
```

These examples demonstrate the range and power of the Athena Prolog engine, from simple facts to complex applications. The engine's design supports both traditional Prolog programming patterns and modern integration with host languages, making it suitable for a wide variety of logic programming applications.