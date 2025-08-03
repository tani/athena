# Athena Prolog Engine Architecture

## Overview

Athena is a comprehensive Prolog engine designed for embedding logic programming capabilities in both Scheme and Common Lisp environments. The architecture emphasizes modularity, performance, and cross-language compatibility.

## Core Architecture Components

### 1. Multi-Language Support Structure

```
athena/
├── src/
│   ├── prolog.scm           # Core engine (Scheme)
│   ├── prolog.sld           # R7RS library wrapper
│   ├── prolog.rkt           # Racket wrapper
│   ├── prolog.sls           # R6RS library wrapper
│   └── prolog/              # Common Lisp implementation
│       ├── core.lisp        # Core engine (CL)
│       ├── primitive.lisp   # Built-in predicates (CL)
│       └── stdlib.lisp      # Standard library (CL)
└── test/                    # Comprehensive test suites
```

### 2. Core Engine Components

#### Unification Engine
- **Location**: `prolog-core.scm`, `prolog/core.lisp`
- **Purpose**: Handles variable binding and term unification
- **Key Features**:
  - Occurs check prevention of infinite terms
  - Efficient variable substitution
  - Support for complex nested structures

#### Choice Points and Backtracking
- **Implementation**: Exception-based backtracking with continuations
- **Cut Operator**: Implemented via exceptions with choice point tags
- **Performance**: Optimized for tail recursion and memory efficiency

#### Clause Database
- **Structure**: Dynamic predicate storage with arity-based indexing
- **Operations**: Add, remove, query clauses with O(1) average access
- **Persistence**: In-memory with optional serialization support

### 3. Built-in Predicates System

#### Core Predicates
```prolog
% Unification
=/2          % Unification
==/2         % Strict equality
\=/2         % Not unifiable
\==/2        % Not strictly equal

% Type checking
var/1        % Test if unbound variable
atom/1       % Test if atom
number/1     % Test if number
compound/1   % Test if compound term

% Control flow
true/0       % Always succeeds
fail/0       % Always fails
!/0          % Cut operator
call/1       % Meta-call
```

#### Meta-predicates
```prolog
% Solution collection
findall/3    % Find all solutions
bagof/3      % Collect solutions with grouping
setof/3      % Collect unique sorted solutions

% Control predicates
once/1       % Succeed at most once
ignore/1     % Ignore failure
```

#### Arithmetic System
```prolog
% Evaluation
is/2         % Arithmetic evaluation

% Comparison
>/2, </2     % Greater/less than
>=/2, =</2   % Greater/less than or equal
=:=/2        % Arithmetic equality
=\=/2        % Arithmetic inequality
```

### 4. Implementation-Specific Features

#### Scheme Implementation
- **Conditional Compilation**: `cond-expand` for different Scheme implementations
- **Parameter Objects**: Used for dynamic variables
- **Syntax Extensions**: Custom macros for Prolog syntax
- **Supported Implementations**:
  - Racket, Gambit, Chez Scheme, Gauche
  - Sagittarius, Chibi Scheme, Guile, Chicken Scheme

#### Common Lisp Implementation
- **Package System**: Modular design with separate packages
- **CLOS Integration**: Object-oriented predicate metadata
- **Condition System**: Proper error handling with restartable conditions
- **ASDF Integration**: Standard build and test system

## Data Structures and Algorithms

### Variable Representation
```scheme
;; Scheme: symbols starting with ?
'?x '?variable '?_

;; Common Lisp: symbols in appropriate package
'?x '?variable '?_
```

### Term Structures
```prolog
% Atoms
foo, bar, 'quoted atom'

% Numbers
42, 3.14, -17

% Compound terms
f(a, b), parent(john, mary), +(3, 4)

% Lists (special compound terms)
[a, b, c] ≡ .(a, .(b, .(c, [])))
```

### Clause Representation
```scheme
;; Internal representation: (head . body)
'((parent john mary) . ())           ; Fact
'((grandparent ?x ?z) . 
  ((parent ?x ?y) (parent ?y ?z)))   ; Rule
```

## Memory Management

### Garbage Collection Considerations
- **Variable Bindings**: Use weak references where available
- **Choice Points**: Automatic cleanup when cut
- **Clause Database**: Periodic compaction for removed predicates

### Performance Optimizations
- **Tail Call Optimization**: Proper tail recursion handling
- **Indexing**: First-argument indexing for clause lookup
- **Caching**: Memoization of frequently used predicates

## Debugging and Introspection

### Spy System
```prolog
% Enable tracing
?- spy(predicate_name).

% Disable tracing  
?- nospy(predicate_name).

% Show trace
?- trace.
```

### Debug Output
```
Call: parent(john, ?X)
  Call: [clause 1] parent(john, mary)
  Exit: parent(john, mary)
Exit: parent(john, mary)
```

## Error Handling

### Exception Hierarchy
```
error
├── prolog-error
│   ├── unification-error
│   ├── instantiation-error
│   └── evaluation-error
└── cut-exception (not an error)
```

### Error Recovery
- **Restartable Conditions**: Common Lisp implementation
- **Exception Handlers**: Scheme implementation  
- **Graceful Degradation**: Continue execution when possible

## Performance Characteristics

### Time Complexity
- **Unification**: O(n) where n is term size
- **Database Lookup**: O(1) average, O(n) worst case
- **Backtracking**: O(b^d) where b is branching factor, d is depth

### Space Complexity
- **Variable Bindings**: O(v) where v is number of variables
- **Choice Points**: O(d) where d is recursion depth
- **Clause Storage**: O(c) where c is number of clauses

## Integration Patterns

### Host Language Integration
```scheme
;; Scheme integration
(define-predicate (lisp-predicate x)
  (lambda (bindings success failure)
    (if (some-scheme-test x)
        (success bindings)
        (failure))))
```

```lisp
;; Common Lisp integration
(define-primitive-predicate (cl-predicate term)
  (if (some-cl-test term)
      (prove-all *current-remaining-goals* *current-bindings*)
      (make-failure)))
```

### Foreign Function Interface
- **C Integration**: Via implementation-specific FFI
- **System Calls**: Through host language facilities
- **Database Access**: Via SQL integration predicates

## Testing Architecture

### Test Organization
```
test/
├── shared/
│   ├── test-utilities.lisp     # Common test utilities
│   └── test-patterns.scm       # Scheme test patterns
├── prolog/                     # Common Lisp tests
│   ├── package.lisp           # Test packages
│   ├── core.lisp              # Core engine tests
│   ├── primitive.lisp         # Built-in predicate tests
│   └── stdlib.lisp            # Standard library tests
└── *.scm                      # Scheme implementation tests
```

### Test Categories
- **Unit Tests**: Individual predicate testing
- **Integration Tests**: Cross-module functionality
- **Performance Tests**: Benchmark critical operations
- **Stress Tests**: Large dataset handling
- **Compatibility Tests**: Cross-implementation verification

## Build System

### Common Lisp (ASDF)
```lisp
(asdf:load-system :prolog)
(asdf:test-system :prolog)
```

### Scheme (Make)
```bash
make all          # Test all implementations
make racket       # Test with Racket
make gauche       # Test with Gauche
```

### Development Environment
```bash
devbox shell      # Unified development environment
make format       # Code formatting
lefthook install  # Git hooks setup
```

## Future Extensibility

### Planned Features
- **Constraint Logic Programming**: CLP(FD), CLP(R)
- **Tabling**: Memoization for recursive predicates
- **Parallel Execution**: Multi-threaded proof search
- **Persistence**: Disk-based clause storage
- **Network Distribution**: Remote predicate calls

### Extension Points
- **Custom Predicates**: Host language integration
- **New Data Types**: Extensible type system
- **Alternative Search**: Different proof strategies
- **Optimization Passes**: Code transformation

This architecture provides a solid foundation for both current functionality and future enhancements while maintaining compatibility across multiple host languages and implementations.