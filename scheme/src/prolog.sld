;; Scheme wrapper for the Prolog engine
(define-library (prolog)
  ;; Public symbols
  (export
    variable?
    named-variable?
    atom?
    failure?
    success?
    substitute-bindings
    variables-in
    replace-anonymous-variables
    unify
    object->string
    remove-clauses-with-arity!
    current-clause-database
    run-query
    add-clause!
    get-clauses
    <-
    <--
    define-predicate
    prove-all
    ?-
    min-arity
    call-with-current-choice-point
    current-lisp-environment
    current-spy-predicates
    success-bindings
    success-continuation
    solve)
  ;; Imports
  (import
    (scheme base)
    (scheme write)
    (scheme read)
    (scheme eval)
    (scheme r5rs)
    (only (srfi 1) alist-delete alist-cons filter delete-duplicates))
  (cond-expand
    ((or gauche chibi sagittarius)
      (import (scheme sort)))
    ((or gambit chicken)
      (import (srfi 132)))
    (guile
      (import (rnrs sorting))))

  ;; Implementation
  (begin
    (define-record-type <failure> (make-failure) failure?)

    (define-record-type <success>
      (make-success bindings continuation)
      success?
      (bindings success-bindings)
      (continuation success-continuation))

    (define-record-type <cut-exception>
      (make-cut-exception tag value)
      cut-exception?
      (tag cut-exception-tag)
      (value cut-exception-value)))

  (cond-expand
    (chicken
      (include "core.scm")
      (include "primitive.scm")
      (include "stdlib.scm"))
    (guile
      (import (only (guile) include-from-path))
      (begin
        (include-from-path "core.scm")
        (include-from-path "primitive.scm")
        (include-from-path "stdlib.scm")))
    ((or gauche sagittarius gambit chibi)
      (include-library-declarations "core.scm")
      (include-library-declarations "primitive.scm")
      (include-library-declarations "stdlib.scm"))))
