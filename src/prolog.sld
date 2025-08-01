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
    make-solution-stream)
  ;; Imports
  (import
    (scheme base)
    (scheme write)
    (scheme read)
    (scheme eval)
    (only (scheme r5rs) interaction-environment)
    (only (srfi 1) alist-delete filter delete-duplicates alist-cons))
  (cond-expand
    ((or gauche chibi sagittarius)
      (import (scheme sort) (scheme stream)))
    ((or gambit chicken)
      (import (srfi 41) (srfi 132)))
    (guile
      (import (srfi 41) (rnrs sorting))))

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
      (include "prolog-core.scm")
      (include "prolog-lib.scm"))
    (guile
      (import (only (guile) include-from-path))
      (begin
        (include-from-path "prolog-core.scm")
        (include-from-path "prolog-lib.scm")))
    ((or gauche sagittarius gambit chibi)
      (include-library-declarations "prolog-core.scm")
      (include-library-declarations "prolog-lib.scm"))))
