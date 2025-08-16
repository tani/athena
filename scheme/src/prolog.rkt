#lang racket

;; Scheme wrapper for the Prolog engine

;; Public symbols
(provide
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
  call-with-current-choice-point
  prove-goal-sequence
  ?-
  current-lisp-environment
  current-spy-predicates
  success-bindings
  success-continuation
  solve)

;; Imports
(require
  (only-in srfi/1 alist-delete alist-cons delete-duplicates fold-right)
  (only-in rnrs flush-output-port guard raise))

;; Implementation
(struct failure () #:transparent #:constructor-name make-failure)

(struct success (bindings continuation) #:transparent #:constructor-name make-success)

(struct cut-exception (tag value) #:transparent #:constructor-name make-cut-exception)

(define-namespace-anchor anchor)

(define (interaction-environment)
  (namespace-anchor->namespace anchor))

(include "core.scm")
(include "primitive.scm")
(include "stdlib.scm")
