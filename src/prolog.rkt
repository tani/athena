#lang racket
;; Scheme wrapper for the Prolog engine

;; Public symbols
(provide
 variable? named-variable? atom?
 failure? success?
 substitute-bindings variables-in
 replace-anonymous-variables unify object->string
 remove-clauses-with-arity!
 current-clause-database run-query
 add-clause! get-clauses <- <-- define-predicate call-with-current-choice-point
 prove-all ?- current-lisp-environment current-spy-predicates current-spy-indent?
 success-bindings success-continuation make-solution-stream)

;; Imports
(require srfi/41
         (only-in srfi/1 alist-delete alist-cons delete-duplicates)
         (only-in rnrs flush-output-port guard raise))

;; Implementation
(struct failure () #:transparent #:constructor-name make-failure)

(struct success (bindings continuation) #:transparent #:constructor-name make-success)

(struct cut-exception (tag value) #:transparent #:constructor-name make-cut-exception)

(define (list-sort lis pred) (sort pred lis))

(define-namespace-anchor anchor)

(define (interaction-environment)
  (namespace-anchor->namespace anchor))

(include "prolog-core.scm")
(include "prolog-lib.scm")
