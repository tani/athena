#lang racket
;; Scheme wrapper for the Prolog engine

;; Public symbols
(provide
 variable? named-variable? atom?
 failure? success?
 substitute-bindings variables-in
 replace-anonymous-variables unify object->string
 remove-clauses-with-arity!
 current-clause-database primitive-clause-database
 standard-clause-database run-query
 add-clause! get-clauses <- <-- define-predicate with-choice-point
 prove-all ?- current-lisp-environment current-spy-predicates
 success-bindings success-continuation prolog prolog*)

;; Imports
(require (only-in srfi/1 alist-delete alist-cons delete-duplicates)
         (only-in rnrs flush-output-port guard raise))

;; Implementation
(struct failure () #:transparent #:constructor-name make-failure)

(struct success (bindings continuation) #:transparent #:constructor-name make-success)

(struct cut-exception (tag value) #:transparent #:constructor-name make-cut-exception)

(define (object->string object)
  (with-output-to-string
    (lambda () (write object))))

(define (list-sort lis pred) (sort pred lis))

(include "prolog.scm")

(current-lisp-environment (current-namespace))
