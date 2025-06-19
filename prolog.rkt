#lang racket

(provide
 variable? named-variable? atom?
 failure? success?
 substitute-bindings variables-in
 replace-anonymous-variables unify object->string
 remove-clauses-with-arity!
 current-clause-database primitive-clause-database
 standard-clause-database
 add-clause! get-clauses <- <-- define-predicate
 prove-all ?- current-lisp-environment
 insert-cut-point
 success-bindings success-continuation prolog)

(define (alist-cons key value alist)
  (cons (cons key value) alist))

(define (alist-delete key alist [equal-pred eq?])
  (filter (lambda (pair) (not (equal-pred key (car pair)))) alist))

(struct failure () #:transparent #:constructor-name make-failure)

(struct success (bindings continuation) #:transparent #:constructor-name make-success)

(define (object->string object)
  (with-output-to-string
    (lambda () (write object))))
(define delete-duplicates remove-duplicates)
(define flush-output-port flush-output)
(define (list-sort lis pred)
  (sort pred lis))

(include "prolog.scm")

(current-lisp-environment (make-base-namespace))
