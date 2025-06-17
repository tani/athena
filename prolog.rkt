#lang racket/base
(require racket/include
         racket/port
         (only-in srfi/1 alist-delete filter delete-duplicates alist-cons))

(struct failure () #:transparent #:constructor-name make-failure)
(struct success (bindings continuation) #:transparent #:constructor-name make-success)

(define (object->string object)
  (with-output-to-string (lambda () (write object))))

(define (list-sort comparator lst)
  (sort lst comparator))
(define flush-output-port flush-output)

(include "prolog.scm")

(*current-lisp-environment* (make-base-namespace))

(provide (all-defined-out))
