;; Scheme wrapper for the Prolog engine
(define-library (prolog)
  ;; Public symbols
  (export
   variable? named-variable? atom? failure? success?
   substitute-bindings variables-in replace-anonymous-variables
   unify object->string remove-clauses-with-arity!
   current-clause-database primitive-clause-database
   standard-clause-database run-query
   add-clause! get-clauses <- <-- define-predicate prove-all ?- min-arity call-with-current-choice-point
   current-lisp-environment current-spy-predicates current-spy-indent?
   success-bindings success-continuation make-solution-stream)
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
   (gambit
    (import (srfi 41) (srfi 132)))
   (chicken
    (import (scheme) (srfi 41) (srfi 132)))
   (guile
    (import (srfi 41) (rnrs sorting)))
   (mit
    (begin
      (define (list-sort x y) (sort y x))
      (define-syntax define-stream
        (syntax-rules ()
          ((_ (name . formals) body ...)
           (define (name . formals)
             body ...))))
      (define (stream-unfold f p g seed)
        (if (p seed)
            '()
            (cons-stream (f seed) (stream-unfold f p g (g seed))))))))
  ;; Implementation
  (begin
    (define-record-type <failure> (make-failure) failure?)

    (define-record-type <success>
      (make-success bindings continuation) success?
      (bindings success-bindings)
      (continuation success-continuation))

    (define-record-type <cut-exception>
      (make-cut-exception tag value) cut-exception?
      (tag cut-exception-tag)
      (value cut-exception-value))

    (define (object->string object)
      (parameterize ((current-output-port (open-output-string)))
        (write object)
        (get-output-string (current-output-port)))))
  (cond-expand
   ((or chicken mit)
    (include "prolog.scm"))
   (guile
    (import (only (guile) include-from-path))
    (begin (include-from-path "prolog.scm")))
   ((or gauche chibi sagittarius gambit)
    (include-library-declarations "prolog.scm"))))
