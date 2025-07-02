;; Scheme wrapper for the Prolog engine
(define-library (prolog)
  ;; Public symbols
  (export
   variable? named-variable? atom? failure? success?
   substitute-bindings variables-in replace-anonymous-variables
   unify object->string remove-clauses-with-arity!
   current-clause-database primitive-clause-database
   standard-clause-database run-query
   add-clause! get-clauses <- <-- define-predicate prove-all ?- min-arity with-choice-point
   current-lisp-environment current-spy-predicates
   success-bindings success-continuation prolog prolog* solution-stream)
  ;; Imports
  (import
   (scheme base)
   (scheme write)
   (scheme read)
   (scheme eval)
   (srfi 41)
   (only (scheme r5rs) interaction-environment)
   (only (srfi 1) alist-delete filter delete-duplicates alist-cons))
  (cond-expand
   (chicken (import scheme))
   (else))
  (cond-expand
   ((or gambit chicken gauche chibi sagittarius)
    (import (only (srfi 132) list-sort)))
   (guile
    (import (only (rnrs sorting) list-sort))))
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
   (chicken
    (include "prolog.scm"))
   (guile
    (import (only (guile) include-from-path))
    (begin (include-from-path "prolog.scm")))
   ((or gauche chibi sagittarius gambit)
    (include-library-declarations "prolog.scm"))))
