;; Scheme wrapper for the Prolog engine
(library (prolog)
  ;; Public symbols
  (export
   variable? named-variable? atom?
   failure? success?
   substitute-bindings variables-in
   replace-anonymous-variables unify object->string
   remove-clauses-with-arity!
   current-clause-database primitive-clause-database
   standard-clause-database run-query
   add-clause! get-clauses <- <-- define-predicate with-choice-point
   prove-all ?- current-lisp-environment current-spy-predicates
   success-bindings success-continuation prolog prolog*  make-solver solver-next!)

  ;; Imports
  (import (rnrs)
          (rnrs eval)
          (only (srfi :1) alist-delete alist-cons delete-duplicates)
          (only (srfi :39) parameterize make-parameter)
          (only (chezscheme) include interaction-environment))

  ;; Implementation
  (begin
    (define-record-type (<failure> make-failure failure?)
      (fields))

    (define-record-type (<success> make-success success?)
      (fields
       (immutable bindings success-bindings)
       (immutable continuation success-continuation)))

    (define-record-type (<cut-exception> make-cut-exception cut-exception?)
      (fields
       (immutable tag cut-exception-tag)
       (immutable value cut-exception-value)))

    (define (object->string object)
      (call-with-string-output-port
       (lambda (p) (write object p))))

    (include "prolog.scm")

    (current-lisp-environment (interaction-environment))
    )
  )
