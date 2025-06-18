;; prolog-r7rs.scm -- R7RS wrapper for the Prolog engine
(define-library (prolog)
  (export
    variable? named-variable? atom? failure? success?
    extend-bindings substitute-bindings variables-in replace-anonymous-variables
    unify object->string remove-clauses-with-arity!
    current-clause-database primitive-clause-database
    standard-clause-database
    add-clause! get-clauses <- <-- define-predicate prove-all ?-
    current-lisp-environment
    success-bindings success-continuation solve-first solve-all)

  (import (scheme base)
          (scheme eval)
          (scheme write)
          (only (srfi 1) alist-delete filter delete-duplicates alist-cons))

  ;; Optional SRFI‑132 list-sort for each implementation
  (cond-expand
    (chicken (import scheme (only (srfi 132) list-sort)))
    (guile (import (only (rnrs sorting) list-sort)))
    (gauche (import (only (scheme sort) list-sort)))
    (chibi (import (only (scheme sort) list-sort)))
    (gambit (import (only (srfi 132) list-sort)))
    (sagittarius (import (only (scheme sort) list-sort))))

  (begin
    (define-record-type <failure> (make-failure) failure?)
    (define-record-type <success>
      (make-success bindings continuation) success?
      (bindings success-bindings)
      (continuation success-continuation))

     (define (object->string object)
      (parameterize ((current-output-port (open-output-string)))
        (write object)
        (get-output-string (current-output-port))))

    (include "prolog.scm")

    (current-lisp-environment (environment '(scheme base)))
  )
)
