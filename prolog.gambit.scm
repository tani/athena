(import (scheme base)
        (scheme eval)
        (scheme write)
        (scheme read)
        (only (srfi 1) alist-delete filter delete-duplicates alist-cons)
        (only (srfi 132) list-sort))

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

(current-lisp-environment (interaction-environment))
