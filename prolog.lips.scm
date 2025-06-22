(load "@lips/lib/srfi/1.scm")
(define (:optional maybe default)
  (if (null? maybe)
      default
      (car maybe)))
(define (list-sort x y) (sort y x))
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
(define (flush-input-port _) (list))
(define (flush-output-port _) (newline))
(current-lisp-environment (interaction-environment))
