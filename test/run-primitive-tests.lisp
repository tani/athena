;;; test/run-primitive-tests.lisp - Run only primitive tests

;; Load the Prolog system
(load "src/prolog/core.lisp")
(load "src/prolog/primitive.lisp")
(load "src/prolog/stdlib.lisp")

;; Load test helpers
(load "test/helpers.lisp")

;; Run primitive tests
(format t "~%Running Prolog primitive tests...~%")
(format t "==================================~%")

(load "test/primitive.lisp")