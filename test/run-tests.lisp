;;; test/run-tests.lisp - Test runner for Prolog tests

;; Load the Prolog system
(load "src/prolog/core.lisp")
(load "src/prolog/primitive.lisp")
(load "src/prolog/stdlib.lisp")

;; Load test helpers
(load "test/helpers.lisp")

;; Run tests
(format t "~%Running Prolog tests...~%")
(format t "========================~%")

;; Load and run core tests
(load "test/core.lisp")