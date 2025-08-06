#lang racket/base

(require
  srfi/64
  srfi/41
  "../src/prolog.rkt"
  racket/include)

;; Custom test runner to track failures
(define *test-failed* #f)

(define (custom-test-runner)
  (let ((runner (test-runner-simple)))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (unless (eq? 'pass (test-result-kind runner))
          (set! *test-failed* #t))))
    runner))

(test-runner-current (custom-test-runner))

(test-begin "prolog")
(include "helpers.scm")
(include "core.scm")
(include "lib.scm")
(test-end "prolog")

;; Exit with appropriate code
(exit (if *test-failed* 1 0))
