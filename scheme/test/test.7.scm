(import
  (scheme base)
  (srfi 64)
  (scheme r5rs)
  (scheme process-context)
  (prolog))

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
(cond-expand
  (chibi
    (include "scheme/test/helpers.scm")
    (include "scheme/test/core.scm")
    (include "scheme/test/lib.scm"))
  (else
    (include "helpers.scm")
    (include "core.scm")
    (include "lib.scm")))
(test-end "prolog")

;; Exit with appropriate code
(exit (if *test-failed* 1 0))
