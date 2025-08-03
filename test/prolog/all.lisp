;;; test/prolog/all.lisp — Main FiveAM test runner
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(in-package :prolog-test)

;; Define the main test entry point
(defun run-all-tests ()
  "Run all Prolog tests using FiveAM and provide detailed reporting"
  (format t "~%========================================~%")
  (format t "  Athena Prolog Engine - FiveAM Test Suite~%")
  (format t "========================================~%")
  
  ;; Run all tests and capture results
  (let ((results (run :prolog-tests)))
    (format t "~%========================================~%")
    (format t "  Test Results Summary~%")
    (format t "========================================~%")
    
    (if results
        (progn
          (format t "✅ All tests passed!~%")
          (format t "Core tests, primitive tests, and stdlib tests completed successfully.~%")
          t)
        (progn
          (format t "❌ Some tests failed!~%")
          (format t "Check the output above for details.~%")
          nil))))

;; Support for direct script execution
#+sbcl
(defun main ()
  "Main entry point for script execution"
  (when (member "--test" sb-ext:*posix-argv* :test #'string=)
    (run-all-tests)
    (sb-ext:quit)))

;; Auto-run when loaded directly (not via ASDF)
(eval-when (:execute)
  (when (and (boundp '*load-pathname*) *load-pathname*)
    (format t "~%Loading tests directly - running test suite...~%")
    (run-all-tests)))


;; Export the main function for external use
(export 'run-all-tests)
