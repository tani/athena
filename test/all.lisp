;;; test.lisp — Main test package and runner for Athena Prolog Engine
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(defpackage :prolog/test/all
  (:use :cl)
  (:import-from :prolog/all
                #:<-
                #:<--
                #:?-
                #:run-query
                #:solve
                #:variable-p
                #:unify
                #:substitute-bindings
                #:add-clause!
                #:get-clauses
                #:remove-clauses-with-arity!
                #:prove-all
                #:*current-clause-database*
                #:ground-p
                #:named-variable-p
                #:atom-p
                #:variables-in
                #:replace-anonymous-variables
                #:call-with-current-choice-point
                #:failure-p
                #:success-p
                #:success-bindings
                #:success-continuation
                #:object->string)
  (:import-from :prolog/test/utilities
                #:solve-first
                #:solve-all
                #:solve-count)
  (:import-from :fiveam
                #:def-suite
                #:in-suite
                #:is
                #:is-true
                #:is-false
                #:signals
                #:finishes
                #:test
                #:run!
                #:run
                #:pass)
  (:export #:run-all-tests
           #:*prolog-test-suite*))

(in-package :prolog/test/all)

;;; Test Suite Definition
;;; =====================

(def-suite *prolog-test-suite*
  :description "Main test suite for Athena Prolog Engine")

;;; Test Runner
;;; ============

(defun run-all-tests ()
  "Run all Prolog tests and return success status"
  (format t "~%Running Athena Prolog Engine Test Suite~%")
  (format t "=======================================~%")

  (let ((results (run! '*prolog-test-suite*)))
    (if results
        (progn
          (format t "~%✅ All tests passed!~%")
          t)
        (progn
          (format t "~%❌ Some tests failed!~%")
          nil))))

;;; Script Support
;;; ==============

#+sbcl
(defun main ()
  "Main entry point for SBCL script execution"
  (when (member "--test" sb-ext:*posix-argv* :test #'string=)
    (let ((success (run-all-tests)))
      (sb-ext:quit :unix-status (if success 0 1)))))

;;; Direct Execution Support
;;; ========================

(eval-when (:execute)
  (when (and (boundp '*load-pathname*) 
             *load-pathname*
             (not (find-package :asdf)))
    (run-all-tests)))

(load #p"./test/core.lisp")
(load #p"./test/primitive.lisp")
(load #p"./test/stdlib.lisp")
