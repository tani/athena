;;; test/prolog/unified-tests.lisp — Unified Common Lisp test suite with improved patterns
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file demonstrates the new unified testing approach using shared utilities
;;; and consistent patterns across all test suites.

(require :asdf)
(asdf:load-system :prolog)

;; Load our shared test utilities
(load (merge-pathnames "shared/test-utilities.lisp" 
                       (make-pathname :directory (pathname-directory *load-truename*))))

(defpackage :athena/test/unified
  (:use :common-lisp :fiveam :prolog :athena/test/utilities)
  (:export :run-unified-tests))

(in-package :athena/test/unified)

;; Define the main test suite
(def-suite :athena-unified-tests
  :description "Unified test suite demonstrating improved testing patterns")

(in-suite :athena-unified-tests)

;;; Core Engine Tests
;;; =================

(test core-unification-tests
  "Test core unification with utilities"
  (with-fresh-database
    ;; Test basic unification
    (assert-solutions '((= ?x foo)) '?x '(foo) "Basic variable binding")
    (assert-solutions '((= (a ?y) (a (b)))) '?y '((b)) "Complex term unification")
    
    ;; Test unification failures
    (assert-no-solutions '((= foo bar)) "Atom unification failure")
    (assert-no-solutions '((= (a b) (a c))) "Structure unification failure")))

(test arithmetic-predicates-tests
  "Test arithmetic predicates with performance monitoring"
  (with-fresh-database
    ;; Test successful comparisons
    (assert-solutions '((> 5 3)) 'dummy '(dummy) "Greater than success")
    (assert-solutions '((< 3 5)) 'dummy '(dummy) "Less than success")
    
    ;; Test failed comparisons
    (assert-no-solutions '((> 3 5)) "Greater than failure")
    (assert-no-solutions '((< 5 3)) "Less than failure")
    
    ;; Performance test
    (performance-test "Arithmetic comparison performance"
      (multiple-value-bind (avg min max)
          (benchmark-predicate 'arithmetic-gt '((> 100 50)) 50 5)
        (is (< avg 1.0) "Arithmetic should be fast (< 1ms average)")))))

(test meta-predicates-tests
  "Test meta-predicates with comprehensive assertions"
  (with-fresh-database
    ;; Set up test data
    (<- (parent john mary))
    (<- (parent john mike))
    (<- (parent mary susan))
    
    ;; Test findall
    (assert-solution-count '((findall ?x (parent john ?x) ?children)) 1 
                          "Findall should produce one solution")
    (let ((children (solve-first '((findall ?x (parent john ?x) ?children)) '?children)))
      (is (= (length children) 2) "John should have 2 children")
      (is (member 'mary children) "Mary should be John's child")
      (is (member 'mike children) "Mike should be John's child"))
    
    ;; Test call predicate
    (assert-solutions '((call (parent john ?x))) '?x '(mary mike) "Call should work with goals")))

(test backtracking-and-control-tests
  "Test backtracking and control flow"
  (with-fresh-database
    ;; Test repeat predicate
    (let ((count 0))
      (solve '((repeat))
             (lambda (solution)
               (declare (ignore solution))
               (incf count)
               (when (>= count 3) (return-from backtracking-and-control-tests)))
             (lambda () nil))
      (is (>= count 3) "Repeat should generate multiple solutions"))
    
    ;; Test cut behavior
    (<- (test-cut ?x) (member ?x (a b c)) !)
    (assert-solution-count '((test-cut ?y)) 1 "Cut should limit solutions")))

;;; Performance and Stress Tests
;;; ============================

(test performance-benchmarks
  "Performance benchmarks for key operations"
  (with-fresh-database
    ;; Generate test data
    (dotimes (i 100)
      (<- (test-fact ,(intern (format nil "ITEM~D" i)))))
    
    ;; Benchmark database operations
    (performance-test "Database query performance"
      (multiple-value-bind (avg min max)
          (benchmark-predicate 'db-query '((test-fact item50)) 100 10)
        (is (< avg 0.1) "Database query should be very fast"))
      
      (multiple-value-bind (avg min max)
          (benchmark-predicate 'db-scan '((test-fact ?x)) 50 5)
        (is (< avg 1.0) "Database scan should be reasonably fast")))
    
    ;; Test with larger dataset
    (dotimes (i 1000)
      (<- (large-fact ,(intern (format nil "LARGE~D" i)))))
    
    (performance-test "Large dataset performance"
      (multiple-value-bind (avg min max)
          (benchmark-predicate 'large-scan '((large-fact ?x)) 20 3)
        (is (< avg 10.0) "Large dataset scan should complete within 10ms")))))

;;; Error Handling and Edge Cases
;;; =============================

(test error-handling-tests
  "Test error handling and edge cases"
  (with-fresh-database
    ;; Test with invalid expressions
    (signals error 
      (solve-count '((lisp ?x (undefined-function)))) 
      "Should signal error for undefined functions")
    
    ;; Test occurs check (if enabled)
    (let ((prolog/core:*current-occurs-check* t))
      (assert-no-solutions '((= ?x (f ?x))) "Occurs check should prevent infinite terms"))
    
    ;; Test deep recursion
    (dotimes (i 10)
      (<- (chain ,(intern (format nil "A~D" i)) ,(intern (format nil "A~D" (+ i 1))))))
    (<- (chain a10 a0))  ; Create a cycle
    
    ;; This should not crash (though it may not terminate)
    (finishes 
      (handler-case
          (with-timeout (1)  ; 1 second timeout
            (solve-count '((chain a0 ?x))))
        (timeout-error () 0))
      "Deep recursion should not crash the system")))

;;; Integration Tests
;;; =================

(test integration-tests
  "Integration tests combining multiple features"
  (with-fresh-database
    ;; Complex family tree
    (<- (parent tom john))
    (<- (parent john mary))
    (<- (parent mary susan))
    (<- (parent john mike))
    
    (<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))
    
    ;; Test complex queries
    (assert-solutions '((grandparent tom ?gchild)) '?gchild '(mary mike)
                     "Grandparent relationship")
    
    (assert-solutions '((ancestor tom ?desc)) '?desc '(john mary mike susan)
                     "Ancestor relationship with backtracking")
    
    ;; Test with arithmetic and logic
    (<- (age john 30))
    (<- (age mary 25))
    (<- (age mike 20))
    
    (<- (adult ?person) (age ?person ?years) (>= ?years 18))
    
    (assert-solution-count '((adult ?person)) 3 "All persons should be adults")
    
    ;; Test findall with complex goals
    (let ((adults (solve-first '((findall ?person (adult ?person) ?adults)) '?adults)))
      (is (= (length adults) 3) "Should find 3 adults")
      (is (every (lambda (person) (member person '(john mary mike))) adults)
          "All found persons should be in the expected list"))))

;;; Test Suite Runner
;;; =================

(defun run-unified-tests (&key (verbose t))
  "Run the unified test suite with optional verbose output."
  (when verbose
    (format t "~%=== Running Athena Unified Test Suite ===~%"))
  
  ;; Reset environment
  (reset-test-environment)
  
  ;; Run tests
  (let ((results (run! :athena-unified-tests)))
    (when verbose
      (format t "~%=== Test Results ===~%")
      (format t "Tests run: ~D~%" (length (fiveam::results results)))
      (format t "Passed: ~D~%" (count-if (lambda (r) (typep r 'fiveam::test-passed)) 
                                         (fiveam::results results)))
      (format t "Failed: ~D~%" (count-if (lambda (r) (typep r 'fiveam::test-failure)) 
                                         (fiveam::results results)))
      
      ;; Generate performance report
      (generate-performance-report))
    
    results))

;; Utility for running from command line
(defun main ()
  "Main entry point for command-line test execution."
  (handler-case
      (let ((results (run-unified-tests :verbose t)))
        (if (every (lambda (r) (typep r 'fiveam::test-passed)) 
                   (fiveam::results results))
            (progn
              (format t "~%All tests passed!~%")
              (sb-ext:exit :code 0))
            (progn
              (format t "~%Some tests failed!~%")
              (sb-ext:exit :code 1))))
    (error (e)
      (format t "~%Error running tests: ~A~%" e)
      (sb-ext:exit :code 2))))

;; Export main for potential use as script
#+sbcl
(defun run-script ()
  (main))