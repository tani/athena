;;; test-fiveam.lisp — FiveAM test runner for Athena Prolog Engine
;;; This script demonstrates running FiveAM tests with the ASDF-loaded system

;; Load ASDF and FiveAM
(require :asdf)

;; Add current directory to ASDF registry
(push (uiop:getcwd) asdf:*central-registry*)

;; Load our system
(format t "Loading Prolog system via ASDF...~%")
(asdf:load-system :prolog)

;; Load FiveAM
(format t "Loading FiveAM...~%")
(asdf:load-system :fiveam)

;; Define a test package
(defpackage :prolog-fiveam-tests
  (:use :cl :fiveam :prolog)
  (:export #:run-tests))

(in-package :prolog-fiveam-tests)

;; Define test suite
(def-suite :prolog-tests
  :description "FiveAM tests for Athena Prolog Engine")

(in-suite :prolog-tests)

;; Helper functions for testing
(defun solve-first (goals var)
  "Get the first solution for GOALS and substitute VAR"
  (let ((result nil))
    (prolog:solve goals
                  (lambda (solution)
                    (setf result (prolog:substitute-bindings solution var)))
                  (lambda () nil))
    result))

(defun solve-all (goals var)
  "Get all solutions for GOALS and substitute VAR"
  (let ((results '()))
    (prolog:solve goals
                  (lambda (solution)
                    (push (prolog:substitute-bindings solution var) results))
                  (lambda () nil))
    (reverse results)))

(defun solve-success-p (goals)
  "Check if GOALS succeeds at least once"
  (let ((success nil))
    (prolog:solve goals
                  (lambda (solution)
                    (declare (ignore solution))
                    (setf success t))
                  (lambda () nil))
    success))

;; Core unification tests
(test variable-p-tests
  "Test variable-p predicate"
  (is (eq t (prolog:variable-p '?x)) "variable-p success for '?x")
  (is (eq t (prolog:variable-p '?)) "variable-p success for '?")
  (is (eq nil (prolog:variable-p 'foo)) "variable-p failure for non-var")
  (is (eq nil (prolog:variable-p '(?x))) "variable-p failure for list"))

(test unification-tests
  "Test basic unification"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (test-unify a))
    (prolog:<- (test-unify ?x) (= ?x b))
    
    (is (eq 'a (solve-first '((test-unify ?result)) '?result))
        "Simple fact unification")
    (is (equal '(a b) (solve-all '((test-unify ?result)) '?result))
        "Multiple solutions from unification")))

(test basic-facts
  "Test basic fact definition and querying"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (parent john mary))
    (prolog:<- (parent mary susan))
    
    (is (eq 'mary (solve-first '((parent john ?child)) '?child))
        "Query simple fact")
    (is (solve-success-p '((parent john mary)))
        "Fact verification succeeds")
    (is (not (solve-success-p '((parent john bob))))
        "Non-existent fact fails")))

(test rule-inference
  "Test rule definition and inference"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (parent john mary))
    (prolog:<- (parent mary susan))
    (prolog:<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
    
    (is (eq 'susan (solve-first '((grandparent john ?grandchild)) '?grandchild))
        "Rule inference works")
    (is (solve-success-p '((grandparent john susan)))
        "Inferred fact verification")))

(test arithmetic-predicates
  "Test arithmetic and comparison predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (is (solve-success-p '((is ?x (+ 2 3)) (= ?x 5)))
        "Arithmetic evaluation with is/2")
    (is (solve-success-p '((> 5 3)))
        "Greater than comparison")
    (is (not (solve-success-p '((> 3 5))))
        "Greater than comparison fails correctly")
    (is (solve-success-p '((number 42)))
        "Number type checking")))

(test list-operations
  "Test list manipulation predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (is (solve-success-p '((member a (a b c))))
        "Member predicate success")
    (is (not (solve-success-p '((member d (a b c)))))
        "Member predicate failure")
    (is (equal '(a b c) (solve-first '((append (a b) (c) ?result)) '?result))
        "Append operation")
    (is (equal '((a b) (c)) (solve-first '((append ?x ?y (a b c))) '(?x ?y)))
        "Append with multiple solutions")))

(test cut-behavior
  "Test cut (!) operator behavior"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (choice a))
    (prolog:<- (choice b))
    (prolog:<- (choice c))
    (prolog:<- (first-only ?x) (choice ?x) !)
    
    (is (eq 'a (solve-first '((first-only ?x)) '?x))
        "Cut returns first solution")
    (is (equal '(a) (solve-all '((first-only ?x)) '?x))
        "Cut prevents backtracking")))

(test meta-predicates
  "Test meta-predicates like findall"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (color red))
    (prolog:<- (color green))
    (prolog:<- (color blue))
    
    (is (equal '(red green blue) 
               (solve-first '((findall ?x (color ?x) ?colors)) '?colors))
        "Findall collects all solutions")
    (is (equal '(red green blue)
               (solve-first '((bagof ?x (color ?x) ?colors)) '?colors))
        "Bagof collects all solutions")))

(test recursion-and-performance
  "Test recursive predicates and performance"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (countdown 0))
    (prolog:<- (countdown ?n) 
               (number ?n) 
               (> ?n 0) 
               (is ?n1 (- ?n 1)) 
               (countdown ?n1))
    
    (is (solve-success-p '((countdown 10)))
        "Small recursion test")
    (is (solve-success-p '((countdown 50)))
        "Deep recursion test (performance)")))

(test built-in-predicates
  "Test various built-in predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (is (solve-success-p '((true)))
        "true predicate succeeds")
    (is (not (solve-success-p '((fail))))
        "fail predicate fails")
    (is (solve-success-p '((atom hello)))
        "atom/1 type check")
    (is (solve-success-p '((var ?x)))
        "var/1 for unbound variable")
    (is (not (solve-success-p '((var hello))))
        "var/1 fails for bound term")))

(test control-flow
  "Test control flow predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (is (solve-success-p '((if (= 1 1) (= ?x success) (= ?x fail)) (= ?x success)))
        "if-then-else true branch")
    (is (solve-success-p '((if (= 1 2) (= ?x success) (= ?x fail)) (= ?x fail)))
        "if-then-else false branch")
    (is (solve-success-p '((not (= 1 2))))
        "not/1 succeeds when goal fails")
    (is (not (solve-success-p '((not (= 1 1)))))
        "not/1 fails when goal succeeds")))

;; Test runner function
(defun run-tests ()
  "Run all FiveAM tests for the Prolog engine"
  (format t "~%========================================~%")
  (format t "  Athena Prolog Engine - FiveAM Tests~%")
  (format t "========================================~%")
  
  (let ((results (run :prolog-tests)))
    (format t "~%========================================~%")
    (format t "  Test Results~%")
    (format t "========================================~%")
    
    (if results
        (progn
          (format t "✅ All FiveAM tests passed!~%")
          (format t "Prolog engine functionality verified with FiveAM.~%")
          t)
        (progn
          (format t "❌ Some FiveAM tests failed!~%")
          (format t "Check the output above for details.~%")
          nil))))

;; Export the test runner
(export 'run-tests)

;; Auto-run when loaded as script
(eval-when (:execute)
  (when (and (boundp '*load-pathname*) *load-pathname*)
    (run-tests)))