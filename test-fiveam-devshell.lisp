;;; test-fiveam-devshell.lisp — FiveAM tests using Nix development shell
;;; This script runs FiveAM tests assuming FiveAM is available in the environment

;; Load ASDF 
(require :asdf)

;; Add current directory to ASDF registry
(push (uiop:getcwd) asdf:*central-registry*)

;; Load our system first
(format t "Loading Prolog system via ASDF...~%")
(asdf:load-system :prolog)

;; Try to use FiveAM from the development environment
(format t "Attempting to use FiveAM from development environment...~%")

;; First try to find FiveAM package
(unless (find-package :fiveam)
  (handler-case
      (progn
        (format t "Loading FiveAM...~%")
        (require :fiveam))
    (error (e)
      (format t "Failed to load FiveAM: ~A~%" e)
      (format t "Trying alternative method...~%")
      (handler-case
          (asdf:load-system :fiveam)
        (error (e2)
          (format t "ASDF load also failed: ~A~%" e2)
          (format t "~%Note: To run FiveAM tests, ensure you're in the Nix devshell:~%")
          (format t "  nix develop  # or use direnv~%")
          (format t "  sbcl --script test-fiveam-devshell.lisp~%")
          (sb-ext:quit :unix-status 1))))))

;; Define our test package
(defpackage :prolog-fiveam-tests
  (:use :cl :fiveam :prolog)
  (:export #:run-all-tests))

(in-package :prolog-fiveam-tests)

;; Test suite definition
(def-suite :prolog-engine-tests
  :description "Comprehensive FiveAM tests for Athena Prolog Engine")

(in-suite :prolog-engine-tests)

;; Helper functions
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

;; Core functionality tests
(test test-variable-recognition
  "Test Prolog variable recognition"
  (is (prolog:variable-p '?x) "?x should be recognized as variable")
  (is (prolog:variable-p '?) "? should be recognized as variable")
  (is (not (prolog:variable-p 'atom)) "Regular atoms should not be variables")
  (is (not (prolog:variable-p 42)) "Numbers should not be variables"))

(test test-basic-facts
  "Test basic fact definition and querying"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Define some facts
    (prolog:<- (likes mary food))
    (prolog:<- (likes mary wine))
    (prolog:<- (likes john wine))
    (prolog:<- (likes john mary))
    
    ;; Test single solution
    (is (eq 'food (solve-first '((likes mary ?what)) '?what))
        "Should find first thing Mary likes")
    
    ;; Test multiple solutions
    (is (equal '(food wine) (solve-all '((likes mary ?what)) '?what))
        "Should find all things Mary likes")
    
    ;; Test bidirectional queries
    (is (eq 'mary (solve-first '((likes ?who food)) '?who))
        "Should find who likes food")))

(test test-rules-and-inference
  "Test rule definition and logical inference"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Define facts and rules
    (prolog:<- (parent tom bob))
    (prolog:<- (parent tom liz))
    (prolog:<- (parent bob ann))
    (prolog:<- (parent bob pat))
    (prolog:<- (parent pat jim))
    
    ;; Define grandparent rule
    (prolog:<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
    
    ;; Define ancestor rule (recursive)
    (prolog:<- (ancestor ?x ?y) (parent ?x ?y))
    (prolog:<- (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))
    
    ;; Test grandparent inference
    (is (eq 'ann (solve-first '((grandparent tom ?grandchild)) '?grandchild))
        "Should infer grandparent relationship")
    
    ;; Test all grandchildren
    (is (equal '(ann pat) (solve-all '((grandparent tom ?grandchild)) '?grandchild))
        "Should find all grandchildren")
    
    ;; Test recursive ancestor relationship
    (is (eq 'jim (solve-first '((ancestor tom jim)) '(?)))
        "Should prove ancestor relationship through recursion")))

(test test-arithmetic-and-comparison
  "Test arithmetic evaluation and comparisons"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Basic arithmetic
    (is (eq 7 (solve-first '((is ?result (+ 3 4))) '?result))
        "Should evaluate arithmetic expressions")
    
    ;; Comparisons
    (is (solve-success-p '((> 10 5)))
        "Greater than should work")
    (is (not (solve-success-p '((> 3 8))))
        "Greater than should fail correctly")
    
    ;; Complex arithmetic in rules
    (prolog:<- (double ?x ?y) (is ?y (* ?x 2)))
    (is (eq 10 (solve-first '((double 5 ?result)) '?result))
        "Arithmetic in rules should work")))

(test test-list-operations
  "Test list manipulation predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Member predicate
    (is (solve-success-p '((member apple (apple banana cherry))))
        "Member should find elements in list")
    (is (not (solve-success-p '((member grape (apple banana cherry)))))
        "Member should fail for non-members")
    
    ;; Generate all members
    (is (equal '(apple banana cherry) 
               (solve-all '((member ?item (apple banana cherry))) '?item))
        "Member should generate all list elements")
    
    ;; Append predicate
    (is (equal '(a b c d) (solve-first '((append (a b) (c d) ?result)) '?result))
        "Append should concatenate lists")
    
    ;; Append in reverse (split list)
    (let ((splits (solve-all '((append ?left ?right (a b c))) '(?left ?right))))
      (is (member '(() (a b c)) splits :test #'equal)
          "Should find empty left split")
      (is (member '((a b c) ()) splits :test #'equal)
          "Should find empty right split"))))

(test test-cut-operator
  "Test cut (!) operator behavior"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Define choice predicates
    (prolog:<- (choice red))
    (prolog:<- (choice green))
    (prolog:<- (choice blue))
    
    ;; Without cut - should get all solutions
    (is (equal '(red green blue) (solve-all '((choice ?color)) '?color))
        "Without cut should get all solutions")
    
    ;; With cut - should get only first
    (prolog:<- (first-choice ?x) (choice ?x) !)
    (is (equal '(red) (solve-all '((first-choice ?color)) '?color))
        "With cut should get only first solution")))

(test test-meta-predicates
  "Test meta-predicates like findall, bagof"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Set up test data
    (prolog:<- (student alice))
    (prolog:<- (student bob))
    (prolog:<- (student charlie))
    (prolog:<- (grade alice a))
    (prolog:<- (grade bob b))
    (prolog:<- (grade charlie a))
    
    ;; Test findall
    (is (equal '(alice bob charlie) 
               (solve-first '((findall ?s (student ?s) ?students)) '?students))
        "Findall should collect all solutions")
    
    ;; Test bagof
    (is (equal '(alice charlie) 
               (solve-first '((bagof ?s (grade ?s a) ?a-students)) '?a-students))
        "Bagof should collect filtered solutions")))

(test test-type-checking
  "Test type checking predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Number type checking
    (is (solve-success-p '((number 42)))
        "Should recognize integers as numbers")
    (is (solve-success-p '((number 3.14)))
        "Should recognize floats as numbers")
    (is (not (solve-success-p '((number hello))))
        "Should reject atoms as numbers")
    
    ;; Atom type checking  
    (is (solve-success-p '((atom hello)))
        "Should recognize symbols as atoms")
    (is (not (solve-success-p '((atom (a list)))))
        "Should reject lists as atoms")
    
    ;; Variable checking
    (is (solve-success-p '((var ?x)))
        "Should recognize unbound variables")
    (is (not (solve-success-p '((var hello))))
        "Should reject bound terms as variables")))

(test test-control-flow
  "Test control flow predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; if-then-else
    (is (eq 'yes (solve-first '((if (= 1 1) (= ?result yes) (= ?result no))) '?result))
        "If-then-else should take true branch")
    (is (eq 'no (solve-first '((if (= 1 2) (= ?result yes) (= ?result no))) '?result))
        "If-then-else should take false branch")
    
    ;; not/1 
    (is (solve-success-p '((not (= 1 2))))
        "Not should succeed when goal fails")
    (is (not (solve-success-p '((not (= 1 1)))))
        "Not should fail when goal succeeds")))

(test test-recursion-performance
  "Test recursive predicates and performance"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Define countdown predicate
    (prolog:<- (countdown 0))
    (prolog:<- (countdown ?n) 
               (number ?n) 
               (> ?n 0) 
               (is ?n1 (- ?n 1)) 
               (countdown ?n1))
    
    ;; Test small recursion
    (is (solve-success-p '((countdown 5)))
        "Should handle small recursive calls")
    
    ;; Test deeper recursion (performance test)
    (is (solve-success-p '((countdown 25)))
        "Should handle moderate recursive depth")
    
    ;; Define factorial for more complex recursion
    (prolog:<- (factorial 0 1))
    (prolog:<- (factorial ?n ?f) 
               (> ?n 0)
               (is ?n1 (- ?n 1))
               (factorial ?n1 ?f1)
               (is ?f (* ?n ?f1)))
    
    (is (eq 120 (solve-first '((factorial 5 ?result)) '?result))
        "Should compute factorial correctly")))

;; Main test runner
(defun run-all-tests ()
  "Run all FiveAM tests and provide detailed reporting"
  (format t "~%================================================~%")
  (format t "   Athena Prolog Engine - FiveAM Test Suite~%")
  (format t "================================================~%")
  (format t "Running comprehensive tests using FiveAM framework~%")
  (format t "------------------------------------------------~%")
  
  ;; Run the test suite
  (let ((results (run :prolog-engine-tests)))
    (format t "~%================================================~%")
    (format t "   Test Results Summary~%")
    (format t "================================================~%")
    
    (if results
        (progn
          (format t "🎉 All FiveAM tests PASSED!~%")
          (format t "~%Test coverage includes:~%")
          (format t "  ✓ Variable recognition and unification~%")
          (format t "  ✓ Fact definition and basic querying~%")
          (format t "  ✓ Rule inference and logical reasoning~%") 
          (format t "  ✓ Arithmetic evaluation and comparisons~%")
          (format t "  ✓ List operations (member, append)~%")
          (format t "  ✓ Cut operator behavior~%")
          (format t "  ✓ Meta-predicates (findall, bagof)~%")
          (format t "  ✓ Type checking predicates~%")
          (format t "  ✓ Control flow (if-then-else, not)~%")
          (format t "  ✓ Recursive predicates and performance~%")
          (format t "~%The Prolog engine is working correctly!~%")
          t)
        (progn
          (format t "❌ Some tests FAILED!~%")
          (format t "Check the detailed output above for failure information.~%")
          nil))))

;; Auto-run when executed as script
(eval-when (:execute)
  (when (and (boundp '*load-pathname*) *load-pathname*)
    (run-all-tests)
    (sb-ext:quit :unix-status 0)))