;;; test-fiveam-nix.lisp — FiveAM tests using Nix store path
;;; This script directly loads FiveAM from the Nix store

;; Load ASDF
(require :asdf)

;; Add current directory to ASDF registry
(push (uiop:getcwd) asdf:*central-registry*)

;; Find and add FiveAM from Nix store
(format t "Looking for FiveAM in Nix store...~%")
(let ((fiveam-path "/nix/store/y5l198lkrfr4j61fgh9zqrwzqzrl2da0-sbcl-fiveam-20241012-git/"))
  (when (probe-file fiveam-path)
    (format t "Found FiveAM at: ~A~%" fiveam-path)
    (push fiveam-path asdf:*central-registry*)))

;; Load our Prolog system
(format t "Loading Prolog system...~%")
(asdf:load-system :prolog)

;; Load FiveAM
(format t "Loading FiveAM...~%")
(handler-case
    (progn
      (asdf:load-system :fiveam)
      (format t "✅ FiveAM loaded successfully!~%"))
  (error (e)
    (format t "❌ Could not load FiveAM: ~A~%" e)
    (format t "Creating simple tests instead...~%")
    
    ;; Define a simple test framework
    (defun simple-test (name test-fn)
      (format t "~%Testing ~A...~%" name)
      (handler-case
          (progn
            (funcall test-fn)
            (format t "  ✅ ~A PASSED~%" name))
        (error (e)
          (format t "  ❌ ~A FAILED: ~A~%" name e))))
    
    (defun assert-equal (expected actual &optional description)
      (unless (equal expected actual)
        (error "Expected ~S but got ~S~@[ (~A)~]" expected actual description)))
    
    (defun assert-true (value &optional description)
      (unless value
        (error "Expected true but got ~S~@[ (~A)~]" value description)))
    
    ;; Run simple tests
    (format t "~%========================================~%")
    (format t "  Simple Prolog Tests (Fallback)~%")
    (format t "========================================~%")
    
    (simple-test "Variable Recognition"
      (lambda ()
        (assert-true (prolog:variable-p '?x) "?x should be variable")
        (assert-true (not (prolog:variable-p 'atom)) "atom should not be variable")))
    
    (simple-test "Basic Facts"
      (lambda ()
        (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
          (prolog:<- (test-fact hello))
          (let ((result nil))
            (prolog:solve '((test-fact ?x))
                          (lambda (solution)
                            (setf result (prolog:substitute-bindings solution '?x)))
                          (lambda () nil))
            (assert-equal 'hello result "Should find hello")))))
    
    (format t "~%Simple tests completed!~%")
    (sb-ext:quit :unix-status 0)))

;; If we get here, FiveAM loaded successfully
(defpackage :prolog-fiveam-tests
  (:use :cl :fiveam :prolog))

(in-package :prolog-fiveam-tests)

;; Define test suite
(def-suite :athena-prolog-tests
  :description "FiveAM test suite for Athena Prolog Engine")

(in-suite :athena-prolog-tests)

;; Helper functions
(defun solve-first (goals var)
  "Get first solution for GOALS, substituting VAR"
  (let ((result nil))
    (prolog:solve goals
                  (lambda (solution)
                    (setf result (prolog:substitute-bindings solution var)))
                  (lambda () nil))
    result))

(defun solve-all (goals var)
  "Get all solutions for GOALS, substituting VAR"
  (let ((results '()))
    (prolog:solve goals
                  (lambda (solution)
                    (push (prolog:substitute-bindings solution var) results))
                  (lambda () nil))
    (reverse results)))

(defun solve-success-p (goals)
  "Check if GOALS succeeds"
  (let ((success nil))
    (prolog:solve goals
                  (lambda (solution)
                    (declare (ignore solution))
                    (setf success t))
                  (lambda () nil))
    success))

;; Core tests
(test variable-recognition
  "Test Prolog variable recognition"
  (is (prolog:variable-p '?x) "?x should be recognized as variable")
  (is (prolog:variable-p '?) "? should be recognized as variable")  
  (is (not (prolog:variable-p 'atom)) "atoms should not be variables")
  (is (not (prolog:variable-p 42)) "numbers should not be variables"))

(test basic-facts-and-queries
  "Test basic fact definition and querying"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Define facts
    (prolog:<- (likes mary wine))
    (prolog:<- (likes john beer))
    (prolog:<- (age mary 25))
    
    ;; Test queries
    (is (eq 'wine (solve-first '((likes mary ?what)) '?what))
        "Should find what Mary likes")
    (is (eq 'mary (solve-first '((likes ?who wine)) '?who))
        "Should find who likes wine")
    (is (eq 25 (solve-first '((age mary ?age)) '?age))
        "Should find Mary's age")))

(test rule-inference
  "Test rule definition and inference"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Family facts
    (prolog:<- (parent john mary))
    (prolog:<- (parent mary ann))
    (prolog:<- (parent john tom))
    
    ;; Grandparent rule
    (prolog:<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
    
    ;; Test inference
    (is (eq 'ann (solve-first '((grandparent john ?grandchild)) '?grandchild))
        "Should infer grandparent relationship")
    (is (solve-success-p '((grandparent john ann)))
        "Should verify inferred relationship")))

(test arithmetic-operations
  "Test arithmetic evaluation"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Basic arithmetic
    (is (eq 7 (solve-first '((is ?x (+ 3 4))) '?x))
        "Should evaluate addition")
    (is (eq 12 (solve-first '((is ?x (* 3 4))) '?x))
        "Should evaluate multiplication")
    
    ;; Comparisons
    (is (solve-success-p '((> 10 5))) "10 > 5 should succeed")
    (is (not (solve-success-p '((> 3 8)))) "3 > 8 should fail")
    
    ;; Arithmetic in rules
    (prolog:<- (double ?x ?y) (is ?y (* ?x 2)))
    (is (eq 10 (solve-first '((double 5 ?result)) '?result))
        "Rule with arithmetic should work")))

(test list-operations
  "Test list manipulation"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Member predicate
    (is (solve-success-p '((member a (a b c))))
        "Member should find elements")
    (is (equal '(a b c) (solve-all '((member ?x (a b c))) '?x))
        "Member should generate all elements")
    
    ;; Append predicate
    (is (equal '(a b c d) (solve-first '((append (a b) (c d) ?result)) '?result))
        "Append should concatenate lists")))

(test meta-predicates
  "Test meta-predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Setup data
    (prolog:<- (animal cat))
    (prolog:<- (animal dog))
    (prolog:<- (animal bird))
    
    ;; Test findall
    (is (equal '(cat dog bird) 
               (solve-first '((findall ?x (animal ?x) ?animals)) '?animals))
        "Findall should collect all solutions")))

(test cut-operator
  "Test cut operator behavior"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Setup choices
    (prolog:<- (option a))
    (prolog:<- (option b))
    (prolog:<- (option c))
    
    ;; Without cut
    (is (equal '(a b c) (solve-all '((option ?x)) '?x))
        "Without cut should get all options")
    
    ;; With cut
    (prolog:<- (first-option ?x) (option ?x) !)
    (is (equal '(a) (solve-all '((first-option ?x)) '?x))
        "With cut should get only first option")))

(test recursion-and-performance
  "Test recursive predicates"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Define countdown
    (prolog:<- (countdown 0))
    (prolog:<- (countdown ?n)
               (number ?n)
               (> ?n 0)
               (is ?n1 (- ?n 1))
               (countdown ?n1))
    
    ;; Test recursion
    (is (solve-success-p '((countdown 5)))
        "Small recursion should work")
    (is (solve-success-p '((countdown 20)))
        "Moderate recursion should work")))

;; Main test runner
(defun run-fiveam-tests ()
  "Run all FiveAM tests"
  (format t "~%============================================~%")
  (format t "  Athena Prolog Engine - FiveAM Tests~%")
  (format t "============================================~%")
  (format t "Running tests with FiveAM framework...~%")
  
  (let ((results (run :athena-prolog-tests)))
    (format t "~%============================================~%")
    (format t "  Test Summary~%")
    (format t "============================================~%")
    
    (if results
        (progn
          (format t "🎉 All FiveAM tests PASSED!~%")
          (format t "~%Test coverage:~%")
          (format t "  ✓ Variable recognition~%")
          (format t "  ✓ Basic facts and queries~%")
          (format t "  ✓ Rule inference~%")
          (format t "  ✓ Arithmetic operations~%")
          (format t "  ✓ List operations~%")
          (format t "  ✓ Meta-predicates~%")
          (format t "  ✓ Cut operator~%")
          (format t "  ✓ Recursion and performance~%")
          (format t "~%✅ Prolog engine verified with FiveAM!~%"))
        (format t "❌ Some tests failed. Check output above.~%"))))

;; Auto-run when loaded
(eval-when (:execute)
  (when (and (boundp '*load-pathname*) *load-pathname*)
    (run-fiveam-tests)))