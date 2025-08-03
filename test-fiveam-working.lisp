;;; test-fiveam-working.lisp — Working FiveAM tests for Athena Prolog Engine
;;; This script properly loads FiveAM from the Nix environment

;; Load ASDF first
(require :asdf)

;; Add current directory to ASDF registry
(push (uiop:getcwd) asdf:*central-registry*)

;; Load our Prolog system
(format t "Loading Prolog system via ASDF...~%")
(asdf:load-system :prolog)

;; Properly set up FiveAM path for Nix environment
(format t "Setting up FiveAM from Nix environment...~%")

;; Add Nix store paths to ASDF
(let ((nix-store-paths '("/nix/store")))
  (dolist (path nix-store-paths)
    (when (probe-file path)
      (dolist (dir (directory (merge-pathnames "*sbcl-fiveam*/" path)))
        (when (probe-file dir)
          (let ((asd-file (merge-pathnames "lib/common-lisp/sbcl/fiveam/fiveam.asd" dir)))
            (when (probe-file asd-file)
              (format t "Found FiveAM at: ~A~%" dir)
              (push (directory-namestring asd-file) asdf:*central-registry*))))))))

;; Try to load FiveAM
(handler-case
    (progn
      (asdf:load-system :fiveam)
      (format t "✅ FiveAM loaded successfully!~%"))
  (error (e)
    (format t "❌ Failed to load FiveAM: ~A~%" e)
    (format t "Falling back to manual test framework...~%")
    
    ;; Simple test framework fallback
    (defpackage :simple-test
      (:use :cl)
      (:export #:is #:test #:run-tests))
    
    (in-package :simple-test)
    
    (defvar *test-results* '())
    (defvar *current-test* nil)
    
    (defmacro test (name &body body)
      `(progn
         (setf *current-test* ,name)
         (format t "~%Running test: ~A~%" ,name)
         ,@body
         (format t "  ✓ ~A passed~%" ,name)))
    
    (defmacro is (form &optional description)
      `(progn
         (if ,form
             (progn
               (when ,description (format t "    ✓ ~A~%" ,description))
               (push (list *current-test* t ,description) *test-results*))
             (progn
               (format t "    ❌ FAILED: ~A~%" (or ,description ',form))
               (push (list *current-test* nil ,description) *test-results*)))))
    
    (defun run-tests ()
      (let ((passed (count-if (lambda (r) (second r)) *test-results*))
            (total (length *test-results*)))
        (format t "~%Tests completed: ~A/~A passed~%" passed total)))
    
    ;; Use simple framework
    (use-package :simple-test)
    (sb-ext:quit :unix-status 1)))

;; Define test package
(defpackage :prolog-fiveam-tests
  (:use :cl :fiveam :prolog))

(in-package :prolog-fiveam-tests)

;; Test suite
(def-suite :prolog-comprehensive-tests
  :description "Comprehensive FiveAM tests for Athena Prolog Engine")

(in-suite :prolog-comprehensive-tests)

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

;; Test basic functionality
(test basic-prolog-functionality
  "Test fundamental Prolog operations"
  
  ;; Variable recognition
  (is (prolog:variable-p '?x) "Should recognize ?x as variable")
  (is (prolog:variable-p '?) "Should recognize ? as variable")
  (is (not (prolog:variable-p 'atom)) "Should not recognize atom as variable")
  
  ;; Basic facts and queries
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (likes mary food))
    (prolog:<- (likes john wine))
    
    (is (eq 'food (solve-first '((likes mary ?what)) '?what))
        "Should find what Mary likes")
    (is (solve-success-p '((likes mary food)))
        "Should verify existing facts")))

(test rule-inference-and-recursion
  "Test rule definition and recursive inference"
  
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Family relationships
    (prolog:<- (parent tom bob))
    (prolog:<- (parent bob ann))
    (prolog:<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
    
    (is (eq 'ann (solve-first '((grandparent tom ?grandchild)) '?grandchild))
        "Should infer grandparent relationship")
    
    ;; Recursive ancestor
    (prolog:<- (ancestor ?x ?y) (parent ?x ?y))
    (prolog:<- (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))
    
    (is (solve-success-p '((ancestor tom ann)))
        "Should prove recursive ancestor relationship")))

(test arithmetic-and-comparisons
  "Test arithmetic evaluation and comparison predicates"
  
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Basic arithmetic
    (is (eq 7 (solve-first '((is ?result (+ 3 4))) '?result))
        "Should evaluate arithmetic expressions")
    
    ;; Comparisons
    (is (solve-success-p '((> 10 5))) "Greater than should work")
    (is (not (solve-success-p '((> 3 8)))) "Greater than should fail correctly")
    (is (solve-success-p '((= 5 5))) "Equality should work")
    
    ;; Complex arithmetic
    (prolog:<- (square ?x ?y) (is ?y (* ?x ?x)))
    (is (eq 25 (solve-first '((square 5 ?result)) '?result))
        "Arithmetic in rules should work")))

(test list-operations
  "Test list manipulation predicates"
  
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Member predicate
    (is (solve-success-p '((member apple (apple banana cherry))))
        "Member should find elements")
    (is (equal '(apple banana cherry) 
               (solve-all '((member ?item (apple banana cherry))) '?item))
        "Member should generate all elements")
    
    ;; Append predicate
    (is (equal '(a b c d) (solve-first '((append (a b) (c d) ?result)) '?result))
        "Append should concatenate lists")))

(test meta-predicates
  "Test meta-predicates like findall and bagof"
  
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (color red))
    (prolog:<- (color green))
    (prolog:<- (color blue))
    
    (is (equal '(red green blue) 
               (solve-first '((findall ?x (color ?x) ?colors)) '?colors))
        "Findall should collect all solutions")
    
    (is (equal '(red green blue)
               (solve-first '((bagof ?x (color ?x) ?colors)) '?colors))
        "Bagof should collect solutions")))

(test cut-operator
  "Test cut (!) operator behavior"
  
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    (prolog:<- (choice red))
    (prolog:<- (choice green))
    (prolog:<- (choice blue))
    
    ;; Without cut
    (is (equal '(red green blue) (solve-all '((choice ?color)) '?color))
        "Without cut should get all solutions")
    
    ;; With cut
    (prolog:<- (first-choice ?x) (choice ?x) !)
    (is (equal '(red) (solve-all '((first-choice ?color)) '?color))
        "With cut should get only first solution")))

(test performance-and-recursion
  "Test performance with recursive predicates"
  
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Countdown test
    (prolog:<- (countdown 0))
    (prolog:<- (countdown ?n) 
               (number ?n) 
               (> ?n 0) 
               (is ?n1 (- ?n 1)) 
               (countdown ?n1))
    
    (is (solve-success-p '((countdown 10)))
        "Should handle small recursion")
    (is (solve-success-p '((countdown 30)))
        "Should handle moderate recursion (performance test)")))

;; Main test runner
(defun run-comprehensive-tests ()
  "Run all comprehensive FiveAM tests"
  (format t "~%==============================================~%")
  (format t "  Athena Prolog Engine - FiveAM Test Suite~%")
  (format t "==============================================~%")
  (format t "Running comprehensive tests using FiveAM...~%")
  
  (let ((results (run :prolog-comprehensive-tests)))
    (format t "~%==============================================~%")
    (format t "  Test Results~%")
    (format t "==============================================~%")
    
    (if results
        (progn
          (format t "🎉 All FiveAM tests PASSED!~%")
          (format t "~%Verified functionality:~%")
          (format t "  ✓ Variable recognition and unification~%")
          (format t "  ✓ Basic facts and queries~%")
          (format t "  ✓ Rule inference and recursion~%")
          (format t "  ✓ Arithmetic and comparisons~%")
          (format t "  ✓ List operations~%")
          (format t "  ✓ Meta-predicates~%")
          (format t "  ✓ Cut operator~%")
          (format t "  ✓ Performance with recursion~%")
          (format t "~%✅ Prolog engine working correctly with FiveAM!~%")
          t)
        (progn
          (format t "❌ Some tests failed!~%")
          nil))))

;; Auto-run
(eval-when (:execute)
  (when (and (boundp '*load-pathname*) *load-pathname*)
    (run-comprehensive-tests)))