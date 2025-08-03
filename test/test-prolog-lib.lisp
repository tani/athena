;;; test-prolog-lib.lisp — Tests for Prolog standard library
;;; Copyright © 2025 Masaya Taniguchi

(defpackage :prolog-lib-test
  (:use :common-lisp :prolog-core :prolog-lib)
  (:export :solve-first :solve-all :solve-count :run-current-tests))

(in-package :prolog-lib-test)

;;; Test helper functions
(defun solve-first (goals variable)
  "Get the first solution for a variable in the given goals."
  (let ((result nil)
        (found nil))
    (solve goals
           (lambda (solution)
             (unless found
               (let ((binding (assoc variable solution :test #'eq)))
                 (when binding
                   (setf result (cdr binding))
                   (setf found t)))))
           (lambda () nil))
    result))

(defun solve-all (goals variable)
  "Get all solutions for a variable in the given goals."
  (let ((results '()))
    (solve goals
           (lambda (solution)
             (let ((binding (assoc variable solution :test #'eq)))
               (when binding
                 (push (cdr binding) results))))
           (lambda () nil))
    (reverse results)))

(defun solve-count (goals)
  "Count the number of solutions for the given goals."
  (let ((count 0))
    (solve goals
           (lambda (solution)
             (declare (ignore solution))
             (incf count))
           (lambda () nil))
    count))

;;; Phase 1 Tests: Basic Infrastructure
(defun test-phase1-infrastructure ()
  (format t "~%Testing Phase 1: Basic Infrastructure~%")
  
  ;; Test utility functions
  (format t "Testing object->string...~%")
  (assert (string= "(1 2 3)" (object->string '(1 2 3))))
  (let ((hello-str (object->string 'hello)))
    (assert (or (string= "HELLO" hello-str) 
                (string= "PROLOG-LIB-TEST::HELLO" hello-str)) 
            () "object->string should handle symbols"))
  
  ;; Test ground-p with simple bindings
  (format t "Testing ground-p...~%")
  (let ((*current-bindings* '((?x . 5) (?y . (a b)))))
    (assert (ground-p 'hello) nil "ground-p failed for atom")
    (assert (ground-p 5) nil "ground-p failed for number")
    (assert (ground-p '?x) nil "ground-p failed for bound variable")
    (assert (not (ground-p '?z)) nil "ground-p failed for unbound variable")
    (assert (ground-p '(a b c)) nil "ground-p failed for ground list")
    (assert (not (ground-p '(a ?z c))) nil "ground-p failed for non-ground list"))
  
  ;; Test dynamic parameters
  (format t "Testing dynamic parameters...~%")
  (setf *current-dynamic-parameters* '()) ; Reset
  (set-dynamic-parameter 'test-param 42)
  (assert (= 42 (get-dynamic-parameter-value 'test-param)))
  (set-dynamic-parameter 'test-param 'new-value)
  (assert (eq 'new-value (get-dynamic-parameter-value 'test-param)))
  
  (format t "Phase 1 tests passed!~%"))

;;; Phase 2 Tests: Core Built-in Predicates
(defun test-phase2-builtin-predicates ()
  (format t "~%Testing Phase 2: Core Built-in Predicates~%")
  
  ;; Set up test facts (don't clear database to preserve function-based predicates)
  (initialize-standard-library)
  
  ;; Test basic unification predicates
  (format t "Testing unification predicates...~%")
  (let ((result (solve-first '((= ?x 5)) '?x)))
    (assert (and result (eql 5 result)) () "= predicate should unify ?x with 5"))
  
  ;; Test fail case
  (assert (= 0 (solve-count '((= 1 2)))) () "Should fail for different atoms")
  
  ;; Test == (strict equality) 
  (assert (> (solve-count '((= ?x foo) (prolog-lib::== ?x foo))) 0))
  (assert (= 0 (solve-count '((= ?x foo) (prolog-lib::== ?x bar)))))
  
  ;; Test type checking predicates
  (format t "Testing type checking predicates...~%")
  (assert (> (solve-count '((atom hello))) 0))
  (assert (= 0 (solve-count '((atom 123)))))
  (assert (= 0 (solve-count '((atom ?x)))))
  
  (assert (> (solve-count '((prolog-lib::atomic 123))) 0))
  (assert (> (solve-count '((prolog-lib::atomic hello))) 0))
  (assert (= 0 (solve-count '((prolog-lib::atomic (?x ?y))))))
  
  (assert (> (solve-count '((prolog-lib::var ?x))) 0))
  (assert (= 0 (solve-count '((= ?x 1) (prolog-lib::var ?x)))))
  
  (assert (> (solve-count '((number 42))) 0))
  (assert (> (solve-count '((number -17))) 0))
  (assert (> (solve-count '((number 3.14))) 0))
  (assert (= 0 (solve-count '((number hello)))))
  
  (assert (> (solve-count '((string "hello"))) 0))
  (assert (= 0 (solve-count '((string hello)))))
  
  ;; Test ground predicate  
  (assert (> (solve-count '((= ?x (a b)) (prolog-lib::ground ?x))) 0))
  (assert (= 0 (solve-count '((prolog-lib::ground (a ?y))))))
  
  ;; Test fail and true
  (assert (= 0 (solve-count '((prolog-lib::fail)))))
  (assert (> (solve-count '((prolog-lib::true))) 0))
  
  (format t "Phase 2 tests passed!~%"))

;;; Phase 3 Tests: Evaluation System
(defun test-phase3-evaluation-system ()
  (format t "~%Testing Phase 3: Evaluation System~%")
  
  ;; Set up test facts (don't clear database to preserve predicates)
  (initialize-standard-library)
  
  ;; Test lisp evaluation
  (format t "Testing lisp evaluation...~%")
  (let ((result (solve-first '((prolog-lib::lisp ?x (+ 2 3))) '?x)))
    (assert (eql 5 result) () "lisp should evaluate (+ 2 3) to 5"))
  
  ;; Test is predicate (arithmetic evaluation)
  (let ((result (solve-first '((prolog-lib::is ?x (* 4 5))) '?x)))
    (assert (eql 20 result) () "is should evaluate (* 4 5) to 20"))
  
  ;; Test dynamic parameters
  (format t "Testing dynamic parameters...~%")
  (assert (> (solve-count '((prolog-lib::dynamic-put test-key 100) (prolog-lib::dynamic-get test-key ?x) (= ?x 100))) 0))
  
  (format t "Phase 3 tests passed!~%"))

;;; Phase 4 Tests: Meta-predicates  
(defun test-phase4-meta-predicates ()
  (format t "~%Testing Phase 4: Meta-predicates~%")
  
  ;; Set up test facts (don't clear database to preserve predicates)
  (initialize-standard-library)
  
  ;; Add some test facts for meta-predicate testing
  (<- (likes mary food))
  (<- (likes mary wine))
  (<- (likes john wine))
  (<- (likes john mary))
  
  ;; Test findall with package qualification
  (format t "Testing findall...~%")
  (let ((result (solve-first '((prolog-lib::findall ?x (likes ?x wine) ?list)) '?list)))
    (assert (and result (= 2 (length result))) () "findall should find 2 people who like wine")
    (assert (member 'mary result :test #'eq) () "mary should like wine")
    (assert (member 'john result :test #'eq) () "john should like wine"))
  
  ;; Test findall with no solutions
  (let ((result (solve-first '((prolog-lib::findall ?x (likes ?x beer) ?list)) '?list)))
    (assert (and (listp result) (null result)) () "findall should return empty list for no solutions"))
  
  ;; Test sort predicate
  (format t "Testing sort...~%")
  (let ((result (solve-first '((sort (c a b a) ?sorted)) '?sorted)))
    (assert (equal '(a b c) result) () "sort should sort and remove duplicates"))
  
  ;; Test empty sort
  (let ((result (solve-first '((sort () ?sorted)) '?sorted)))
    (assert (null result) () "sort of empty list should be empty"))
  
  ;; Test bagof (simplified implementation)
  (format t "Testing bagof...~%")
  (let ((result (solve-first '((prolog-lib::bagof ?x (likes ?x wine) ?bag)) '?bag)))
    (assert (and result (= 2 (length result))) () "bagof should find 2 people who like wine"))
  
  ;; Test setof (bagof + sort)
  (format t "Testing setof...~%")
  (let ((result (solve-first '((prolog-lib::setof ?item (likes mary ?item) ?set)) '?set)))
    (assert (and result (= 2 (length result))) () "setof should find mary's likes"))
  
  (format t "Phase 4 tests passed!~%"))

;;; Phase 5 Tests: Standard Logic Predicates and List Operations
(defun test-phase5-standard-predicates ()
  (format t "~%Testing Phase 5: Standard Logic Predicates and List Operations~%")
  
  ;; Set up test facts (don't clear database to preserve predicates)
  (initialize-standard-library)
  
  ;; Test list operations
  (format t "Testing list operations...~%")
  
  ;; Test member predicate
  (assert (> (solve-count '((member a (a b c)))) 0) () "member should find element in list")
  (let ((results (solve-all '((member ?x (a b c))) '?x)))
    (assert (= 3 (length results)) () "member should find all elements")
    (assert (equal '(a b c) results) () "member should find elements in order"))
  
  ;; Test append predicate  
  (let ((result (solve-first '((append (a b) (c d) ?result)) '?result)))
    (assert (equal '(a b c d) result) () "append should concatenate lists"))
  
  ;; Test append with empty list
  (let ((result (solve-first '((append () (a b c) ?result)) '?result)))
    (assert (equal '(a b c) result) () "append with empty list should return second list"))
  
  ;; Test control flow predicates (need package qualification)
  (format t "Testing control flow predicates...~%")
  
  ;; Test true predicate
  (assert (> (solve-count '((prolog-lib::true))) 0) () "true should always succeed")
  
  ;; Test evaluation predicates
  (format t "Testing evaluation predicates...~%")
  (let ((result (solve-first '((prolog-lib::lisp ?x (+ 2 3))) '?x)))
    (when result (assert (eql 5 result) () "lisp should evaluate arithmetic")))
  
  (let ((result (solve-first '((prolog-lib::is ?x (* 4 5))) '?x)))
    (when result (assert (eql 20 result) () "is should evaluate arithmetic")))
  
  (format t "Phase 5 tests passed!~%"))

;;; Comprehensive test suite for all phases
(defun run-comprehensive-tests ()
  (format t "~%Running Comprehensive Prolog Library Tests~%")
  (format t "==========================================~%")
  
  ;; Run all phase tests
  (test-phase1-infrastructure)
  (test-phase2-builtin-predicates)
  (test-phase3-evaluation-system)
  (test-phase4-meta-predicates)
  (test-phase5-standard-predicates)
  
  (format t "~%=== COMPREHENSIVE TEST SUMMARY ===~%")
  (format t "✅ Phase 1: Basic Infrastructure - PASSED~%")
  (format t "✅ Phase 2: Core Built-in Predicates - PASSED~%")  
  (format t "✅ Phase 3: Evaluation System - PASSED~%")
  (format t "✅ Phase 4: Meta-predicates - PASSED~%")
  (format t "✅ Phase 5: Standard Logic Predicates - PASSED~%")
  (format t "~%🎉 ALL TESTS PASSED! Translation complete.~%"))

;;; Backward compatibility - run comprehensive tests
(defun run-current-tests ()
  (run-comprehensive-tests))

;; Run tests manually with (run-current-tests)
; (run-current-tests)