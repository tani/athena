;;; stdlib.lisp — Standard library tests
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(in-package :prolog/test)

;; Define test suite for standard library
(def-suite *prolog-stdlib-tests* :in *prolog-test-suite*)
(in-suite *prolog-stdlib-tests*)

;; -----------------------------------------------------------
;; Control flow predicates
;; -----------------------------------------------------------

(test control-flow
  "Test control flow predicates"
  ;; Setup test data
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (<- (parent john mary))
    (<- (parent john michael))
    (<- (parent mary susan))
    
    ;; Test or predicate
    (is (eq 'ok (solve-first '((or (= ?r ok) (= ?r ng) (= ?r nope))) '?r)) "or variadic first succeeds")
    (is (equal '(ok also-ok stop) (solve-all '((or (= ?r ok) (= ?r also-ok) (= ?r stop))) '?r)) "or variadic all solutions")
    
    ;; Test and predicate  
    (is (equal '(ok yes) (solve-first '((and (= ?a ok) (= ?b yes))) '(?a ?b))) "and variadic success")
    (is (null (solve-all '((and (= 1 2) (= ?x 3))) '?x)) "and variadic failure")
    
    ;; Test not predicate
    (is (not (null (solve-all '((not (parent susan ?_))) 'dummy))) "not succeeds when goal fails")
    (is (null (solve-all '((not (parent john mary))) 'dummy)) "not fails when goal succeeds")
    
    ;; Test if-then-else
    (is (eq 'yes (solve-first '((if (= a a) (= ?r yes) (= ?r no))) '?r)) "if-then-else true condition")
    (is (eq 'no (solve-first '((if (= a b) (= ?r yes) (= ?r no))) '?r)) "if-then-else false condition")
    (is (eq 'ok (solve-first '((if (parent john mary) (= ?r ok))) '?r)) "if-then true condition")
    (is (null (solve-all '((if (= a b) (= ?r ok))) 'dummy)) "if-then false condition fails")))

;; -----------------------------------------------------------
;; List manipulation predicates
;; -----------------------------------------------------------

(test list-operations
  "Test list manipulation predicates"
  ;; Test member
  (is (not (null (solve-all '((member b (a b c))) 'dummy))) "member/2 success")
  (is (null (solve-all '((member x (a b c))) 'dummy)) "member/2 failure")
  (is (equal '(a b c) (solve-all '((member ?x (a b c))) '?x)) "member/2 generate")
  
  ;; Test append
  (is (equal '(a b c d) (solve-first '((append (a b) (c d) ?x)) '?x)) "append/3 forward")
  (is (equal '(() (a b c)) (solve-first '((append ?x ?y (a b c))) '(?x ?y))) "append/3 backward")
  (is (= 4 (length (solve-all '((append ?x ?y (a b c))) '(?x ?y)))) "append/3 all splits"))

;; -----------------------------------------------------------
;; Repeat and backtracking
;; -----------------------------------------------------------

(test backtracking
  "Test backtracking predicates"
  ;; Test repeat (generates infinite solutions)
  (block test-repeat
    (let ((count 0))
      (solve '((repeat))
             (lambda (solution)
               (declare (ignore solution))
               (incf count)
               (when (>= count 5) (return-from test-repeat nil)))
             (lambda () nil))
      (is (>= count 5) "repeat generates multiple solutions"))))

