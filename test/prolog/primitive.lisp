;;; primitive.lisp — Primitive predicate tests
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(in-package :prolog/test)

;; Define test suite for primitive predicates
(def-suite *prolog-primitive-tests* :in *prolog-test-suite*)
(in-suite *prolog-primitive-tests*)

;; -----------------------------------------------------------
;; Built-in predicates
;; -----------------------------------------------------------

(test basic-predicates
  "Test basic built-in predicates"
  (is (null (solve-all '((fail)) 'dummy)) "fail predicate")
  (is (not (null (solve-all '((true)) 'dummy))) "true predicate"))

(test unification-predicates
  "Test unification predicates"
  (is (eq 'foo (solve-first '((= ?x foo)) '?x)) "= binds simple var")
  (is (equal '(b) (solve-first '((= (a ?y) (a (b)))) '?y)) "= binds complex terms"))

(test equality-predicates
  "Test equality predicates"
  (is (not (null (solve-all '((= ?x foo) (== ?x foo)) 'dummy))) "== succeeds for bound vars")
  (is (null (solve-all '((= ?x foo) (== ?x bar)) 'dummy)) "== fails for different vals"))

(test type-predicates
  "Test type checking predicates"
  (is (not (null (solve-all '((atom foo)) 'dummy))) "atom/1 success")
  (is (null (solve-all '((atom ?X)) 'dummy)) "atom/1 failure on var")
  (is (not (null (solve-all '((atomic 123)) 'dummy))) "atomic/1 success on number")
  (is (null (solve-all '((atomic (a b))) 'dummy)) "atomic/1 failure on list")
  (is (not (null (solve-all '((var ?X)) 'dummy))) "var/1 success on unbound")
  (is (null (solve-all '((= ?X 1) (var ?X)) 'dummy)) "var/1 failure on bound")
  (is (not (null (solve-all '((ground foo)) 'dummy))) "ground/1 success on atom")
  (is (null (solve-all '((ground (a ?Y))) 'dummy)) "ground/1 failure on partial")
  (is (not (null (solve-all '((number 42)) 'dummy))) "number/1 success")
  (is (null (solve-all '((number abc)) 'dummy)) "number/1 failure")
  (is (not (null (solve-all '((string "hello")) 'dummy))) "string/1 success")
  (is (null (solve-all '((string hello)) 'dummy)) "string/1 failure"))

(test meta-call
  "Test meta-call predicates"
  ;; Set up test data
  (<- (parent john mary))
  ;; Test meta-call
  (is (eq 'mary (solve-first '((call (parent john ?x))) '?x)) "call/1 simple")
  (is (eq 'mary (solve-first '((call parent john ?x)) '?x)) "call variadic simple"))

(test arithmetic-evaluation
  "Test arithmetic and Lisp evaluation"
  (is (= 7 (solve-first '((is ?v (+ 3 4))) '?v)) "is (lisp alias)")
  (is (= 10 (solve-first '((lisp ?res (* 5 2))) '?res)) "lisp/2"))

(test meta-predicates  
  "Test solution collection predicates"
  ;; Setup some test data
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (<- (item a))
    (<- (item b))
    (<- (item a))
    
    ;; Test findall
    (is (equal '(a b a) (solve-first '((findall ?x (item ?x) ?l)) '?l)) "findall gets all solutions")
    (is (equal '() (solve-first '((findall ?x (missing ?x) ?l)) '?l)) "findall with no solutions")
    
    ;; Test bagof  
    (is (equal '(a b a) (solve-first '((bagof ?x (item ?x) ?l)) '?l)) "bagof gets all solutions")
    (is (null (solve-all '((bagof ?x (missing ?x) ?l)) 'dummy)) "bagof fails with no solutions")
    
    ;; Test setof
    (is (equal '(a b) (solve-first '((setof ?x (item ?x) ?l)) '?l)) "setof removes duplicates")
    (is (null (solve-all '((setof ?x (missing ?x) ?l)) 'dummy)) "setof fails with no solutions")
    
    ;; Test sort
    (is (equal '(1 2 3) (solve-first '((sort (3 1 2) ?s)) '?s)) "sort numbers")
    (is (equal '(1 2 3) (solve-first '((sort (3 1 2 3 1) ?s)) '?s)) "sort removes duplicates")))