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
  "Test fundamental built-in predicates: true/0 and fail/0.
   Validates that true always succeeds and fail always fails,
   forming the basis for control flow in logic programs."
  (is (= 0 (solve-count '((fail)))) "fail/0 should produce zero solutions")
  (is (> (solve-count '((true))) 0) "true/0 should produce at least one solution"))

(test unification-predicates
  "Test unification predicate =/2 for term matching and variable binding.
   Validates that =/2 correctly unifies terms and creates appropriate
   variable bindings for both simple and complex term structures."
  (is (eq 'foo (solve-first '((= ?x foo)) '?x)) "=/2 should bind variable ?x to atom foo")
  (is (equal '(b) (solve-first '((= (a ?y) (a (b)))) '?y)) "=/2 should unify complex terms and bind ?y to (b)"))

(test equality-predicates
  "Test strict equality predicate ==/2 versus unification =/2.
   Validates that ==/2 tests for structural equality without unification,
   while =/2 performs unification with variable binding."
  (is (> (solve-count '((= ?x foo) (== ?x foo))) 0) "After unification, == should succeed on bound variables")
  (is (= 0 (solve-count '((= ?x foo) (== ?x bar)))) "== should fail when comparing different values"))

(test type-predicates
  "Test built-in type checking predicates for term classification.
   Validates correct identification of different term types including atoms,
   numbers, variables, and compound terms. Essential for type-safe operations."
  (is (> (solve-count '((atom foo))) 0) "atom/1 should succeed for atom 'foo'")
  (is (= 0 (solve-count '((atom ?X)))) "atom/1 should fail for unbound variable ?X")
  (is (> (solve-count '((atomic 123))) 0) "atomic/1 should succeed for number 123")
  (is (= 0 (solve-count '((atomic (a b))))) "atomic/1 should fail for compound term (a b)")
  (is (> (solve-count '((var ?X))) 0) "var/1 should succeed for unbound variable ?X")
  (is (= 0 (solve-count '((= ?X 1) (var ?X)))) "var/1 should fail for bound variable ?X")
  (is (> (solve-count '((ground foo))) 0) "ground/1 should succeed for ground term 'foo'")
  (is (= 0 (solve-count '((ground (a ?Y))))) "ground/1 should fail for term containing variable ?Y")
  (is (> (solve-count '((number 42))) 0) "number/1 should succeed for number 42")
  (is (= 0 (solve-count '((number abc)))) "number/1 should fail for atom 'abc'")
  (is (> (solve-count '((string "hello"))) 0) "string/1 should succeed for string \"hello\"")
  (is (= 0 (solve-count '((string hello)))) "string/1 should fail for atom 'hello'"))

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
  ;; Clean up any existing item facts first
  (remove-clauses-with-arity! 'item 1)
  
  ;; Setup test data
  (<- (item a))
  (<- (item b))
  (<- (item a))
  
  ;; Test findall
  (is (equal '(a b a) (solve-first '((findall ?x (item ?x) ?l)) '?l)))
  (is (equal '() (solve-first '((findall ?x (missing ?x) ?l)) '?l)))
  
  ;; Test bagof  
  (is (equal '(a b a) (solve-first '((bagof ?x (item ?x) ?l)) '?l)))
  (is (= 0 (solve-count '((bagof ?x (missing ?x) ?l)))))
  
  ;; Test setof
  (is (equal '(a b) (solve-first '((setof ?x (item ?x) ?l)) '?l)))
  (is (= 0 (solve-count '((setof ?x (missing ?x) ?l)))))
  
  ;; Test sort
  (is (equal '(1 2 3) (solve-first '((sort (3 1 2) ?s)) '?s)))
  (is (equal '(1 2 3) (solve-first '((sort (3 1 2 3 1) ?s)) '?s))))