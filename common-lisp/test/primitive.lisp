;;; primitive.lisp — Primitive predicate tests
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(in-package :prolog/test/all)

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
  (is (> (solve-count '((= ?X (a b)) (ground ?X))) 0) "ground/1 should succeed for bound variable")
  (is (= 0 (solve-count '((ground (a ?Y))))) "ground/1 should fail for partial ground term")
  ;; Comprehensive number tests
  (is (> (solve-count '((number 42))) 0) "number/1 should succeed for positive integer")
  (is (> (solve-count '((number -17))) 0) "number/1 should succeed for negative integer")
  (is (> (solve-count '((number 0))) 0) "number/1 should succeed for zero")
  (is (> (solve-count '((number 3.14))) 0) "number/1 should succeed for positive float")
  (is (> (solve-count '((number -2.5))) 0) "number/1 should succeed for negative float")
  (is (= 0 (solve-count '((number abc)))) "number/1 should fail for atom 'abc'")
  (is (= 0 (solve-count '((number "123")))) "number/1 should fail for string \"123\"")
  (is (= 0 (solve-count '((number (1 2 3))))) "number/1 should fail for list")
  (is (= 0 (solve-count '((number ?)))) "number/1 should fail for unbound variable")
  ;; String tests
  (is (> (solve-count '((string "hello"))) 0) "string/1 should succeed for string \"hello\"")
  (is (= 0 (solve-count '((string hello)))) "string/1 should fail for atom 'hello'")
  (is (= 0 (solve-count '((string 123)))) "string/1 should fail for number")
  (is (= 0 (solve-count '((string ?)))) "string/1 should fail for unbound variable"))

(test meta-call
  "Test meta-call predicates for dynamic goal execution.
   Validates different forms of call/N including simple, variadic,
   and compound predicates with extra arguments."
  ;; Set up test data
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (<- (parent john mary))
    (<- (parent john michael))
    (<- (parent mary susan))
    (<- (parent michael david))
    (<- (grandparent ?x ?y) (parent ?x ?z) (parent ?z ?y))
    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))

    ;; Test meta-call forms
    (is (eq 'mary (solve-first '((call (parent john ?x))) '?x)) "call/1 simple")
    (is (equal '(mary michael susan david) (solve-all '((call (ancestor john ?gc))) '?gc)) "call/1 with conjunction")
    ;; Variadic call form
    (is (eq 'mary (solve-first '((call parent john ?x)) '?x)) "call variadic simple")
    (is (equal '(mary michael susan david) (solve-all '((call ancestor john ?gc)) '?gc)) "call variadic with conjunction")
    ;; Compound predicate with extra arguments
    (is (eq 'mary (solve-first '((call (parent john) ?x)) '?x)) "call compound+args")
    (is (equal '(mary michael susan david) (solve-all '((call (ancestor john) ?gc)) '?gc)) "call compound+args conjunction")))

(test arithmetic-evaluation
  "Test arithmetic and Lisp evaluation"
  (is (= 7 (solve-first '((is ?v (+ 3 4))) '?v)) "is (lisp alias)")
  (is (= 10 (solve-first '((lisp ?res (* 5 2))) '?res)) "lisp/2"))

(test conditional-predicates
  "Test if-then-else conditional predicates.
   Validates both if/3 (if-then-else) and if/2 (if-then) forms."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (<- (parent john mary))

    ;; Test if/3 (if-then-else)
    (is (eq 'yes (solve-first '((if (= a a) (= ?r yes) (= ?r no))) '?r)) "if/3 then branch")
    (is (eq 'no (solve-first '((if (= a b) (= ?r yes) (= ?r no))) '?r)) "if/3 else branch")

    ;; Test if/2 (if-then)
    (is (eq 'ok (solve-first '((if (parent john mary) (= ?r ok))) '?r)) "if/2 then succeeds")
    (is (null (solve-all '((if (= a b) (= ?r ok))) 'dummy)) "if/2 else fails")))

(test dynamic-predicates
  "Test dynamic variable storage predicates.
   Validates dynamic-put/2 and dynamic-get/2 for runtime variable storage."
  (is (= 42 (solve-first '((dynamic-put my-var 42) (dynamic-get my-var ?V)) '?V)) "dynamic-put/get simple")
  (is (equal "new" (solve-first '((dynamic-put my-var "old") (dynamic-put my-var "new") (dynamic-get my-var ?V)) '?V)) "dynamic-put overwrite"))

(test meta-predicates
  "Test solution collection predicates"
  ;; Clean up any existing item facts first
  (remove-clauses-with-arity! 'item 1)

  ;; Setup test data
  (<- (item a))
  (<- (item b))
  (<- (item a)))

(test maplist-predicate
  "Test maplist/2 predicate for applying predicates to lists.
   Validates that maplist correctly applies a predicate to each
   element of a list and succeeds only if all applications succeed."
  ;; Test maplist with atom predicate
  (is (not (null (solve-all '((maplist atom (a b c))) 'dummy))) "maplist atom success")
  (is (null (solve-all '((maplist atom (a 1 c))) 'dummy)) "maplist atom failure")

  ;; Test maplist with = to unify lists
  (is (equal '(a b c) (solve-first '((maplist = (a b c) (?x ?y ?z))) '(?x ?y ?z))) "maplist = unify lists"))
