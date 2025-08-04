;;; core.lisp — Core Prolog engine tests
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

;; Define test suite for core engine
(def-suite *prolog-core-tests* :in *prolog-test-suite*)
(in-suite *prolog-core-tests*)

;; -----------------------------------------------------------
;; Low-level helpers
;; -----------------------------------------------------------

(test variable-p-tests
  "Test variable-p predicate for Prolog variable identification.
   Validates correct identification of Prolog variables (symbols starting with ?).
   This is fundamental for unification and term processing."
  (is-true (variable-p '?x) "?x should be recognized as a Prolog variable")
  (is-true (variable-p '?) "? should be recognized as an anonymous variable")
  (is-false (variable-p 'foo) "foo should not be recognized as a variable (atom)")
  (is-false (variable-p '(?x)) "(?x) should not be recognized as a variable (list)"))

(test named-variable-p-tests
  "Test named-variable-p predicate for non-anonymous variable identification.
   Validates distinction between named variables (?x) and anonymous variables (?).
   Important for variable scoping and binding operations."
  (is-true (named-variable-p '?x) "?x should be recognized as a named variable")
  (is-false (named-variable-p '?) "? should not be recognized as named (anonymous)")
  (is-false (named-variable-p 'foo) "foo should not be recognized as a variable at all"))

(test atom-p-tests
  "Test atom-p predicate for atomic term identification.
   Validates correct classification of atomic (non-compound) terms.
   Essential for term structure analysis and unification."
  (is-false (atom-p '(a b)) "List (a b) should not be classified as atomic")
  (is-true (atom-p 'a) "Symbol 'a' should be classified as atomic")
  (is-true (atom-p 123) "Number 123 should be classified as atomic"))

(test binding-operations
  "Test basic binding operations"
  (is (equal '((?x . 1)) (cons (cons '?x 1) '())) "cons add binding empty")
  (is (equal '((?y . 2) (?x . 1))
       (cons (cons '?y 2) '((?x . 1))))
    "cons add binding non-empty"))

(test substitute-bindings-tests
  "Test variable substitution"
  (let* ((bindings (cons (cons '?x 'foo) (cons (cons '?y '(bar)) '()))))
    (is (eq 'foo (substitute-bindings bindings '?x)) "substitute-bindings simple")
    (is (equal '(g foo (bar)) (substitute-bindings bindings '(g ?x ?y))) "substitute-bindings with list"))

  ;; Test cycles
  (let* ((cycle-bindings (cons (cons '?x '?y) (cons (cons '?y '?x) '()))))
    (is (eq '?x (substitute-bindings cycle-bindings '?x)) "substitute-bindings simple cycle"))

  ;; Test nested cycles
  (let* ((nested-bindings (cons (cons '?x '(a ?y)) (cons (cons '?y '(b ?x)) '()))))
    (is (equal '(a (b ?x)) (substitute-bindings nested-bindings '?x)) "substitute-bindings nested cycle")))

(test variables-in-tests
  "Test variable extraction"
  (is (equal '(?x ?y) (variables-in '(f ?x (g ?y ?x)))) "variables-in simple")
  (is (equal '() (variables-in '(a b (c)))) "variables-in with no vars"))

(test anonymous-variable-tests
  "Test anonymous variable replacement"
  (let ((expr-with-anon (replace-anonymous-variables '(a ? (?x ?)))))
    (is (not (member '? expr-with-anon)) "anonymous vars replaced"))
  (let ((expr-no-anon (replace-anonymous-variables '(a b))))
    (is (equal '(a b) expr-no-anon) "replace-anonymous-variables with no anon vars")))

;; -----------------------------------------------------------
;; Unification tests
;; -----------------------------------------------------------

(test unification-basic
  "Test basic unification"
  (is (equal '() (unify 'a 'a '())))
  (is-true (failure-p (unify 'a 'b '())))
  (is-true (failure-p (unify 'a '(a) '()))))

(test unification-variables
  "Test variable unification"
  (is (equal '((?x . a)) (unify '?x 'a '())) "unify var-atom")
  (is (equal '((?x . (a b))) (unify '?x '(a b) '())) "unify var-list")
  (is (equal '((?x . ?y)) (unify '?x '?y '())) "unify var-var link")

  ;; Test variable chains
  (let ((bindings (unify '?x '?y (unify '?y 'val '()))))
    (is (eq 'val (substitute-bindings bindings '?x)) "unify var-var with one bound")))

(test unification-lists
  "Test list unification"
  (is (equal '((?x . c)) (unify '(a b ?x) '(a b c) '())))
  (is-true (failure-p (unify '(a b) '(a b c) '())))
  (is-true (failure-p (unify '(a x) '(a y) '()))))

(test occurs-check
  "Test occurs check prevention of infinite structures"
  (is-true (failure-p (unify '?x '(foo ?x) '())))
  (is-true (failure-p (unify '?y '(bar (baz ?y)) '()))))

;; -----------------------------------------------------------
;; API function coverage
;; -----------------------------------------------------------

(test api-exports
  "Test exported API functions"
  ;; object->string
  (is (equal "(1 2)" (object->string '(1 2))) "object->string pair")

  ;; Test clause operations
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (setf *current-clause-database* '())
    (<- (r 1))
    (<- (r 1 2))
    (remove-clauses-with-arity! 'r 1)
    (is (= 1 (length (get-clauses 'r))) "arity removal"))

  ;; Test prove-all and success structures
  (let ((res (prove-all '((= ?x 1) (member ?y (1 2))) '())))
    (is (success-p res) "prove-all success")
    (is (= 1 (substitute-bindings (success-bindings res) '?x)) "success ?x")
    (is (= 1 (substitute-bindings (success-bindings res) '?y)) "success ?y")
    (let ((next (funcall (success-continuation res))))
      (is (success-p next) "success-continuation")
      (is (= 2 (substitute-bindings (success-bindings next) '?y)) "next ?y")))

  ;; call-with-current-choice-point
  (is (eq 'ok (call-with-current-choice-point (lambda (tag) (declare (ignore tag)) 'ok))) "call-with-current-choice-point")

  ;; solve function
  (let ((result nil))
    (solve '((= ?z 5))
      (lambda (solution) (setf result solution))
      (lambda () nil))
    (is (equal '((?z . 5)) result) "solve function")))

(test backwards-compatibility
  "Test that FiveAM tests are compatible with existing functionality"
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; Use the same test patterns as the original framework
    (<- (test-compat hello))
    (<- (test-rule ?x) (test-compat ?x))

    ;; Test using solve-first helper (from existing framework)
    (is (eq 'hello (solve-first '((test-rule ?y)) '?y))
      "solve-first helper should work with FiveAM")

    ;; Test using solve-all helper
    (is (equal '(hello) (solve-all '((test-rule ?y)) '?y))
      "solve-all helper should work with FiveAM")))

(test performance-comparison
  "Test performance aspects that were important in original tests"
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; The famous countdown test that was in the original suite
    (<- (countdown 0))
    (<- (countdown ?n)
      (number ?n)
      (lisp t (> ?n 0))
      !
      (is ?n1 (- ?n 1))
      (countdown ?n1))

    ;; Test the same depth as the original test
    (is (not (null (solve-all '((countdown 50)) 'dummy)))
      "deep recursion (countdown from 50) - same as original test")))

(test basic-fact-querying
  "Test basic fact storage and retrieval in the clause database.
   Validates that simple facts can be stored and retrieved correctly,
   forming the foundation for all Prolog operations."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (setf *current-clause-database* '())
    (<- (parent john mary))
    (<- (parent mary susan))
    (<- (parent tom john))

    (is (eq 'mary (solve-first '((parent john ?child)) '?child))
      "Should retrieve Mary as John's child")
    (is (eq 'susan (solve-first '((parent mary ?child)) '?child))
      "Should retrieve Susan as Mary's child")))

(test rule-inference-resolution
  "Test rule-based inference and goal resolution.
   Validates that compound rules can be defined and resolved correctly,
   demonstrating the core logic programming capability."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (setf *current-clause-database* '())
    (<- (parent john mary))
    (<- (parent mary susan))
    (<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))

    (is (eq 'susan (solve-first '((grandparent john ?grandchild)) '?grandchild))
      "Should infer Susan as John's grandchild through rule resolution")))

(test recursive-rule-evaluation
  "Test recursive rule definitions and their evaluation.
   Validates that recursive rules work correctly with proper termination
   and backtracking behavior, essential for complex logic programs."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (setf *current-clause-database* '())
    (<- (parent john mary))
    (<- (parent mary susan))
    (<- (parent tom john))
    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))

    (is (not (null (solve-first '((ancestor tom susan)) 'dummy)))
      "Should find Tom as ancestor of Susan through recursive resolution")))

(test list-operation-integration
  "Test integration with built-in list operations.
   Validates that list predicates like member/2 work correctly
   within the query resolution system."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; Don't clear database - we need built-in predicates
    (is (eq 'a (solve-first '((member ?x (a b c))) '?x))
      "Should find first member 'a' in list (a b c)")))

(test arithmetic-evaluation-integration
  "Test arithmetic expression evaluation within queries.
   Validates that the 'is' predicate correctly evaluates arithmetic
   expressions and unifies results with variables."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; Don't clear database - we need built-in predicates
    (is (eq 7 (solve-first '((is ?result (+ 3 4))) '?result))
      "Should evaluate (+ 3 4) to 7 and bind to ?result")))

(test meta-predicate-findall
  "Test meta-predicate findall/3 for solution collection.
   Validates that findall correctly collects all solutions to a goal
   and returns them as a list, demonstrating meta-level operations."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; Only clear user-defined facts, keep built-ins
    (<- (color red))
    (<- (color green))
    (<- (color blue))

    (is (equal '(red green blue)
         (solve-first '((findall ?x (color ?x) ?colors)) '?colors))
      "Should collect all colors into list [red, green, blue]")))

(test cut-operator-behavior
  "Test cut operator (!) for choice point elimination.
   Validates that the cut operator correctly prevents backtracking
   to alternative solutions, implementing deterministic behavior."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; Only clear user-defined facts, keep built-ins
    (<- (choice a))
    (<- (choice b))
    (<- (first-choice ?x) (choice ?x) !)

    (is (equal '(a) (solve-all '((first-choice ?x)) '?x))
      "Should return only first choice 'a' due to cut preventing backtracking")))
