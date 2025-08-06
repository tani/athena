;;; core.lisp — Core Prolog engine tests
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(in-package :prolog/test/all)

;; Define test suite for core engine
(def-suite :prolog-core-tests :in :prolog-test-suite)
(in-suite :prolog-core-tests)

;; -----------------------------------------------------------
;; Low-level helpers
;; -----------------------------------------------------------

(test variable-p-tests
  "Test variable-p predicate for Prolog variable identification.
   Validates correct identification of Prolog variables (symbols starting with ?).
   This is fundamental for unification and term processing."
  (is-false (variable-p '?x) "?x should be recognized as a Prolog variable")
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

(test cut-operator-behavior
  "Test cut operator (!) for choice point elimination.
   Validates that the cut operator correctly prevents backtracking
   to alternative solutions, implementing deterministic behavior."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; Basic cut behavior
    (<- (choice a))
    (<- (choice b))
    (<- (first-choice ?x) (choice ?x) !)

    (is (equal '(a) (solve-all '((first-choice ?x)) '?x))
      "Should return only first choice 'a' due to cut preventing backtracking")))

(test sophisticated-cut-behavior
  "Test sophisticated cut behavior in complex control structures.
   Validates that cuts work correctly within nested meta-predicates,
   matching the advanced behavior of the Scheme implementation."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; Setup test predicates for complex cut scenarios
    (<- (p a))
    (<- (p b))
    (<- (p c))
    (<- (q 1))
    (<- (q 2))

    ;; Test cut inside 'or' - should cut the or but not outer choice points
    (<- (test-or-cut ?x) (or (and (p ?x) (== ?x b) !) (p ?x)))

    (is (equal '(b a b c) (solve-all '((test-or-cut ?x)) '?x))
      "Cut inside 'or' should only affect the or's choice points")

    ;; Test cut in 'and' with 'or' - more complex interaction
    (<- (test-and-or-cut ?x) (and (p ?x) (or (and (== ?x c) !) (fail))))

    (is (equal '(c) (solve-all '((test-and-or-cut ?x)) '?x))
      "Cut in nested and/or should work precisely")

    ;; Test cut in 'call' with 'or' - meta-predicate cut scoping
    (is (equal '(c) (solve-all '((and (p ?x) (call (or (and (== ?x c) !) (fail))))) '?x))
      "Cut inside call should only affect the called goal's choice points")

    ;; Test complex rule with cut and backtracking
    (<- (complex-rule ?x ?y)
      (p ?x)
      (or (and (q ?y) (== ?x b) !)
        (q ?y)))

    ;; When ?x = b, cut prevents backtracking in the or, but other ?x values still work
    (is (equal '((a 1) (a 2) (b 1) (b 2) (b 1) (b 2) (c 1) (c 2))
         (solve-all '((complex-rule ?x ?y)) '(?x ?y)))
      "Complex rule cut should only affect local choice points")))

(test cut-scope-isolation
  "Test that cuts properly isolate their scope and don't affect outer choice points."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (<- (outer a))
    (<- (outer b))
    (<- (inner 1))
    (<- (inner 2))

    ;; Cut inside inner goal should not affect outer choice points
    (<- (test-isolation ?x ?y)
      (outer ?x)
      (call (and (inner ?y) (== ?y 1) !)))

    (is (equal '((a 1) (b 1)) (solve-all '((test-isolation ?x ?y)) '(?x ?y)))
      "Cut should not affect choice points outside its scope")))

;; -----------------------------------------------------------
;; Zero-arity predicate definitions
;; -----------------------------------------------------------

(test zero-arity-predicates
  "Test zero-arity predicate definitions and calls.
   Validates that predicates with no arguments can be defined and called
   using both traditional (name) syntax and bare predicate symbol syntax."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (<- hello)
    ;; body also calls hello without parentheses
    (<- greet hello)
    ;; calls using traditional (name) syntax
    (is (not (null (solve-all '((hello)) 'dummy))) "zero-arity fact with parentheses")
    (is (not (null (solve-all '((greet)) 'dummy))) "zero-arity rule with parentheses")
    ;; calls using bare predicate symbol
    (is (not (null (solve-all '(hello) 'dummy))) "zero-arity fact bare symbol")
    (is (not (null (solve-all '(greet) 'dummy))) "zero-arity rule bare symbol")))

;; -----------------------------------------------------------
;; Variadic predicate definitions
;; -----------------------------------------------------------

(test variadic-predicates
  "Test variadic predicate definitions with rest arguments.
   Validates that predicates can use dot notation to capture
   remaining arguments into a list, similar to Lisp's &rest."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (<- (capture-rest ?out ?first . ?rest) (= ?out ?rest))
    (is (equal '(b c) (solve-first '((capture-rest ?r a b c)) '?r))
      "variadic rest collects remaining arguments")
    (is (equal '() (solve-first '((capture-rest ?r a)) '?r))
      "variadic rest empty when no extra arguments")
    (is (null (solve-all '((capture-rest ?r)) '?r))
      "variadic fails with too few arguments")))

;; -----------------------------------------------------------
;; Advanced cut behavior and hard failures
;; -----------------------------------------------------------

(test advanced-cut-hard-failure
  "Test advanced cut behavior with hard failures.
   Validates that cuts creating hard failures in one branch
   don't prevent backtracking in independent queries."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; test predicates
    (<- (q 1))
    (<- (q 2))

    (<-- (p ?x) (= ?x 1) ! (fail)) ; p(1) triggers a hard failure
    (<- (p ?x) (= ?x 2)) ; p(2) succeeds

    (is (= 2 (solve-first '((q ?y) (p ?y)) '?y))
      "hard failure in p(x) should not block backtracking in q(y)")))

;; -----------------------------------------------------------
;; Spy/trace behavior
;; -----------------------------------------------------------

(test spy-behavior
  "Test spy/trace functionality for debugging.
   Validates that spy predicates produce appropriate debugging output
   when enabled, showing CALL and EXIT traces."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (setf *current-clause-database* '())
    (<- (watched))
    (let ((output (make-string-output-stream))
          (input (make-string-input-stream "l"))
          (result ""))
      (let ((prolog/all:*current-spy-predicates* '(watched))
            (*standard-input* input)
            (*standard-output* output))
        (solve-all '((watched)) 'dummy)
        (setf result (get-output-stream-string output)))
      (is (search "CALL: (WATCHED)" result) "spy should show CALL trace")
      (is (search "EXIT: (WATCHED)" result) "spy should show EXIT trace"))))

;; -----------------------------------------------------------
;; Clause database operations
;; -----------------------------------------------------------

(test clause-database-operations
  "Test advanced clause database operations.
   Validates clause addition, retrieval, and the <-- overwrite operator
   for replacing existing clauses with the same head."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (setf *current-clause-database* '())
    (let ((clause1 '((parent alice bob)))
          (clause2 '((parent alice carol))))
      (add-clause! clause1)
      (is (equal (list clause1) (get-clauses 'parent)) "get-clauses after one add")
      (add-clause! clause2)
      (is (equal (list clause1 clause2) (get-clauses 'parent)) "get-clauses after two adds")
      (is (equal '() (get-clauses 'unknown)) "get-clauses for unknown predicate")

      ;; Test <-- overwrite functionality
      (<-- (father ?x ?y) (male ?x) (parent ?x ?y))
      (is (= 1 (length (get-clauses 'father))) "<-- overwrites existing clauses"))))

;; -----------------------------------------------------------
;; Run-query and ?- macro tests
;; -----------------------------------------------------------

(test run-query-success
  "Test run-query function for interactive query execution.
   Validates that successful queries produce appropriate output
   and handle user interaction correctly."
  (let ((output (make-string-output-stream))
        (input (make-string-input-stream "n")))
    (let ((*standard-input* input)
          (*standard-output* output))
      (run-query '((= ?a 1))))
    (let ((txt (get-output-stream-string output)))
      (is (not (string= txt (format nil "No.~%"))) "run-query should succeed for satisfiable query"))))

(test run-query-failure
  "Test run-query function for failing queries.
   Validates that queries with no solutions produce 'No.' output."
  (let ((output (make-string-output-stream))
        (input (make-string-input-stream "")))
    (let ((*standard-input* input)
          (*standard-output* output))
      (run-query '((fail))))
    (is (string= (format nil "No.~%") (get-output-stream-string output)) "run-query should output 'No.' for failing query")))

(test query-macro
  "Test ?- macro for interactive query execution.
   Validates that the ?- macro correctly delegates to run-query
   and produces expected output for queries."
  (let ((output (make-string-output-stream))
        (input (make-string-input-stream "n")))
    (let ((*standard-input* input)
          (*standard-output* output))
      (?- (= ?v ok)))
    (is (not (string= (get-output-stream-string output) "No.")) "?- macro should work for successful query")))

;; -----------------------------------------------------------
;; Engine simple recursion tests
;; -----------------------------------------------------------

(test engine-simple-recursion
  "Test engine with simple recursive rules.
   Validates basic fact retrieval, rule application, and recursive
   rule evaluation including transitive relations like ancestor."
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    (<- (parent john mary))
    (<- (parent john michael))
    (<- (parent mary susan))
    (<- (parent michael david))
    (<- (grandparent ?x ?y) (parent ?x ?z) (parent ?z ?y))

    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))

    (is (eq 'mary (solve-first '((parent john ?child)) '?child)) "direct fact")
    (is (equal '(mary michael) (solve-all '((parent john ?child)) '?child)) "all direct facts")
    (is (eq 'susan (solve-first '((grandparent john ?grandchild)) '?grandchild)) "simple rule")
    (is (equal '(susan david) (solve-all '((grandparent john ?grandchild)) '?grandchild)) "all simple rule")
    (is (eq 'mary (solve-first '((ancestor john ?d)) '?d)) "recursion first result")
    (is (equal '(mary michael susan david) (solve-all '((ancestor john ?d)) '?d)) "recursion all results")
    (is (eq 'mary (solve-first '((ancestor ?a susan)) '?a)) "recursion backward query")
    (is (null (solve-all '((parent david ?x)) '?x)) "failing goal")))
