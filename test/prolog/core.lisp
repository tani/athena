;;; core.lisp — Core Prolog engine tests
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(in-package :prolog/test)

;; Define test suite for core engine
(def-suite *prolog-core-tests* :in *prolog-test-suite*)
(in-suite *prolog-core-tests*)

;; -----------------------------------------------------------
;; Low-level helpers
;; -----------------------------------------------------------

(test variable-p-tests
  "Test variable-p predicate"
  (is-true (variable-p '?x))
  (is-true (variable-p '?))  
  (is-false (variable-p 'foo))
  (is-false (variable-p '(?x))))

(test named-variable-p-tests
  "Test named-variable-p predicate"
  (is-true (named-variable-p '?x))
  (is-false (named-variable-p '?))
  (is-false (named-variable-p 'foo)))

(test atom-p-tests
  "Test atom-p predicate"
  (is-false (atom-p '(a b)))
  (is-true (atom-p 'a))
  (is-true (atom-p 123)))

(test binding-operations
  "Test basic binding operations"
  (is (equal '((?x . 1)) (cons (cons '?x 1) '())) "cons add binding empty")
  (is (equal '((?y . 2) (?x . 1)) 
             (cons (cons '?y 2) '((?x . 1)))) "cons add binding non-empty"))

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
  (is (eq 'ok (call-with-current-choice-point (lambda (tag) 'ok))) "call-with-current-choice-point")
  
  ;; solve function
  (let ((result nil))
    (solve '((= ?z 5))
           (lambda (solution) (setf result solution))
           (lambda () nil))
    (is (equal '((?z . 5)) result) "solve function")))

;; Integration and compatibility tests moved from fiveam-runner.lisp

(test integration-with-asdf
  "Test that ASDF integration works correctly"
  (is (find-package :prolog) "Main prolog package should exist")
  (is (find-package :prolog/core) "Core package should exist")
  (is (find-package :prolog/primitive) "Primitive package should exist")
  (is (find-package :prolog/stdlib) "Stdlib package should exist")
  
  ;; Test that we can use the public API
  (is (fboundp 'solve) "solve function should be accessible")
  (is (fboundp '<-) "<- macro should be accessible")
  (is (fboundp 'unify) "unify function should be accessible"))

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

(test comprehensive-functionality
  "Comprehensive test covering all major Prolog features"
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    ;; Family relationship example (common in Prolog)
    (<- (parent john mary))
    (<- (parent mary susan))
    (<- (parent tom john))
    (<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))
    
    ;; Test basic facts
    (is (eq 'mary (solve-first '((parent john ?child)) '?child))
        "Basic fact querying")
    
    ;; Test rule inference
    (is (eq 'susan (solve-first '((grandparent john ?grandchild)) '?grandchild))
        "Rule inference")
    
    ;; Test recursive rules
    (is (not (null (solve-first '((ancestor tom susan)) 'dummy)))
        "Recursive rule evaluation")
    
    ;; Test list operations
    (is (eq 'a (solve-first '((member ?x (a b c))) '?x))
        "List member operation")
    
    ;; Test arithmetic
    (is (eq 7 (solve-first '((is ?result (+ 3 4))) '?result))
        "Arithmetic evaluation")
    
    ;; Test meta-predicates
    (<- (color red))
    (<- (color green))
    (<- (color blue))
    (is (equal '(red green blue) 
               (solve-first '((findall ?x (color ?x) ?colors)) '?colors))
        "Meta-predicate findall")
    
    ;; Test cut operator
    (<- (choice a))
    (<- (choice b))
    (<- (first-choice ?x) (choice ?x) !)
    (is (equal '(a) (solve-all '((first-choice ?x)) '?x))
        "Cut operator behavior")))
