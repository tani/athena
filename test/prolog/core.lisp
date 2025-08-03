;;; test/prolog/core.lisp — Core Prolog engine tests using FiveAM
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(in-package :prolog/test/core)

;; Define test suite for core engine
(def-suite :prolog/test/core :in :prolog-tests)
(in-suite :prolog/test/core)

;; -----------------------------------------------------------
;; Low-level helpers
;; -----------------------------------------------------------

(test variable-p-tests
  "Test variable-p predicate"
  (is (eq t (variable-p '?x)) "variable-p success for '?x")
  (is (eq t (variable-p '?)) "variable-p success for '?")  
  (is (eq nil (variable-p 'foo)) "variable-p failure for non-var")
  (is (eq nil (variable-p '(?x))) "variable-p failure for list"))

(test named-variable-p-tests
  "Test named-variable-p predicate"
  (is (eq t (named-variable-p '?x)) "named-variable-p success for '?x")
  (is (eq nil (named-variable-p '?)) "named-variable-p failure for '?")
  (is (eq nil (named-variable-p 'foo)) "named-variable-p failure for non-var"))

(test atom-p-tests
  "Test atom-p predicate"
  (is (eq nil (atom-p '(a b))) "atom-p for pair")
  (is (eq t (atom-p 'a)) "atom-p for symbol")
  (is (eq t (atom-p 123)) "atom-p for number"))

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
  (is (equal '() (unify 'a 'a '())) "unify atom-atom success")
  (is (failure-p (unify 'a 'b '())) "unify atom-atom mismatch")
  (is (failure-p (unify 'a '(a) '())) "unify atom-list mismatch"))

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
  (is (equal '((?x . c)) (unify '(a b ?x) '(a b c) '())) "unify list-list success")
  (is (failure-p (unify '(a b) '(a b c) '())) "unify list-list failure (length)")
  (is (failure-p (unify '(a x) '(a y) '())) "unify list-list failure (element)"))

(test occurs-check
  "Test occurs check prevention of infinite structures"
  (is (failure-p (unify '?x '(foo ?x) '())) "occurs-check simple")
  (is (failure-p (unify '?y '(bar (baz ?y)) '())) "occurs-check nested"))

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