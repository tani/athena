;;; test/core.lisp - Core Prolog engine tests

(defpackage :prolog/test-core
  (:use :common-lisp :prolog/core :prolog/test-helpers))

(in-package :prolog/test-core)

(defun run-tests ()
  (let ((*test-count* 0)
        (*pass-count* 0))
    
    ;; -----------------------------------------------------------
    ;; Low-level helpers
    ;; -----------------------------------------------------------
    (test-group "low-level helpers"
      (test-equal "variable-p success for '?x" t (variable-p '?x))
      (test-equal "variable-p success for '?" t (variable-p '?))
      (test-equal "variable-p failure for non-var" nil (variable-p 'foo))
      (test-equal "variable-p failure for list" nil (variable-p '(?x)))
      
      (test-equal "named-variable-p success for '?x" t (named-variable-p '?x))
      (test-equal "named-variable-p failure for '?" nil (named-variable-p '?))
      (test-equal "named-variable-p failure for non-var" nil (named-variable-p 'foo))
      
      (test-equal "atom-p for pair" nil (atom-p '(a b)))
      (test-equal "atom-p for symbol" t (atom-p 'a))
      (test-equal "atom-p for number" t (atom-p 123))
      
      (test-equal "cons add binding empty" '((?x . 1)) (cons (cons '?x 1) '()))
      (test-equal "cons add binding non-empty" '((?y . 2) (?x . 1)) 
                  (cons (cons '?y 2) '((?x . 1))))
      
      (let* ((bindings (cons (cons '?x 'foo) (cons (cons '?y '(bar)) '()))))
        (test-equal "substitute-bindings simple" 'foo 
                    (substitute-bindings bindings '?x))
        (test-equal "substitute-bindings with list" '(g foo (bar)) 
                    (substitute-bindings bindings '(g ?x ?y))))
      
      (let* ((cycle-bindings (cons (cons '?x '?y)
                                  (cons (cons '?y '?x) '()))))
        (test-equal "substitute-bindings simple cycle" '?x
                    (substitute-bindings cycle-bindings '?x)))
      
      (let* ((nested-bindings (cons (cons '?x '(a ?y))
                                   (cons (cons '?y '(b ?x)) '()))))
        (test-equal "substitute-bindings nested cycle" '(a (b ?x))
                    (substitute-bindings nested-bindings '?x)))
      
      (test-equal "variables-in simple" '(?x ?y) 
                  (variables-in '(f ?x (g ?y ?x))))
      (test-equal "variables-in with no vars" '() 
                  (variables-in '(a b (c))))
      
      (let ((expr-with-anon (replace-anonymous-variables '(a ? (?x ?)))))
        (test-assert "anonymous vars replaced" 
                     (not (member '? expr-with-anon))))
      (let ((expr-no-anon (replace-anonymous-variables '(a b))))
        (test-equal "replace-anonymous-variables with no anon vars" '(a b) 
                    expr-no-anon)))
    
    ;; -----------------------------------------------------------
    ;; Unification
    ;; -----------------------------------------------------------
    (test-group "unify"
      (test-equal "unify atom-atom success" '() (unify 'a 'a '()))
      (test-assert "unify atom-atom mismatch" (failure-p (unify 'a 'b '())))
      (test-assert "unify atom-list mismatch" (failure-p (unify 'a '(a) '())))
      
      (test-equal "unify var-atom" '((?x . a)) (unify '?x 'a '()))
      (test-equal "unify var-list" '((?x . (a b))) (unify '?x '(a b) '()))
      
      (test-equal "unify var-var link" '((?x . ?y)) (unify '?x '?y '()))
      (let ((bindings (unify '?x '?y (unify '?y 'val '()))))
        (test-equal "unify var-var with one bound" 'val 
                    (substitute-bindings bindings '?x)))
      
      (test-equal "unify list-list success" '((?x . c)) 
                  (unify '(a b ?x) '(a b c) '()))
      (test-assert "unify list-list failure (length)" 
                   (failure-p (unify '(a b) '(a b c) '())))
      (test-assert "unify list-list failure (element)" 
                   (failure-p (unify '(a x) '(a y) '())))
      
      (test-assert "occurs-check simple" 
                   (failure-p (unify '?x '(foo ?x) '())))
      (test-assert "occurs-check nested" 
                   (failure-p (unify '?y '(bar (baz ?y)) '()))))
    
    ;; -----------------------------------------------------------
    ;; API function coverage
    ;; -----------------------------------------------------------
    (test-group "exports"
      ;; object->string
      (test-equal "object->string pair" "(1 2)" (prolog/primitive:object->string '(1 2)))
      
      ;; remove-clauses-with-arity!
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        (setf *current-clause-database* '())
        (<- (r 1))
        (<- (r 1 2))
        (remove-clauses-with-arity! 'r 1)
        (test-equal "arity removal" 1 (length (get-clauses 'r))))
      
      ;; prove-all and success structures
      (let ((res (prove-all '((= ?x 1) (member ?y (1 2))) '())))
        (test-assert "prove-all success" (success-p res))
        (test-equal "success ?x" 1 (substitute-bindings (success-bindings res) '?x))
        (test-equal "success ?y" 1 (substitute-bindings (success-bindings res) '?y))
        (let ((next (funcall (success-continuation res))))
          (test-assert "success-continuation" (success-p next))
          (test-equal "next ?y" 2 (substitute-bindings (success-bindings next) '?y))))
      
      ;; call-with-current-choice-point
      (test-equal "call-with-current-choice-point" 'ok
                  (call-with-current-choice-point (lambda (tag) 'ok)))
      
      ;; solve function
      (let ((result nil))
        (solve '((= ?z 5))
               (lambda (solution) (setf result solution))
               (lambda () nil))
        (test-equal "solve function" '((?z . 5)) result))
      
      ;; run-query success
      (let ((out (make-string-output-stream))
            (in (make-string-input-stream "n
")))
        (let ((*standard-input* in)
              (*standard-output* out))
          (run-query '((= ?a 1))))
        (let ((txt (cl:get-output-stream-string out)))
          (test-assert "run-query success" (not (string= txt "No.
")))))
      
      ;; run-query failure
      (let ((out (make-string-output-stream))
            (in (make-string-input-stream "")))
        (let ((*standard-input* in)
              (*standard-output* out))
          (run-query '((fail))))
        (test-equal "run-query failure" "No.
" (cl:get-output-stream-string out)))
      
      ;; ?- macro
      (let ((out (make-string-output-stream))
            (in (make-string-input-stream "n
")))
        (let ((*standard-input* in)
              (*standard-output* out))
          (?- (= ?v ok)))
        (test-assert "?- macro" (not (string= (cl:get-output-stream-string out) "No.
")))))
    
    ;; -----------------------------------------------------------
    ;; Clause DB operations
    ;; -----------------------------------------------------------
    (test-group "clause-db"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        (setf *current-clause-database* '())
        (let ((clause1 '((parent alice bob)))
              (clause2 '((parent alice carol))))
          (add-clause! clause1)
          (test-equal "get-clauses after one add" (list clause1) (get-clauses 'parent))
          (add-clause! clause2)
          (test-equal "get-clauses after two adds" (list clause1 clause2) (get-clauses 'parent))
          (test-equal "get-clauses for unknown pred" '() (get-clauses 'unknown))
          
          (<-- (father ?x ?y) (male ?x) (parent ?x ?y))
          (test-equal "<-- overwrites" 1 (length (get-clauses 'father))))))
    
    ;; -----------------------------------------------------------
    ;; Engine simple recursion
    ;; -----------------------------------------------------------
    (test-group "engine"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        (<- (parent john mary))
        (<- (parent john michael))
        (<- (parent mary susan))
        (<- (parent michael david))
        (<- (grandparent ?x ?y) (parent ?x ?z) (parent ?z ?y))
        
        (<- (ancestor ?x ?y) (parent ?x ?y))
        (<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))
        
        (test-equal "direct fact" 'mary (solve-first '((parent john ?child)) '?child))
        (test-equal "all direct facts" '(mary michael) (solve-all '((parent john ?child)) '?child))
        (test-equal "simple rule" 'susan (solve-first '((grandparent john ?grandchild)) '?grandchild))
        (test-equal "all simple rule" '(susan david) (solve-all '((grandparent john ?grandchild)) '?grandchild))
        (test-equal "recursion first result" 'mary (solve-first '((ancestor john ?d)) '?d))
        (test-equal "recursion all results" '(mary michael susan david) (solve-all '((ancestor john ?d)) '?d))
        (test-equal "recursion backward query" 'mary (solve-first '((ancestor ?a susan)) '?a))
        (test-assert "failing goal" (null (solve-all '((parent david ?x)) '?x)))))
    
    ;; -----------------------------------------------------------
    ;; Zero-arity predicate definitions
    ;; -----------------------------------------------------------
    (test-group "zero-arity-predicates"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        (<- hello)
        ;; body also calls hello without parentheses
        (<- greet hello)
        ;; calls using traditional (name) syntax
        (test-assert "zero-arity fact" (not (null (solve-all '((hello)) 'dummy))))
        (test-assert "zero-arity rule" (not (null (solve-all '((greet)) 'dummy))))
        ;; calls using bare predicate symbol
        (test-assert "zero-arity fact bare" (not (null (solve-all '(hello) 'dummy))))
        (test-assert "zero-arity rule bare" (not (null (solve-all '(greet) 'dummy))))))
    
    ;; -----------------------------------------------------------
    ;; Variadic predicate definitions
    ;; -----------------------------------------------------------
    (test-group "variadic-predicates"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        (<- (capture-rest ?out ?first . ?rest) (= ?out ?rest))
        (test-equal "variadic rest collects args" '(b c) 
                    (solve-first '((capture-rest ?r a b c)) '?r))
        (test-equal "variadic rest empty" '() 
                    (solve-first '((capture-rest ?r a)) '?r))
        (test-assert "variadic too few arguments" 
                     (null (solve-all '((capture-rest ?r)) '?r)))))
    
    ;; -----------------------------------------------------------
    ;; Cut behavior in control structures
    ;; -----------------------------------------------------------
    (test-group "cut-behavior"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        (<- (p a))
        (<- (p b))
        (<- (p c))
        (<- (q 1))
        (<- (q 2))
        (<- (s ?x ?y)
            (p ?x)
            (or (and (q ?y) (== ?x b) !)
                (q ?y)))
        
        (test-equal "No Cut (Baseline)" '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))
                    (solve-all '((p ?X) (q ?Y)) '(?X ?Y)))
        
        (test-equal "Simple Cut in 'and'" '((a 1) (a 2))
                    (solve-all '((p ?X) ! (q ?Y)) '(?X ?Y)))
        
        ;; This may appear incorrect in Prolog, but defining 'or' and 'and'
        ;; predicates first in Prolog yields the same result.
        (test-equal "Cut inside 'or'" '(b a b c)
                    (solve-all '((or (and (p ?X) (== ?X b) !) (p ?X))) '?X))
        
        (test-equal "'and' with 'or' containing '!'" '(c)
                    (solve-all '((and (p ?X) (or (and (== ?X c) !) (fail)))) '?X))
        
        (test-equal "'call' with 'or' containing '!'" '(c)
                    (solve-all '((and (p ?X) (call (or (and (== ?X c) !) (fail))))) '?X))
        
        ;; This may appear incorrect in Prolog, but defining 'or' and 'and'
        ;; predicates first in Prolog yields the same result.
        (test-equal "Cut affecting parent goals (s/2)" 
                    '((a 1) (a 2) (b 1) (b 2) (b 1) (b 2) (c 1) (c 2))
                    (solve-all '((s ?X ?Y)) '(?X ?Y)))))
    
    ;; -----------------------------------------------------------
    ;; Advanced backtracking and ! propagation
    ;; -----------------------------------------------------------
    (test-group "advanced-cut-behavior"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; test predicates
        (<- (q 1))
        (<- (q 2))
        
        (<-- (p ?x) (= ?x 1) ! (fail)) ; p(1) triggers a hard failure
        (<- (p ?x) (= ?x 2)) ; p(2) succeeds
        
        (test-equal "hard failure in p(x) should not block backtracking in q(y)" 2
                    (solve-first '((q ?y) (p ?y)) '?y))))
    
    ;; -----------------------------------------------------------
    ;; Spy/debugging behavior
    ;; -----------------------------------------------------------
    (test-group "spy-behavior"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        (setf *current-clause-database* '())
        (<- (watched))
        (let ((out (make-string-output-stream))
              (in (make-string-input-stream "l"))
              (result ""))
          (let ((*current-spy-predicates* '(watched))
                (*standard-input* in)
                (*standard-output* out))
            (solve-all '((watched)) 'dummy)
            (setf result (cl:get-output-stream-string out)))
          (test-equal "spy output"
                      "Spy on (watched)? [l=leap c=creep n=nodebug] CALL: (watched)
EXIT: (watched)
"
                      result))))
    
    (format t "~%Total tests: ~D, Passed: ~D~%" *test-count* *pass-count*)))

;; Run tests when loaded
(run-tests)