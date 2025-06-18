(test-begin "prolog")

;; -----------------------------------------------------------
;; 1. Low‑level helpers
;; -----------------------------------------------------------
(test-group "low-level helpers"
  (test-equal "variable? success for '?x" #t (variable? '?x))
  (test-equal "variable? success for '?" #t (variable? '?))
  (test-equal "variable? failure for non-var" #f (variable? 'foo))
  (test-equal "variable? failure for list" #f (variable? '(?x)))

  (test-equal "named-variable? success for '?x" #t (named-variable? '?x))
  (test-equal "named-variable? failure for '?" #f (named-variable? '?))
  (test-equal "named-variable? failure for non-var" #f (named-variable? 'foo))

  (test-equal "atom? for pair" #f (atom? '(a b)))
  (test-equal "atom? for symbol" #t (atom? 'a))
  (test-equal "atom? for number" #t (atom? 123))

  (test-equal "extend-bindings on empty" '((?x . 1)) (extend-bindings '?x 1 *empty-bindings*))
  (test-equal "extend-bindings on non-empty" '((?y . 2) (?x . 1)) (extend-bindings '?y 2 '((?x . 1))))

  (let* ((bindings (extend-bindings '?x 'foo (extend-bindings '?y '(bar) *empty-bindings*))))
    (test-equal "substitute-bindings simple" 'foo (substitute-bindings bindings '?x))
    (test-equal "substitute-bindings with list" '(g foo (bar)) (substitute-bindings bindings '(g ?x ?y))))

  (test-equal "variables-in simple" '(?x ?y) (variables-in '(f ?x (g ?y ?x))))
  (test-equal "variables-in with no vars" '() (variables-in '(a b (c))))

  (let ((expr-with-anon (replace-anonymous-variables '(a ? (?x ?)))))
    (test-assert "anonymous vars replaced" (not (memq '? expr-with-anon))))
  (let ((expr-no-anon (replace-anonymous-variables '(a b))))
    (test-equal "replace-anonymous-variables with no anon vars" '(a b) expr-no-anon)))

;; -----------------------------------------------------------
;; 2. Unification
;; -----------------------------------------------------------
(test-group "unify"
  (test-equal "unify atom-atom success" *empty-bindings* (unify 'a 'a *empty-bindings*))
  (test-assert "unify atom-atom mismatch" (failure? (unify 'a 'b *empty-bindings*)))
  (test-assert "unify atom-list mismatch" (failure? (unify 'a '(a) *empty-bindings*)))

  (test-equal "unify var-atom" '((?x . a)) (unify '?x 'a *empty-bindings*))
  (test-equal "unify var-list" '((?x . (a b))) (unify '?x '(a b) *empty-bindings*))

  (test-equal "unify var-var link" '((?x . ?y)) (unify '?x '?y *empty-bindings*))
  (let ((bindings (unify '?x '?y (unify '?y 'val *empty-bindings*))))
    (test-equal "unify var-var with one bound" 'val (substitute-bindings bindings '?x)))

  (test-equal "unify list-list success" '((?x . c)) (unify '(a b ?x) '(a b c) *empty-bindings*))
  (test-assert "unify list-list failure (length)" (failure? (unify '(a b) '(a b c) *empty-bindings*)))
  (test-assert "unify list-list failure (element)" (failure? (unify '(a x) '(a y) *empty-bindings*)))

  (test-assert "occurs-check simple" (failure? (unify '?x '(foo ?x) *empty-bindings*)))
  (test-assert "occurs-check nested" (failure? (unify '?y '(bar (baz ?y)) *empty-bindings*))))

;; -----------------------------------------------------------
;; 3. Clause DB operations
;; -----------------------------------------------------------
(let ((clause1 '((parent alice bob)))
      (clause2 '((parent alice carol))))
  (test-group "clause-db"
    (parameterize ((clause-database '())) ; fresh DB for this group
      (add-clause! clause1)
      (test-equal "get-clauses after one add" (list clause1) (get-clauses 'parent))
      (add-clause! clause2)
      (test-equal "get-clauses after two adds" (list clause1 clause2) (get-clauses 'parent))
      (test-equal "get-clauses for unknown pred" '() (get-clauses 'unknown))

      (<-- (father ?x ?y) (male ?x) (parent ?x ?y))
      (test-equal "<- overwrites" 1 (length (get-clauses 'father))))))


;; -----------------------------------------------------------
;; 4. Engine simple recursion
;; -----------------------------------------------------------
(<- (parent john mary))
(<- (parent john michael))
(<- (parent mary susan))
(<- (parent michael david))
(<- (grandparent ?x ?y) (parent ?x ?z) (parent ?z ?y))

(<- (ancestor ?x ?y) (parent ?x ?y))
(<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))


(test-group "engine"
  (test-equal "direct fact" 'mary (solve-first '((parent john ?child)) '?child))
  (test-equal "all direct facts" '(mary michael) (solve-all '((parent john ?child)) '?child))
  (test-equal "simple rule" 'susan (solve-first '((grandparent john ?grandchild)) '?grandchild))
  (test-equal "all simple rule" '(susan david) (solve-all '((grandparent john ?grandchild)) '?grandchild))
  (test-equal "recursion first result" 'mary (solve-first '((ancestor john ?d)) '?d))
  (test-equal "recursion all results" '(mary michael susan david) (solve-all '((ancestor john ?d)) '?d))
  (test-equal "recursion backward query" 'mary (solve-first '((ancestor ?a susan)) '?a))
  (test-assert "failing goal" (null? (solve-all '((parent david ?x)) '?x))))

;; -----------------------------------------------------------
;; 5. Built‑ins
;; -----------------------------------------------------------
(<- (foo 1))
(<- (foo 2))
(<- (foo 3))
(<- (bar ?x) (foo ?x) (cut) (= ?x 1))

(test-group "built-ins"
  (test-assert "fail predicate" (failure? (prove-all '((fail)) *empty-bindings*)))
  (test-assert "true predicate" (not (failure? (prove-all '((true)) *empty-bindings*))))

  (test-equal "= binds simple var" 'foo (solve-first '((= ?x foo)) '?x))
  (test-equal "= binds complex terms" '(b) (solve-first '((= (a ?y) (a (b)))) '?y))

  (test-assert "== succeeds for bound vars" (not (failure? (prove-all '((= ?x foo) (== ?x foo)) *empty-bindings*))))
  (test-assert "== fails for different vals" (failure? (prove-all '((= ?x foo) (== ?x bar)) *empty-bindings*)))

  (test-assert "not/1 succeeds" (not (failure? (prove-all '((not (parent susan ?_))) *empty-bindings*))))
  (test-assert "not/1 fails" (failure? (prove-all '((not (parent john mary))) *empty-bindings*)))

  (test-equal "call/1 simple" 'mary (solve-first '((call (parent john ?x))) '?x))
  (test-equal "call/1 with conjunction" '(mary michael susan david) (solve-all '((call (ancestor john ?gc))) '?gc))

  (test-equal "if/3 then" 'yes (solve-first '((if (= a a) (= ?r yes) (= ?r no))) '?r))
  (test-equal "if/3 else" 'no (solve-first '((if (= a b) (= ?r yes) (= ?r no))) '?r))
  (test-equal "if/2 then" 'ok (solve-first '((if (parent john mary) (= ?r ok))) '?r))
  (test-equal "if/2 else fails" #t (failure? (prove-all '((if (= a b) (= ?r ok))) *empty-bindings*)))


  (test-equal "is (lisp alias)" 7 (solve-first '((is ?v (+ 3 4))) '?v))
  (test-equal "lisp/2" 10 (solve-first '((lisp ?res (* 5 2))) '?res))

  (<- (item a)) (<- (item b)) (<- (item a))
  (test-equal "bagof gets all solutions" '(mary michael) (solve-first '((bagof ?c (parent john ?c) ?l)) '?l))
  (test-equal "setof gets unique sorted" '(david mary michael susan) (solve-first '((setof ?x (ancestor john ?x) ?o)) '?o))
  (test-equal "bagof vs setof" '((a b a) (a b)) (solve-first '((bagof ?x (item ?x) ?bag) (setof ?x (item ?x) ?set)) '(?bag ?set)))

  (test-equal "cut prunes choices" '(1) (solve-all '((bar ?v)) '?v))
  (test-equal "no cut finds all" '(1 2 3) (solve-all '((foo ?x) (= ?x ?x)) '?x)))

;; -----------------------------------------------------------
;; 6. Type & Dynamic predicates
;; -----------------------------------------------------------
(test-group "type-and-dynamic-predicates"
  (test-assert "atom/1 success" (not (failure? (prove-all '((atom foo)) *empty-bindings*))))
  (test-assert "atom/1 failure on var" (failure? (prove-all '((atom ?X)) *empty-bindings*)))
  (test-assert "atomic/1 success on number" (not (failure? (prove-all '((atomic 123)) *empty-bindings*))))
  (test-assert "atomic/1 failure on list" (failure? (prove-all '((atomic (a b))) *empty-bindings*)))
  (test-assert "var/1 success on unbound" (not (failure? (prove-all '((var ?X)) *empty-bindings*))))
  (test-assert "var/1 failure on bound" (failure? (prove-all '((= ?X 1) (var ?X)) *empty-bindings*)))
  (test-assert "ground/1 success on bound var" (not (failure? (prove-all '((= ?X (a b)) (ground ?X)) *empty-bindings*))))
  (test-assert "ground/1 failure on partial" (failure? (prove-all '((ground (a ?Y))) *empty-bindings*)))
  (test-assert "number/1 success" (not (failure? (prove-all '((number 42)) *empty-bindings*))))
  (test-assert "number/1 failure on atom" (failure? (prove-all '((number abc)) *empty-bindings*)))
  
  (test-equal "dynamic-put/get" 42 (solve-first '((dynamic-put my-var 42) (dynamic-get my-var ?V)) '?V))
  (test-equal "dynamic-put overwrite" "new" (solve-first '((dynamic-put my-var "old") (dynamic-put my-var "new") (dynamic-get my-var ?V)) '?V)))

;; -----------------------------------------------------------
;; 7. Library predicates (or/member/append/repeat/true)
;; -----------------------------------------------------------
(test-group "library"
  (test-equal "or/2 first succeeds" 'ok (solve-first '((or (= ?r ok) (= ?r ng))) '?r))
  (test-equal "or/2 second succeeds" 'ok (solve-first '((or (= 1 2) (= ?r ok))) '?r))
  (test-equal "or/2 all solutions" '(ok also-ok) (solve-all '((or (= ?r ok) (= ?r also-ok))) '?r))

  (test-assert "member/2 success" (not (failure? (prove-all '((member b (a b c))) *empty-bindings*))))
  (test-assert "member/2 failure" (failure? (prove-all '((member x (a b c))) *empty-bindings*)))
  (test-equal "member/2 generate" '(a b c) (solve-all '((member ?x (a b c))) '?x))

  (test-equal "append/3 forward" '(a b c d) (solve-first '((append (a b) (c d) ?x)) '?x))
  (test-equal "append/3 backward" '(() (a b c)) (solve-first '((append ?x ?y (a b c))) '(?x ?y)))
  (test-equal "append/3 all splits" 4 (length (solve-all '((append ?x ?y (a b c))) '(?x ?y))))

  (test-assert "repeat generates multiple solutions"
               (let loop ((cont (lambda () (prove-all '((repeat)) *empty-bindings*))) (n 0))
                 (cond ((>= n 5) #t) ; check for at least 5 solutions
                       (else (let ((r (cont)))
                               (and (not (failure? r))
                                    (loop (success-continuation r) (+ n 1))))))))
  (test-assert "true/0 always succeeds" (not (failure? (prove-all '((true)) *empty-bindings*)))))

;; -----------------------------------------------------------

;; 8. Advanced backtracking and cut propagation

;; -----------------------------------------------------------

(test-group "advanced-cut-behavior"

  ;; テスト用の述語を定義
  (<- (q 1))
  (<- (q 2))


  (<-- (p ?x) (= ?x 1) (cut) (fail)) ; p(1) はハードな失敗を引き起こす
  (<- (p ?x) (= ?x 2))             ; p(2) は成功する

  (test-equal "hard failure in p(x) should not block backtracking in q(y)"
              2
              (solve-first '((q ?y) (p ?y)) '?y)))

(test-end "prolog")
