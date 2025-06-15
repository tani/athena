;; test.scm — SRFI‑64 tests for mini‑Prolog (record‑based version)

(import (scheme base)
        (srfi 64)
        (prolog))

(test-begin "prolog")

;; -----------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------
(define (solve-first goals term)
  (let ((r (reset (prove-all goals *empty-bindings*))))
    (and (not (failure? r))
         (substitute-bindings (success-bindings r) term))))

(define (solve-all goals term)
  (let loop ((cont (lambda () (prove-all goals *empty-bindings*)))
             (acc '()))
    (let ((r (cont)))
      (if (failure? r)
          (reverse acc)
          (loop (success-continuation r)
                (cons (substitute-bindings (success-bindings r) term) acc))))))

;; -----------------------------------------------------------
;; 1. Low‑level helpers
;; -----------------------------------------------------------
(test-group "low-level helpers"
  (test-equal "variable? '?x" #t (variable? '?x))
  (test-equal "variable? non-var" #f (variable? 'foo))
  (test-equal "named-variable? '?" #f (named-variable? '?))
  (test-equal "named-variable? '?x" #t (named-variable? '?x))

  (test-equal "atom? for pair" #f (atom? '(a b)))
  (test-assert "failure? tag" (failure? (failure)))

  (test-equal "extend-bindings" '((?x . 1)) (extend-bindings '?x 1 *empty-bindings*))

  (let* ((b (extend-bindings '?x 'foo *empty-bindings*)))
    (test-equal "substitute-bindings" 'foo (substitute-bindings b '?x)))

  (test-equal "variables-in" '(?x ?y) (variables-in '(f ?x (g ?y ?x))))
  (let ((e (replace-anonymous-variables '(a ? (?x ?)))))
    (test-assert "anonymous vars replaced" (not (memq '? e)))))

;; -----------------------------------------------------------
;; 2. Unification
;; -----------------------------------------------------------
(test-group "unify"
  (test-equal "atom-atom" *empty-bindings* (unify 'a 'a *empty-bindings*))
  (test-equal "var-atom" '((?x . a)) (unify '?x 'a *empty-bindings*))
  (test-assert "atom mismatch" (failure? (unify 'a 'b *empty-bindings*)))
  (test-equal "var-var link" '((?x . ?y)) (unify '?x '?y *empty-bindings*))
  (test-assert "occurs-check" (failure? (unify '?x '(foo ?x) *empty-bindings*))))

;; -----------------------------------------------------------
;; 3. Clause DB operations
;; -----------------------------------------------------------
(let ((test-cl '((parent alice bob))))
  (test-group "clause-db"
    (parameterize ((clause-database (clause-database)))
      (add-clause! test-cl)
      (test-equal "get-clauses returns inserted" (list test-cl) (get-clauses 'parent)))))

;; -----------------------------------------------------------
;; 4. Engine simple recursion
;; -----------------------------------------------------------
(<- (parent john mary))
(<- (parent john michael))
(<- (parent mary susan))
(<- (parent michael david))

(<- (ancestor ?x ?y)
    (parent ?x ?z)
    (parent ?z ?y))
(<- (ancestor ?x ?y)
    (parent ?x ?z)
    (ancestor ?z ?y))

(test-group "engine"
  (test-equal "direct fact" 'mary (solve-first '((parent john ?c)) '?c))
  (test-equal "recursion" '(susan david) (solve-all '((ancestor john ?d)) '?d)))

;; -----------------------------------------------------------
;; 5. Built‑ins
;; -----------------------------------------------------------
(<- (foo 1))
(<- (foo 2))
(<- (bar ?x) (foo ?x) (cut) (= ?x 1))

(test-group "built-ins"
  (test-assert "fail" (failure? (reset (prove-all '((fail)) *empty-bindings*))))

  (test-equal "= binds" 'foo (solve-first '((= ?x foo)) '?x))

  (test-assert "== succeeds"
               (not (failure? (reset (prove-all '((= ?x foo) (== ?x foo)) *empty-bindings*)))))

  (test-assert "not/1 succeeds"
               (not (failure? (reset (prove-all '((not (parent susan _))) *empty-bindings*)))))

  (test-equal "call/1" 'mary (solve-first '((call (parent john ?x))) '?x))

  (test-equal "if/3 then" 'yes (solve-first '((if (= a a) (= ?r yes) (= ?r no))) '?r))
  (test-equal "if/3 else" 'no (solve-first '((if (= a b) (= ?r yes) (= ?r no))) '?r))

  (test-equal "lisp/is" 7 (solve-first '((is ?v (+ 3 4))) '?v))

  (test-equal "bagof" '(mary michael) (solve-first '((bagof ?c (parent john ?c) ?l)) '?l))
  (test-equal "setof" '(david susan) (solve-first '((setof ?x (ancestor john ?x) ?o)) '?o))

  (test-equal "cut" '(1) (solve-all '((bar ?v)) '?v)))

;; -----------------------------------------------------------
;; 6. Type & Dynamic predicates
;; -----------------------------------------------------------
(test-group "type-and-dynamic-predicates"
  (test-assert "atom/1 success" (not (failure? (reset (prove-all '((atom foo)) *empty-bindings*)))))
  (test-assert "atom/1 failure" (failure? (reset (prove-all '((atom ?X)) *empty-bindings*))))
  (test-assert "atomic/1 success" (not (failure? (reset (prove-all '((atomic 123)) *empty-bindings*)))))
  (test-assert "atomic/1 failure" (failure? (reset (prove-all '((atomic (a b))) *empty-bindings*))))
  (test-assert "var/1 success" (not (failure? (reset (prove-all '((var ?X)) *empty-bindings*)))))
  (test-assert "var/1 failure" (failure? (reset (prove-all '((= ?X 1) (var ?X)) *empty-bindings*))))
  (test-assert "ground/1 success"
               (not (failure? (reset (prove-all '((= ?X (a b)) (ground ?X)) *empty-bindings*)))))
  (test-assert "ground/1 failure" (failure? (reset (prove-all '((ground (a ?Y))) *empty-bindings*))))
  (test-assert "number/1 success" (not (failure? (reset (prove-all '((number 42)) *empty-bindings*)))))
  (test-assert "number/1 failure" (failure? (reset (prove-all '((number abc)) *empty-bindings*))))
  (test-equal "dynamic-put/get" 42 (solve-first '((dynamic-put my-var 42) (dynamic-get my-var ?V)) '?V)))

;; -----------------------------------------------------------
;; 7. Library predicates (or/member/append/repeat/true)
;; -----------------------------------------------------------
(test-group "library"
  (test-equal "or/2" 'ok (solve-first '((or (= ?r ok) (= ?r ng))) '?r))
  (test-assert "member/2" (not (failure? (reset (prove-all '((member b (a b c))) *empty-bindings*)))))
  (test-equal "append/3" '(a b c d) (solve-first '((append (a b) (c d) ?x)) '?x))
  (test-assert "repeat ≥ 3"
               (let loop ((cont (lambda () (prove-all '((repeat)) *empty-bindings*))) (n 0))
                 (cond ((>= n 3) #t)
                       (else (let ((r (cont)))
                               (and (not (failure? r))
                                    (loop (success-continuation r) (+ n 1))))))))
  (test-assert "true/0" (not (failure? (reset (prove-all '((true)) *empty-bindings*))))) )

(test-end "prolog")

