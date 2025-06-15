;; test-mini-prolog-srfi64.scm --- SRFI-64 portable unit tests for mini-Prolog engine

;;; ------------------------------------------------------------
;;; 0. Load the code to be tested
;;; ------------------------------------------------------------
(import
  (scheme base)
  (srfi 64)
  (prolog))

;;; ------------------------------------------------------------
;;; 1. Start tests
;;; ------------------------------------------------------------
(test-begin "prolog")

;;; ------------------------------------------------------------
;;; 2. Helper: Solution set utility
;;; ------------------------------------------------------------
(define (solve-first goals term)
  (let ((r (reset (prove-all goals *empty-bindings*))))
    (and (not (failure? r)) (substitute-bindings (car r) term))))

(define (solve-all goals term)
  (let loop ((cont (lambda () (prove-all goals *empty-bindings*)))
             (acc '()))
    (let ((r (cont)))
      (if (failure? r)
          (reverse acc)
          (loop (cdr r)
                (cons (substitute-bindings (car r) term) acc))))))

;;; ------------------------------------------------------------
;;; 3. Low-level utilities
;;; ------------------------------------------------------------
(test-group "low-level helpers"
  ;; variable?/named-variable?
  (test-equal "variable? '?x" #t (variable? '?x))
  (test-equal "variable? non-var" #f (variable? 'foo))
  (test-equal "named-variable? '?'" #f (named-variable? '?))
  (test-equal "named-variable? '?x" #t (named-variable? '?x))

  ;; atom?/failure?/extend-bindings
  (test-equal "atom? for pair" #f (atom? '(a b)))
  (test-assert "failure? tag check" (failure? (failure)))
  (test-equal "extend-bindings" (list (cons '?x 1)) (extend-bindings '?x 1 *empty-bindings*))

  ;; substitute-bindings
  (let* ((b (extend-bindings '?x 'foo *empty-bindings*)))
    (test-equal "substitute-bindings" 'foo (substitute-bindings b '?x)))

  ;; variables-in / replace-anonymous-variables
  (test-equal "variables-in collects unique" '(?x ?y)
              (variables-in '(f ?x (g ?y ?x))))
  (let ((e (replace-anonymous-variables '(a ? (?x ?)))))
    (test-assert "anonymous vars replaced" (not (memq '? e)))))

;;; ------------------------------------------------------------
;;; 4. Unification & related
;;; ------------------------------------------------------------
(test-group "unify"
  (test-equal "unify atom-atom" *empty-bindings*
              (unify 'a 'a *empty-bindings*))

  (test-equal "unify var-atom" '((?x . a))
              (unify '?x 'a *empty-bindings*))

  (test-equal "unify atom mismatch" *failure-object*
              (unify 'a 'b *empty-bindings*))

  (test-equal "unify var-var linking" '((?x . ?y))
              (unify '?x '?y *empty-bindings*))

  (test-equal "occurs-check positive" *failure-object*
              (unify '?x '(foo ?x) *empty-bindings*)))

;;; ------------------------------------------------------------
;;; 5. Database operations
;;; ------------------------------------------------------------
;;; Note: For portability, definitions are moved outside the test-group.
;;; The test group itself will now only contain test expressions.
(let ((test-cl '( (parent alice bob) )))
  (test-group "clause-db"
    ;; Test operation
    (add-clause! test-cl)
    (test-equal "get-clauses returns inserted clause"
                (list test-cl) (get-clauses 'parent))
    (clear-database!)))

;;; ------------------------------------------------------------
;;; 6. Prolog engine (prove-all, etc.)
;;; ------------------------------------------------------------
;;; Setup clauses for the "engine" test group
(<- (parent john mary))
(<- (parent john michael))
(<- (parent mary  susan))
(<- (parent michael david))
;; -> Direct "grandparent" rule
(<- (ancestor ?x ?y)
    (parent   ?x ?z)
    (parent   ?z ?y))
;; -> Rule for deeper generations
(<- (ancestor ?x ?y)
    (parent   ?x ?z)
    (ancestor ?z ?y))

(test-group "engine: simple facts/recursion"
  (test-equal "prove-all direct fact" 'mary
              (solve-first '((parent john ?c)) '?c))

  (test-equal "prove-all recursion" '(susan david)
              (solve-all '((ancestor john ?d)) '?d)))

;;; ------------------------------------------------------------
;;; 7. Built-in predicates
;;; ------------------------------------------------------------
;;; Setup clauses for the "cut" test in the "built-ins" group
(<- (foo 1))
(<- (foo 2))
(<- (bar ?x) (foo ?x) (cut) (= ?x 1))

(test-group "built-ins"
  ;; fail
  (test-equal "fail built-in" *failure-object*
              (reset (prove-all '((fail)) *empty-bindings*)))

  ;; = (unify)    / == (equality after subst)
  (test-equal "= built-in binds" 'foo
              (solve-first '((= ?x foo)) '?x))

  (test-assert "== built-in succeeds"
               (let ((r (reset (prove-all '((= ?x foo) (== ?x foo)) *empty-bindings*))))
                 (not (failure? r))))

  ;; not
  ;; Note: '_' is not a special variable here, just a symbol. The test works because
  ;; there are no facts for (parent susan ...), so the goal fails, and `not` succeeds.
  (test-assert "not/1 succeeds"
               (let ((r (reset (prove-all '((not (parent susan _))) *empty-bindings*))))
                 (not (failure? r))))

  ;; call
  (test-equal "call/1 meta" 'mary
              (solve-first '((call (parent john ?x))) '?x))

  ;; if/3        if(Cond,Then,Else)
  (test-equal "if/3 then-branch" 'yes
              (solve-first '((if (= a a) (= ?r yes) (= ?r no))) '?r))
  (test-equal "if/3 else-branch" 'no
              (solve-first '((if (= a b) (= ?r yes) (= ?r no))) '?r))

  ;; lisp / is
  (test-equal "lisp-in-Prolog expr" 7
              (solve-first '((is ?v (+ 3 4))) '?v))

  ;; bagof / setof
  (test-equal "bagof collects order-preserved" '(mary michael)
              (solve-first '((bagof ?c (parent john ?c) ?list)) '?list))

  (test-equal "setof sorts & dedups" '(david susan)
              (solve-first '((setof ?x (ancestor john ?x) ?out)) '?out))

  ;; cut  (confirming effect)
  (test-equal "cut prevents backtracking" '(1)
              (solve-all '((bar ?v)) '?v)))

;;; ------------------------------------------------------------
;;; 7.5. Type & Dynamic Predicates
;;; ------------------------------------------------------------
(test-group "type-and-dynamic-predicates"
  (test-assert "atom/1 success"
               (not (failure? (reset (prove-all '((atom foo)) *empty-bindings*)))))
  (test-assert "atom/1 failure"
               (failure? (reset (prove-all '((atom ?X)) *empty-bindings*))))
  (test-assert "atomic/1 success"
               (not (failure? (reset (prove-all '((atomic 123)) *empty-bindings*)))))
  (test-assert "atomic/1 failure"
               (failure? (reset (prove-all '((atomic (a b))) *empty-bindings*))))
  (test-assert "var/1 success"
               (not (failure? (reset (prove-all '((var ?X)) *empty-bindings*)))))
  (test-assert "var/1 failure"
               (failure? (reset (prove-all '((= ?X 1) (var ?X)) *empty-bindings*))))
  (test-assert "ground/1 success"
               (not (failure? (reset (prove-all '((= ?X (a b)) (ground ?X)) *empty-bindings*)))))
  (test-assert "ground/1 failure"
               (failure? (reset (prove-all '((ground (a ?Y))) *empty-bindings*))))
  (test-assert "number/1 success"
               (not (failure? (reset (prove-all '((number 42)) *empty-bindings*)))))
  (test-assert "number/1 failure"
               (failure? (reset (prove-all '((number abc)) *empty-bindings*))))
  (test-equal "dynamic-put/get" 42
              (solve-first '((dynamic-put my-var 42) (dynamic-get my-var ?V)) '?V)))

;;; ------------------------------------------------------------
;;; 8. Library predicates (or/member/append/repeat/true)
;;; ------------------------------------------------------------
(test-group "library-predicates"
  ;; or/2
  (test-equal "or/2 first goal succeeds" 'ok
              (solve-first '((or (= ?r ok) (= ?r ng))) '?r))

  ;; member/2
  (test-assert "member/2 true"
               (let ((r (reset (prove-all '((member b (a b c))) *empty-bindings*))))
                 (not (failure? r))))

  ;; append/3
  (test-equal "append/3 concat" '(a b c d)
              (solve-first '((append (a b) (c d) ?x)) '?x))

  ;; repeat/0   (take only 3 times)
  (test-assert "repeat/0 at least three successes"
               (let loop ((cont (lambda () (prove-all '((repeat)) *empty-bindings*)))
                          (count 0))
                 (if (>= count 3)
                     #t
                     (let ((result (cont)))
                       (if (failure? result)
                           #f
                           (loop (cdr result) (+ count 1)))))))

  ;; true/0
  (test-assert "true/0 succeeds"
               (let ((r (reset (prove-all '((true)) *empty-bindings*))))
                 (not (failure? r)))))

;;; ------------------------------------------------------------
;;; 9. End tests & show results
;;; ------------------------------------------------------------
(test-end "prolog")

