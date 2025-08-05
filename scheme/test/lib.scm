;; -----------------------------------------------------------
;; Built‑ins
;; -----------------------------------------------------------
(test-group "built-ins"
  (parameterize ((current-clause-database (current-clause-database)))
    (<- (foo 1))
    (<- (foo 2))
    (<- (foo 3))
    (<- (bar ?x) (foo ?x) ! (= ?x 1))

    ;; facts for subsequent queries
    (<- (parent john mary))
    (<- (parent john michael))
    (<- (parent mary susan))
    (<- (parent michael david))
    (<- (grandparent ?x ?y) (parent ?x ?z) (parent ?z ?y))

    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))

    (test-assert "fail predicate" (null? (solve-all '((fail)) 'dummy)))
    (test-assert "true predicate" (not (null? (solve-all '((true)) 'dummy))))

    (test-equal "= binds simple var" 'foo (solve-first '((= ?x foo)) '?x))
    (test-equal "= binds complex terms" '(b) (solve-first '((= (a ?y) (a (b)))) '?y))

    (test-assert "== succeeds for bound vars" (not (null? (solve-all '((= ?x foo) (== ?x foo)) 'dummy))))
    (test-assert "== fails for different vals" (null? (solve-all '((= ?x foo) (== ?x bar)) 'dummy)))

    (test-assert "not/1 succeeds" (not (null? (solve-all '((not (parent susan ?_))) 'dummy))))
    (test-assert "not/1 fails" (null? (solve-all '((not (parent john mary))) 'dummy)))

    (test-equal "call/1 simple" 'mary (solve-first '((call (parent john ?x))) '?x))
    (test-equal "call/1 with conjunction" '(mary michael susan david) (solve-all '((call (ancestor john ?gc))) '?gc))
    ;; Variadic call form
    (test-equal "call variadic simple" 'mary (solve-first '((call parent john ?x)) '?x))
    (test-equal "call variadic with conjunction" '(mary michael susan david)
      (solve-all '((call ancestor john ?gc)) '?gc))
    ;; Compound predicate with extra arguments
    (test-equal "call compound+args" 'mary (solve-first '((call (parent john) ?x)) '?x))
    (test-equal "call compound+args conjunction" '(mary michael susan david)
      (solve-all '((call (ancestor john) ?gc)) '?gc))

    (test-equal "if/3 then" 'yes (solve-first '((if (= a a) (= ?r yes) (= ?r no))) '?r))
    (test-equal "if/3 else" 'no (solve-first '((if (= a b) (= ?r yes) (= ?r no))) '?r))
    (test-equal "if/2 then" 'ok (solve-first '((if (parent john mary) (= ?r ok))) '?r))
    (test-equal "if/2 else fails" #t (null? (solve-all '((if (= a b) (= ?r ok))) 'dummy)))

    (test-equal "is (lisp alias)" 7 (solve-first '((is ?v (+ 3 4))) '?v))
    (test-equal "lisp/2" 10 (solve-first '((lisp ?res (* 5 2))) '?res))

    (<- (item a))
    (<- (item b))
    (<- (item a))
    (<- (likes mary food))
    (<- (likes mary wine))
    (<- (likes mary music))
    (<- (likes john food))
    (<- (likes john music))
    (<- (likes bob food))
    (<- (age mary 25))
    (<- (age john 30))
    (<- (age bob 35))
    (<- (owns mary car))
    (<- (owns mary book))
    (<- (owns mary car))
    (<- (color red))
    (<- (color blue))
    (<- (color red))
    (<- (color green))

    ;; Edge case tests
    (<- (digit 1))
    (<- (digit 2))
    (<- (digit 1))

    (test-equal "! prunes choices" '(1) (solve-all '((bar ?v)) '?v))
    (test-equal "no ! finds all" '(1 2 3) (solve-all '((foo ?x) (= ?x ?x)) '?x))))

;; -----------------------------------------------------------
;; Type & Dynamic predicates
;; -----------------------------------------------------------
(test-group "type-and-dynamic-predicates"
  (test-assert "atom/1 success" (not (null? (solve-all '((atom foo)) 'dummy))))
  (test-assert "atom/1 failure on var" (null? (solve-all '((atom ?X)) 'dummy)))
  (test-assert "atomic/1 success on number" (not (null? (solve-all '((atomic 123)) 'dummy))))
  (test-assert "atomic/1 failure on list" (null? (solve-all '((atomic (a b))) 'dummy)))
  (test-assert "var/1 success on unbound" (not (null? (solve-all '((var ?X)) 'dummy))))
  (test-assert "var/1 failure on bound" (null? (solve-all '((= ?X 1) (var ?X)) 'dummy)))
  (test-assert "ground/1 success on bound var" (not (null? (solve-all '((= ?X (a b)) (ground ?X)) 'dummy))))
  (test-assert "ground/1 failure on partial" (null? (solve-all '((ground (a ?Y))) 'dummy)))
  (test-assert "number/1 success integer" (not (null? (solve-all '((number 42)) 'dummy))))
  (test-assert "number/1 success negative" (not (null? (solve-all '((number -17)) 'dummy))))
  (test-assert "number/1 success zero" (not (null? (solve-all '((number 0)) 'dummy))))
  (test-assert "number/1 success float" (not (null? (solve-all '((number 3.14)) 'dummy))))
  (test-assert "number/1 success negative float" (not (null? (solve-all '((number -2.5)) 'dummy))))
  (test-assert "number/1 failure on atom" (null? (solve-all '((number abc)) 'dummy)))
  (test-assert "number/1 failure on string" (null? (solve-all '((number "123")) 'dummy)))
  (test-assert "number/1 failure on list" (null? (solve-all '((number (1 2 3))) 'dummy)))
  (test-assert "number/1 failure on unbound" (null? (solve-all '((number ?)) 'dummy)))
  (test-assert "string/1 success" (not (null? (solve-all '((string "hello")) 'dummy))))
  (test-assert "string/1 failure on atom" (null? (solve-all '((string hello)) 'dummy)))
  (test-assert "string/1 failure on number" (null? (solve-all '((string 123)) 'dummy)))
  (test-assert "string/1 failure on unbound" (null? (solve-all '((string ?)) 'dummy)))

  (test-equal "dynamic-put/get" 42 (solve-first '((dynamic-put my-var 42) (dynamic-get my-var ?V)) '?V))
  (test-equal "dynamic-put overwrite" "new" (solve-first '((dynamic-put my-var "old") (dynamic-put my-var "new") (dynamic-get my-var ?V)) '?V)))

;; -----------------------------------------------------------
;; Library predicates (or/member/append/repeat/true)
;; -----------------------------------------------------------
(test-group "library"
  (test-equal "or variadic first succeeds"
    'ok
    (solve-first '((or (= ?r ok) (= ?r ng) (= ?r nope))) '?r))
  (test-equal "or variadic middle succeeds"
    'ok
    (solve-first '((or (= 1 2) (= ?r ok) (= ?r ng))) '?r))
  (test-equal "or variadic all solutions"
    '(ok also-ok stop)
    (solve-all '((or (= ?r ok) (= ?r also-ok) (= ?r stop))) '?r))

  (test-equal "and variadic success"
    '(ok yes)
    (solve-first '((and (= ?a ok) (= ?b yes))) '(?a ?b)))
  (test-assert "and variadic failure"
    (null? (solve-all '((and (= 1 2) (= ?x 3))) '?x)))

  (test-assert "member/2 success" (not (null? (solve-all '((member b (a b c))) 'dummy))))
  (test-assert "member/2 failure" (null? (solve-all '((member x (a b c))) 'dummy)))
  (test-equal "member/2 generate" '(a b c) (solve-all '((member ?x (a b c))) '?x))

  (test-equal "append/3 forward" '(a b c d) (solve-first '((append (a b) (c d) ?x)) '?x))
  (test-equal "append/3 backward" '(() (a b c)) (solve-first '((append ?x ?y (a b c))) '(?x ?y)))
  (test-equal "append/3 all splits" 4 (length (solve-all '((append ?x ?y (a b c))) '(?x ?y))))

  (test-assert "repeat generates multiple solutions"
    (let loop ((n 0))
      (if (>= n 5)
        #t
        (and (solve-first '((repeat)) 'dummy)
          (loop (+ n 1))))))
  (test-assert "true/0 always succeeds" (not (null? (solve-all '((true)) 'dummy))))
  (test-assert "and/0 behaves as true"
    (not (null? (solve-all '((and)) 'dummy))))
  (test-assert "or/0 behaves as fail"
    (null? (solve-all '((or)) 'dummy)))

  ;; maplist
  (test-assert "maplist atom success"
    (not (null? (solve-all '((maplist atom (a b c))) 'dummy))))
  (test-assert "maplist atom failure"
    (null? (solve-all '((maplist atom (a 1 c))) 'dummy)))
  (test-equal "maplist = unify lists"
    '(a b c)
    (solve-first '((maplist = (a b c) (?x ?y ?z))) '(?x ?y ?z))))
