(define (solve-first goals term)
  (let ((ss (make-solution-stream goals)))
    (if (stream-null? ss)
      '()
      (substitute-bindings (stream-car ss) term))))

(define (solve-all goals term)
  (let loop ((ss (make-solution-stream goals)))
    (if (stream-null? ss)
      '()
      (cons (substitute-bindings (stream-car ss) term)
            (loop (stream-cdr ss))))))

(test-begin "prolog")

;; -----------------------------------------------------------
;; Low-level helpers
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

  (test-equal "cons add binding empty" '((?x . 1)) (cons (cons '?x 1) '()))
  (test-equal "cons add binding non-empty" '((?y . 2) (?x . 1)) (cons (cons '?y 2) '((?x . 1))))

  (let* ((bindings (cons (cons '?x 'foo) (cons (cons '?y '(bar)) '()))))
    (test-equal "substitute-bindings simple" 'foo (substitute-bindings bindings '?x))
    (test-equal "substitute-bindings with list" '(g foo (bar)) (substitute-bindings bindings '(g ?x ?y))))

  (let* ((cycle-bindings (cons (cons '?x '?y)
                               (cons (cons '?y '?x) '()))))
    (test-equal "substitute-bindings simple cycle"
                '?x
                (substitute-bindings cycle-bindings '?x)))

  (let* ((nested-bindings (cons (cons '?x '(a ?y))
                                (cons (cons '?y '(b ?x)) '()))))
    (test-equal "substitute-bindings nested cycle"
                '(a (b ?x))
                (substitute-bindings nested-bindings '?x)))

  (test-equal "variables-in simple" '(?x ?y) (variables-in '(f ?x (g ?y ?x))))
  (test-equal "variables-in with no vars" '() (variables-in '(a b (c))))

  (let ((expr-with-anon (replace-anonymous-variables '(a ? (?x ?)))))
    (test-assert "anonymous vars replaced" (not (memq '? expr-with-anon))))
  (let ((expr-no-anon (replace-anonymous-variables '(a b))))
    (test-equal "replace-anonymous-variables with no anon vars" '(a b) expr-no-anon)))

;; -----------------------------------------------------------
;; Unification
;; -----------------------------------------------------------
(test-group "unify"
  (test-equal "unify atom-atom success" '() (unify 'a 'a '()))
  (test-assert "unify atom-atom mismatch" (failure? (unify 'a 'b '())))
  (test-assert "unify atom-list mismatch" (failure? (unify 'a '(a) '())))

  (test-equal "unify var-atom" '((?x . a)) (unify '?x 'a '()))
  (test-equal "unify var-list" '((?x . (a b))) (unify '?x '(a b) '()))

  (test-equal "unify var-var link" '((?x . ?y)) (unify '?x '?y '()))
  (let ((bindings (unify '?x '?y (unify '?y 'val '()))))
    (test-equal "unify var-var with one bound" 'val (substitute-bindings bindings '?x)))

  (test-equal "unify list-list success" '((?x . c)) (unify '(a b ?x) '(a b c) '()))
  (test-assert "unify list-list failure (length)" (failure? (unify '(a b) '(a b c) '())))
  (test-assert "unify list-list failure (element)" (failure? (unify '(a x) '(a y) '())))

  (test-assert "occurs-check simple" (failure? (unify '?x '(foo ?x) '())))
  (test-assert "occurs-check nested" (failure? (unify '?y '(bar (baz ?y)) '()))))
  

;; -----------------------------------------------------------
;; API function coverage
;; -----------------------------------------------------------
(test-group "exports"
  ;; object->string
  (test-equal "object->string pair" "(1 2)" (object->string '(1 2)))

  ;; remove-clauses-with-arity!
  (parameterize ((current-clause-database (current-clause-database)))
    (current-clause-database '())
    (<- (r 1))
    (<- (r 1 2))
    (remove-clauses-with-arity! 'r 1)
    (test-equal "arity removal" 1 (length (get-clauses 'r))))

  ;; primitive vs standard databases
  (test-assert "standard has maplist" (assoc 'maplist (standard-clause-database)))
  (test-assert "primitive lacks maplist" (not (assoc 'maplist (primitive-clause-database))))

  ;; prove-all and success structures
  (let ((res (prove-all '((= ?x 1) (member ?y (1 2))) '())))
    (test-assert "prove-all success" (success? res))
    (test-equal "success ?x" 1 (substitute-bindings (success-bindings res) '?x))
    (test-equal "success ?y" 1 (substitute-bindings (success-bindings res) '?y))
    (let ((next ((success-continuation res))))
      (test-assert "success-continuation" (success? next))
      (test-equal "next ?y" 2 (substitute-bindings (success-bindings next) '?y))))

  ;; call-with-current-choice-point
  (test-equal "call-with-current-choice-point" 'ok
              (call-with-current-choice-point (lambda (tag) 'ok)))

  ;; current-lisp-environment via eval
  (test-equal "eval env" 42 (eval '42 (current-lisp-environment)))

  ;; make-solution-stream
  (let* ((ss (make-solution-stream '((= ?z 5))))
         (first (stream-car ss)))
    (test-equal "make-solution-stream" '((?z . 5)) first))

  ;; run-query success
  (let ((out (open-output-string))
        (in (open-input-string "n\n")))
    (parameterize ((current-input-port in)
                   (current-output-port out))
      (run-query '((= ?a 1))))
    (let ((txt (get-output-string out)))
      (test-assert "run-query success" (not (string=? txt "No.\n")))))

  ;; run-query failure
  (let ((out (open-output-string))
        (in (open-input-string "")))
    (parameterize ((current-input-port in)
                   (current-output-port out))
      (run-query '((fail))))
    (test-equal "run-query failure" "No.\n" (get-output-string out)))

  ;; ?- macro
  (let ((out (open-output-string))
        (in (open-input-string "n\n")))
    (parameterize ((current-input-port in)
                   (current-output-port out))
      (?- (= ?v ok)))
    (test-assert "?- macro" (not (string=? (get-output-string out) "No.\n")))))

;; -----------------------------------------------------------
;; Clause DB operations
;; -----------------------------------------------------------
(test-group "clause-db"
  (parameterize ((current-clause-database (current-clause-database)))
    (current-clause-database '())
    (let ((clause1 '((parent alice bob)))
          (clause2 '((parent alice carol))))
      (add-clause! clause1)
      (test-equal "get-clauses after one add" (list clause1) (get-clauses 'parent))
      (add-clause! clause2)
      (test-equal "get-clauses after two adds" (list clause1 clause2) (get-clauses 'parent))
      (test-equal "get-clauses for unknown pred" '() (get-clauses 'unknown))

      (<-- (father ?x ?y) (male ?x) (parent ?x ?y))
      (test-equal "<- overwrites" 1 (length (get-clauses 'father))))))


;; -----------------------------------------------------------
;; Engine simple recursion
;; -----------------------------------------------------------
(test-group "engine"
  (parameterize ((current-clause-database (current-clause-database)))
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
    (test-assert "failing goal" (null? (solve-all '((parent david ?x)) '?x)))))

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

   (<- (item a)) (<- (item b)) (<- (item a))
   (test-equal "bagof gets all solutions" '(mary michael) (solve-first '((bagof ?c (parent john ?c) ?l)) '?l))
   (test-equal "findall gets all solutions" '(mary michael) (solve-first '((findall ?c (parent john ?c) ?l)) '?l))
   (test-equal "findall with no solutions" '() (solve-first '((findall ?x (parent susan ?x) ?l)) '?l))
   (test-equal "bagof groups by free vars"
               '((john (mary michael)) (mary (susan)) (michael (david)))
               (solve-all '((bagof ?C (parent ?P ?C) ?L)) '(?P ?L)))

   (test-equal "sort numbers" '(1 2 3) (solve-first '((sort (3 1 2) ?s)) '?s))
   (test-equal "sort removes duplicates" '(1 2 3) (solve-first '((sort (3 1 2 3 1) ?s)) '?s))
   (test-equal "sort removes duplicates" '(1 2 3 ?z) (solve-first '((sort (3 ?z 2 3 1) ?s)) '?s))
   
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
  (test-assert "number/1 success" (not (null? (solve-all '((number 42)) 'dummy))))
  (test-assert "number/1 failure on atom" (null? (solve-all '((number abc)) 'dummy)))
  
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
  

;; -----------------------------------------------------------
;; Zero-arity predicate definitions
;; -----------------------------------------------------------

(test-group "zero-arity-predicates"
  (parameterize ((current-clause-database (current-clause-database)))
    (<- hello)
    ;; body also calls hello without parentheses
    (<- greet hello)
    ;; calls using traditional (name) syntax
    (test-assert "zero-arity fact" (not (null? (solve-all '((hello)) 'dummy))))
    (test-assert "zero-arity rule" (not (null? (solve-all '((greet)) 'dummy))))
    ;; calls using bare predicate symbol
    (test-assert "zero-arity fact bare" (not (null? (solve-all '(hello) 'dummy))))
    (test-assert "zero-arity rule bare" (not (null? (solve-all '(greet) 'dummy))))))
    

;; -----------------------------------------------------------
;; Variadic predicate definitions
;; -----------------------------------------------------------

(test-group "variadic-predicates"
  (parameterize ((current-clause-database (current-clause-database)))
    (<- (capture-rest ?out ?first . ?rest) (= ?out ?rest))
    (test-equal "variadic rest collects args"
                '(b c)
                (solve-first '((capture-rest ?r a b c)) '?r))
    (test-equal "variadic rest empty"
                '()
                (solve-first '((capture-rest ?r a)) '?r))
    (test-assert "variadic too few arguments"
                 (null? (solve-all '((capture-rest ?r)) '?r)))))

;; -----------------------------------------------------------
;; Cut behavior in control structures
;; -----------------------------------------------------------

(test-group "cut-behavior"
  (parameterize ((current-clause-database (current-clause-database)))
    (<- (p a))
    (<- (p b))
    (<- (p c))
    (<- (q 1))
    (<- (q 2))
    (<- (s ?x ?y)
        (p ?x)
        (or (and (q ?y) (== ?x b) !)
            (q ?y)))

    (test-equal "No Cut (Baseline)"
      '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))
      (solve-all '((p ?X) (q ?Y)) '(?X ?Y)))

    (test-equal "Simple Cut in 'and'"
      '((a 1) (a 2))
      (solve-all '((p ?X) ! (q ?Y)) '(?X ?Y)))

    ;; This may appear incorrect in Prolog, but defining 'or' and 'and'
    ;; predicates first in Prolog yields the same result.
    (test-equal "Cut inside 'or'"
      '(b a b c)
      (solve-all '((or (and (p ?X) (== ?X b) !) (p ?X))) '?X))
    
    (test-equal "'and' with 'or' containing '!'"
      '(c)
      (solve-all '((and (p ?X) (or (and (== ?X c) !) (fail)))) '?X))

    (test-equal "'call' with 'or' containing '!'"
      '(c)
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
  (parameterize ((current-clause-database (current-clause-database)))
    ;; test predicates
    (<- (q 1))
    (<- (q 2))
    
    (<-- (p ?x) (= ?x 1) ! (fail)) ; p(1) triggers a hard failure
    (<- (p ?x) (= ?x 2))             ; p(2) succeeds
    
    (test-equal "hard failure in p(x) should not block backtracking in q(y)"
      2
      (solve-first '((q ?y) (p ?y)) '?y))))
    


(test-group "spy-behavior"
  (parameterize ((current-clause-database (current-clause-database)))
    (current-clause-database '())
    (<- (watched))
    (let ((out (open-output-string))
          (in (open-input-string "l"))
          (result ""))
      (parameterize ((current-spy-predicates '(watched))
                     (current-input-port in)
                     (current-output-port out))
        (solve-all '((watched)) 'dummy)
        (set! result (get-output-string out)))
      (test-equal "spy output"
                  "Spy on (watched)? [l=leap c=creep n=nodebug b=break] CALL: (watched)\nEXIT: (watched)\n"
                  result))))

(test-group "spy-indent-toggle"
  (parameterize ((current-clause-database (current-clause-database)))
    (current-clause-database '())
    (<- child)
    (<- (parent) (child))
    ;; indentation enabled
    (let ((out1 (open-output-string))
          (in1 (open-input-string "l"))
          (result1 ""))
      (parameterize ((current-spy-predicates '(parent child))
                     (current-input-port in1)
                     (current-output-port out1))
        (solve-all '((parent)) 'dummy)
        (set! result1 (get-output-string out1)))
      (test-equal "indent on"
                  "CALL: (parent)\n CALL: (child)\n EXIT: (child)\nEXIT: (parent)\n"
                  result1))
    ;; indentation disabled
    (let ((out2 (open-output-string))
          (in2 (open-input-string "l"))
          (result2 ""))
      (parameterize ((current-spy-predicates '(parent child))
                     (current-input-port in2)
                     (current-spy-indent? #f)
                     (current-output-port out2))
        (solve-all '((parent)) 'dummy)
        (set! result2 (get-output-string out2)))
      (test-equal "indent off"
                  "CALL: (parent)\nCALL: (child)\nEXIT: (child)\nEXIT: (parent)\n"
                  result2))))

  

(test-end "prolog")
