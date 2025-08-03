;;; test/primitive.lisp - Prolog primitive predicate tests

(defpackage :prolog/test-primitive
  (:use :common-lisp :prolog/core :prolog/primitive :prolog/stdlib :prolog/test-helpers))

(in-package :prolog/test-primitive)

(defun run-tests ()
  (let ((*test-count* 0)
        (*pass-count* 0))
    
    ;; -----------------------------------------------------------
    ;; Built-ins
    ;; -----------------------------------------------------------
    (test-group "built-ins"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
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
        
        (test-assert "fail predicate" (null (solve-all '((fail)) 'dummy)))
        (test-assert "true predicate" (not (null (solve-all '((true)) 'dummy))))
        
        (test-equal "= binds simple var" 'foo (solve-first '((= ?x foo)) '?x))
        (test-equal "= binds complex terms" '(b) (solve-first '((= (a ?y) (a (b)))) '?y))
        
        (test-assert "== succeeds for bound vars" 
                     (not (null (solve-all '((= ?x foo) (== ?x foo)) 'dummy))))
        (test-assert "== fails for different vals" 
                     (null (solve-all '((= ?x foo) (== ?x bar)) 'dummy)))
        
        (test-assert "not/1 succeeds" 
                     (not (null (solve-all '((not (parent susan ?_))) 'dummy))))
        (test-assert "not/1 fails" 
                     (null (solve-all '((not (parent john mary))) 'dummy)))
        
        (test-equal "call/1 simple" 'mary 
                    (solve-first '((call (parent john ?x))) '?x))
        (test-equal "call/1 with conjunction" '(mary michael susan david)
                    (solve-all '((call (ancestor john ?gc))) '?gc))
        ;; Variadic call form
        (test-equal "call variadic simple" 'mary 
                    (solve-first '((call parent john ?x)) '?x))
        (test-equal "call variadic with conjunction" '(mary michael susan david)
                    (solve-all '((call ancestor john ?gc)) '?gc))
        ;; Compound predicate with extra arguments
        (test-equal "call compound+args" 'mary 
                    (solve-first '((call (parent john) ?x)) '?x))
        (test-equal "call compound+args conjunction" '(mary michael susan david)
                    (solve-all '((call (ancestor john) ?gc)) '?gc))
        
        (test-equal "if/3 then" 'yes 
                    (solve-first '((if (= a a) (= ?r yes) (= ?r no))) '?r))
        (test-equal "if/3 else" 'no 
                    (solve-first '((if (= a b) (= ?r yes) (= ?r no))) '?r))
        (test-equal "if/2 then" 'ok 
                    (solve-first '((if (parent john mary) (= ?r ok))) '?r))
        (test-equal "if/2 else fails" t 
                    (null (solve-all '((if (= a b) (= ?r ok))) 'dummy)))
        
        (test-equal "is (lisp alias)" 7 (solve-first '((is ?v (+ 3 4))) '?v))
        (test-equal "lisp/2" 10 (solve-first '((lisp ?res (* 5 2))) '?res))))
    
    ;; -----------------------------------------------------------
    ;; Type & Dynamic predicates
    ;; -----------------------------------------------------------
    (test-group "type-and-dynamic-predicates"
      (test-assert "atom/1 success" (not (null (solve-all '((atom foo)) 'dummy))))
      (test-assert "atom/1 failure on var" (null (solve-all '((atom ?X)) 'dummy)))
      (test-assert "atomic/1 success on number" (not (null (solve-all '((atomic 123)) 'dummy))))
      (test-assert "atomic/1 failure on list" (null (solve-all '((atomic (a b))) 'dummy)))
      (test-assert "var/1 success on unbound" (not (null (solve-all '((var ?X)) 'dummy))))
      (test-assert "var/1 failure on bound" (null (solve-all '((= ?X 1) (var ?X)) 'dummy)))
      (test-assert "ground/1 success on bound var" 
                   (not (null (solve-all '((= ?X (a b)) (ground ?X)) 'dummy))))
      (test-assert "ground/1 failure on partial" (null (solve-all '((ground (a ?Y))) 'dummy)))
      (test-assert "number/1 success integer" (not (null (solve-all '((number 42)) 'dummy))))
      (test-assert "number/1 success negative" (not (null (solve-all '((number -17)) 'dummy))))
      (test-assert "number/1 success zero" (not (null (solve-all '((number 0)) 'dummy))))
      (test-assert "number/1 success float" (not (null (solve-all '((number 3.14)) 'dummy))))
      (test-assert "number/1 success negative float" 
                   (not (null (solve-all '((number -2.5)) 'dummy))))
      (test-assert "number/1 failure on atom" (null (solve-all '((number abc)) 'dummy)))
      (test-assert "number/1 failure on string" (null (solve-all '((number "123")) 'dummy)))
      (test-assert "number/1 failure on list" (null (solve-all '((number (1 2 3))) 'dummy)))
      (test-assert "number/1 failure on unbound" (null (solve-all '((number ?)) 'dummy)))
      (test-assert "string/1 success" (not (null (solve-all '((string "hello")) 'dummy))))
      (test-assert "string/1 failure on atom" (null (solve-all '((string hello)) 'dummy)))
      (test-assert "string/1 failure on number" (null (solve-all '((string 123)) 'dummy)))
      (test-assert "string/1 failure on unbound" (null (solve-all '((string ?)) 'dummy)))
      
      (test-equal "dynamic-put/get" 42 
                  (solve-first '((dynamic-put my-var 42) (dynamic-get my-var ?V)) '?V))
      (test-equal "dynamic-put overwrite" "new" 
                  (solve-first '((dynamic-put my-var "old") 
                                 (dynamic-put my-var "new") 
                                 (dynamic-get my-var ?V)) '?V)))
    
    ;; -----------------------------------------------------------
    ;; Meta-predicates (bagof, setof, findall, sort)
    ;; -----------------------------------------------------------
    (test-group "meta-predicates"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; Set up test data - copy from earlier test group
        (<- (parent john mary))
        (<- (parent john michael))
        (<- (parent mary susan))
        (<- (parent michael david))
        
        ;; Additional test data
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
        
        ;; Basic findall tests
        (test-equal "findall gets all solutions" '(mary michael)
                    (solve-first '((findall ?c (parent john ?c) ?l)) '?l))
        (test-equal "findall with no solutions" '()
                    (solve-first '((findall ?x (parent susan ?x) ?l)) '?l))
        
        ;; Basic bagof tests
        (test-equal "bagof gets all solutions" '(mary michael)
                    (solve-first '((bagof ?c (parent john ?c) ?l)) '?l))
        (test-equal "bagof with compound template" '((likes mary food) (likes mary wine) (likes mary music))
                    (solve-first '((bagof (likes mary ?x) (likes mary ?x) ?l)) '?l))
        (test-equal "bagof preserves duplicates" '(a b a)
                    (solve-first '((bagof ?x (item ?x) ?l)) '?l))
        (test-assert "bagof fails with no solutions"
                     (null (solve-all '((bagof ?x (parent susan ?x) ?l)) 'dummy)))
        
        ;; Basic setof tests
        (test-equal "setof removes duplicates" '(a b)
                    (solve-first '((setof ?x (item ?x) ?l)) '?l))
        (test-equal "setof removes duplicates from colors" '(blue green red)
                    (solve-first '((setof ?x (color ?x) ?l)) '?l))
        (test-equal "setof sorts results" '(book car)
                    (solve-first '((setof ?x (owns mary ?x) ?l)) '?l))
        (test-assert "setof fails with no solutions"
                     (null (solve-all '((setof ?x (parent susan ?x) ?l)) 'dummy)))
        
        ;; Sort tests
        (test-equal "sort numbers" '(1 2 3) (solve-first '((sort (3 1 2) ?s)) '?s))
        (test-equal "sort removes duplicates" '(1 2 3) 
                    (solve-first '((sort (3 1 2 3 1) ?s)) '?s))))
    
    ;; -----------------------------------------------------------
    ;; Type checking predicates (additional tests)
    ;; -----------------------------------------------------------
    (test-group "type-predicates-extended"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; ground/1 tests
        (test-assert "ground with no variables"
                     (not (null (solve-all '((ground (a b c))) 'dummy))))
        (test-assert "ground fails with variable"
                     (null (solve-all '((ground ?x)) 'dummy)))
        (test-assert "ground fails with nested variable"
                     (null (solve-all '((ground (a ?x c))) 'dummy)))
        (test-assert "ground succeeds after binding"
                     (not (null (solve-all '((= ?x 5) (ground ?x)) 'dummy))))
        
        ;; number/1 tests
        (test-assert "number with integer"
                     (not (null (solve-all '((number 42)) 'dummy))))
        (test-assert "number with float"
                     (not (null (solve-all '((number 3.14)) 'dummy))))
        (test-assert "number fails with atom"
                     (null (solve-all '((number foo)) 'dummy)))
        (test-assert "number fails with list"
                     (null (solve-all '((number (1 2 3))) 'dummy)))
        
        ;; string/1 tests
        (test-assert "string with string literal"
                     (not (null (solve-all '((string "hello")) 'dummy))))
        (test-assert "string fails with number"
                     (null (solve-all '((string 42)) 'dummy)))
        (test-assert "string fails with atom"
                     (null (solve-all '((string hello)) 'dummy)))))
    
    ;; -----------------------------------------------------------
    ;; Equality predicates (=/2 vs ==/2)
    ;; -----------------------------------------------------------
    (test-group "equality-predicates"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; =/2 unifies, ==/2 checks strict equality
        (test-equal "= unifies variables" 5
                    (solve-first '((= ?x 5)) '?x))
        (test-assert "== fails with unbound variable"
                     (null (solve-all '((== ?x 5)) 'dummy)))
        (test-assert "== succeeds with identical terms"
                     (not (null (solve-all '((= ?x 5) (== ?x 5)) 'dummy))))
        (test-assert "== checks structural equality"
                     (not (null (solve-all '((== (a b c) (a b c))) 'dummy))))
        (test-assert "== fails on different structures"
                     (null (solve-all '((== (a b c) (a b d))) 'dummy)))))
    
    ;; -----------------------------------------------------------
    ;; Dynamic variable storage
    ;; -----------------------------------------------------------
    (test-group "dynamic-variables"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; Basic put/get
        (test-equal "dynamic-put and dynamic-get" 42
                    (solve-first '((dynamic-put myvar 42) (dynamic-get myvar ?x)) '?x))
        (test-equal "dynamic-put overwrites" 100
                    (solve-first '((dynamic-put myvar 42) 
                                   (dynamic-put myvar 100) 
                                   (dynamic-get myvar ?x)) '?x))
        ;; Complex values
        (test-equal "dynamic-put with list" '(a b c)
                    (solve-first '((dynamic-put mylist (a b c)) 
                                   (dynamic-get mylist ?x)) '?x))
        ;; Undefined variable
        (test-assert "dynamic-get undefined fails"
                     (null (solve-all '((dynamic-get undefined-var ?x)) 'dummy)))))
    
    ;; -----------------------------------------------------------
    ;; is/2 arithmetic evaluation
    ;; -----------------------------------------------------------
    (test-group "arithmetic-evaluation"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; Basic arithmetic
        (test-equal "is addition" 7 (solve-first '((is ?x (+ 3 4))) '?x))
        (test-equal "is subtraction" 3 (solve-first '((is ?x (- 10 7))) '?x))
        (test-equal "is multiplication" 24 (solve-first '((is ?x (* 6 4))) '?x))
        (test-equal "is division" 5 (solve-first '((is ?x (/ 20 4))) '?x))
        
        ;; Nested expressions
        (test-equal "is nested expression" 25 
                    (solve-first '((is ?x (+ (* 3 5) (- 20 10)))) '?x))
        
        ;; With variables
        (test-equal "is with bound variable" 15
                    (solve-first '((= ?y 5) (is ?x (* ?y 3))) '?x))
        
        ;; Error cases
        (test-assert "is fails with unbound variable in expression"
                     (null (solve-all '((is ?x (+ ?y 5))) 'dummy)))))
    
    ;; -----------------------------------------------------------
    ;; lisp/2 - direct lisp evaluation
    ;; -----------------------------------------------------------
    (test-group "lisp-evaluation"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; Basic lisp calls
        (test-equal "lisp list creation" '(1 2 3)
                    (solve-first '((lisp ?x (list 1 2 3))) '?x))
        (test-equal "lisp string manipulation" "HELLO"
                    (solve-first '((lisp ?x (string-upcase "hello"))) '?x))
        
        ;; With prolog variables
        (test-equal "lisp with bound variables" 10
                    (solve-first '((= ?a 5) (= ?b 2) (lisp ?x (* ?a ?b))) '?x))))
    
    ;; -----------------------------------------------------------
    ;; true/0 predicate
    ;; -----------------------------------------------------------
    (test-group "true-predicate"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        (test-assert "true always succeeds"
                     (not (null (solve-all '((true)) 'dummy))))
        (test-equal "true in conjunction" 42
                    (solve-first '((true) (= ?x 42)) '?x))))
    
    (format t "~%Total tests: ~D, Passed: ~D~%" *test-count* *pass-count*)))

;; Run tests when loaded
(run-tests)
