;;; test/stdlib.lisp - Prolog standard library tests

(defpackage :prolog/test-stdlib
  (:use :common-lisp :prolog/core :prolog/primitive :prolog/stdlib :prolog/test-helpers))

(in-package :prolog/test-stdlib)

(defun run-tests ()
  (let ((*test-count* 0)
        (*pass-count* 0))
    
    ;; -----------------------------------------------------------
    ;; Library predicates (and/or/member/append/repeat/maplist)
    ;; -----------------------------------------------------------
    (test-group "library"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; Set up test data
        (<- (parent john mary))
        (<- (parent john michael))
        (<- (parent mary susan))
        (<- (parent michael david))
        
        ;; Define repeat in the test package to ensure it's accessible
        ;; (The clauses in stdlib are in the prolog/stdlib package)
        (<- (repeat))
        (<- (repeat) (repeat))
        
        ;; Define maplist and helpers in test package
        (<- (--all-null ()))
        (<- (--all-null (() . ?rest)) (--all-null ?rest))
        
        (<- (--get-heads () ()))
        (<- (--get-heads ((?head . ?) . ?rest) (?head . ?heads))
            (--get-heads ?rest ?heads))
        
        (<- (--get-tails () ()))
        (<- (--get-tails ((? . ?tail) . ?rest) (?tail . ?tails))
            (--get-tails ?rest ?tails))
        
        (<- (maplist ?pred . ?lists)
            (if (--all-null ?lists)
                true
                (and (--get-heads ?lists ?heads)
                     (--get-tails ?lists ?tails)
                     (call (?pred . ?heads))
                     (call (maplist ?pred . ?tails)))))
        
        ;; Test or predicate
        (test-equal "or variadic first succeeds" 'ok
                    (solve-first '((or (= ?r ok) (= ?r ng) (= ?r nope))) '?r))
        (test-equal "or variadic middle succeeds" 'ok
                    (solve-first '((or (= 1 2) (= ?r ok) (= ?r ng))) '?r))
        (test-equal "or variadic all solutions" '(ok also-ok stop)
                    (solve-all '((or (= ?r ok) (= ?r also-ok) (= ?r stop))) '?r))
        
        ;; Test and predicate
        (test-equal "and variadic success" '(ok yes)
                    (solve-first '((and (= ?a ok) (= ?b yes))) '(?a ?b)))
        (test-assert "and variadic failure"
                     (null (solve-all '((and (= 1 2) (= ?x 3))) '?x)))
        
        ;; Test member predicate
        (test-assert "member/2 success" (not (null (solve-all '((member b (a b c))) 'dummy))))
        (test-assert "member/2 failure" (null (solve-all '((member x (a b c))) 'dummy)))
        (test-equal "member/2 generate" '(a b c) (solve-all '((member ?x (a b c))) '?x))
        
        ;; Test append predicate
        (test-equal "append/3 forward" '(a b c d) 
                    (solve-first '((append (a b) (c d) ?x)) '?x))
        (test-equal "append/3 backward" '(() (a b c)) 
                    (solve-first '((append ?x ?y (a b c))) '(?x ?y)))
        (test-equal "append/3 all splits" 4 
                    (length (solve-all '((append ?x ?y (a b c))) '(?x ?y))))
        
        ;; Test repeat predicate
        ;; Use a more robust approach with catch/throw instead of block/return-from
        (test-assert "repeat generates multiple solutions"
                     (catch 'repeat-test
                       (let ((count 0))
                         (solve '((repeat))
                                (lambda (solution) 
                                  (declare (ignore solution))
                                  (incf count)
                                  (when (>= count 5) 
                                    (throw 'repeat-test t)))
                                (lambda () 
                                  (throw 'repeat-test nil))))))
        
        ;; Test and/0 and or/0
        (test-assert "and/0 behaves as true"
                     (not (null (solve-all '((and)) 'dummy))))
        (test-assert "or/0 behaves as fail"
                     (null (solve-all '((or)) 'dummy)))
        
        ;; Test maplist predicate
        (test-assert "maplist atom success"
                     (not (null (solve-all '((maplist atom (a b c))) 'dummy))))
        (test-assert "maplist atom failure"
                     (null (solve-all '((maplist atom (a 1 c))) 'dummy)))
        (test-equal "maplist = unify lists" '(a b c)
                    (solve-first '((maplist = (a b c) (?x ?y ?z))) '(?x ?y ?z)))))
    
    ;; -----------------------------------------------------------
    ;; Control flow predicates (not/if-then-else)
    ;; -----------------------------------------------------------
    (test-group "control-flow"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; Test not/1 predicate
        (test-assert "not succeeds when goal fails"
                     (not (null (solve-all '((not (= 1 2))) 'dummy))))
        (test-assert "not fails when goal succeeds"
                     (null (solve-all '((not (= 1 1))) 'dummy)))
        (test-equal "not with unification" 'b
                    (solve-first '((= ?x b) (not (= ?x a))) '?x))
        
        ;; Test if-then-else
        (test-equal "if-then-else true condition" 'yes
                    (solve-first '((if (= 1 1) (= ?x yes) (= ?x no))) '?x))
        (test-equal "if-then-else false condition" 'no
                    (solve-first '((if (= 1 2) (= ?x yes) (= ?x no))) '?x))
        
        ;; Test if-then (without else)
        (test-equal "if-then true condition" 'ok
                    (solve-first '((if (= 1 1) (= ?x ok))) '?x))
        (test-assert "if-then false condition fails"
                     (null (solve-all '((if (= 1 2) (= ?x ok))) '?x)))
        
        ;; Complex if-then-else with cut behavior
        (<- (test-pred a))
        (<- (test-pred b))
        (test-equal "if-then-else with backtracking" 'found-a
                    (solve-first '((test-pred ?x) 
                                   (if (= ?x a) 
                                       (= ?result found-a) 
                                       (= ?result found-other))) 
                                 '?result))))
    
    ;; -----------------------------------------------------------
    ;; List manipulation predicates (extended tests)
    ;; -----------------------------------------------------------
    (test-group "list-manipulation-extended"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; append/3 edge cases
        (test-equal "append empty lists" '()
                    (solve-first '((append () () ?x)) '?x))
        (test-equal "append with empty first list" '(a b c)
                    (solve-first '((append () (a b c) ?x)) '?x))
        (test-equal "append with empty second list" '(a b c)
                    (solve-first '((append (a b c) () ?x)) '?x))
        
        ;; member/2 edge cases
        (test-assert "member in empty list fails"
                     (null (solve-all '((member ?x ())) '?x)))
        (test-equal "member generates singleton" 'a
                    (solve-first '((member ?x (a))) '?x))
        
        ;; maplist with multiple lists
        ;; This should unify corresponding elements
        (test-equal "maplist = unify lists" '(a b c)
                    (solve-first '((maplist = (a b c) (?x ?y ?z))) '(?x ?y ?z)))
        
        ;; Helper predicates (--all-null, --get-heads, --get-tails)
        (test-assert "--all-null empty list"
                     (not (null (solve-all '((--all-null ())) 'dummy))))
        (test-assert "--all-null with all empty lists"
                     (not (null (solve-all '((--all-null (() () ()))) 'dummy))))
        (test-assert "--all-null fails with non-empty"
                     (null (solve-all '((--all-null ((a) ()))) 'dummy)))
        
        (test-equal "--get-heads extracts heads" '(a b c)
                    (solve-first '((--get-heads ((a x) (b y) (c z)) ?heads)) '?heads))
        (test-equal "--get-heads empty input" '()
                    (solve-first '((--get-heads () ?heads)) '?heads))
        
        (test-equal "--get-tails extracts tails" '((x) (y) (z))
                    (solve-first '((--get-tails ((a x) (b y) (c z)) ?tails)) '?tails))
        (test-equal "--get-tails empty input" '()
                    (solve-first '((--get-tails () ?tails)) '?tails))))
    
    ;; -----------------------------------------------------------
    ;; Repeat predicate (bounded test)
    ;; -----------------------------------------------------------
    (test-group "repeat-bounded"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; Test repeat with counter
        (<- (count-repeat 0 ?max) !)
        (<- (count-repeat ?n ?max) 
            (is ?n1 (+ ?n 1))
            (repeat)
            (count-repeat ?n1 ?max))
        
        (test-equal "repeat generates exactly 5 solutions" 5
                    (let ((solutions 0))
                      (solve '((repeat))
                             (lambda (bindings)
                               (declare (ignore bindings))
                               (incf solutions)
                               (when (>= solutions 5)
                                 (error 'prolog/core::cut-exception :tag nil :value nil)))
                             (lambda () nil))
                      solutions))))
    
    ;; -----------------------------------------------------------
    ;; Complex nested control structures
    ;; -----------------------------------------------------------
    (test-group "nested-control-structures"
      (let ((*current-clause-database* (copy-list *current-clause-database*)))
        ;; Setup test predicates
        (<- (category ?x animal) (member ?x (dog cat bird)))
        (<- (category ?x plant) (member ?x (tree flower grass)))
        (<- (category ?x other))
        
        ;; Test nested or/and
        (test-equal "nested or in and" '(dog cat)
                    (solve-all '((and (or (category ?x animal) (category ?x plant))
                                      (or (= ?x dog) (= ?x cat) (= ?x tree)))) 
                               '?x))
        
        ;; Test if-then-else with not
        (test-equal "if with not condition" 'not-dog
                    (solve-first '((= ?x cat)
                                   (if (not (= ?x dog))
                                       (= ?result not-dog)
                                       (= ?result is-dog)))
                                 '?result))))
    
    (format t "~%Total tests: ~D, Passed: ~D~%" *test-count* *pass-count*)))

;; Run tests when loaded
(run-tests)