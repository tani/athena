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
    
    (format t "~%Total tests: ~D, Passed: ~D~%" *test-count* *pass-count*)))

;; Run tests when loaded
(run-tests)