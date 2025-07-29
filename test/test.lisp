;;; test.lisp — Test suite for the Prolog engine in Common Lisp
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(defpackage :prolog-test
  (:use :cl :prolog)
  (:import-from :prolog-core
                #:variable-p #:named-variable-p #:atom-p
                #:variables-in #:replace-anonymous-variables
                #:substitute-bindings #:unify
                #:failure-p #:success-p #:success-bindings
                #:get-clauses #:remove-clauses-with-arity
                #:prove-all #:min-arity
                #:*current-clause-database*)
  (:export #:run-tests #:main))

(in-package :prolog-test)

;;; Simple test framework

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)
(defvar *current-test-group* nil)

(defmacro test-begin (name)
  `(progn
     (setf *test-count* 0
           *pass-count* 0
           *fail-count* 0
           *current-test-group* nil)
     (format t "~%Running test suite: ~A~%" ,name)))

(defmacro test-group (name &body body)
  `(let ((*current-test-group* ,name))
     (format t "~%Test group: ~A~%" ,name)
     ,@body))

(defmacro test-equal (description expected actual)
  `(progn
     (incf *test-count*)
     (let ((expected-val ,expected)
           (actual-val ,actual))
       (if (equal expected-val actual-val)
           (progn
             (incf *pass-count*)
             (format t "  ✓ ~A~%" ,description))
           (progn
             (incf *fail-count*)
             (format t "  ✗ ~A~%" ,description)
             (format t "    Expected: ~S~%" expected-val)
             (format t "    Actual:   ~S~%" actual-val))))))

(defmacro test-assert (description condition)
  `(test-equal ,description t ,condition))

(defmacro test-end ()
  `(format t "~%Test Results: ~A passed, ~A failed (total: ~A)~%"
           *pass-count* *fail-count* *test-count*))

;;; Utility functions from test.scm

(defun solve-first (goals term)
  (prolog:solve-first goals term))

(defun solve-all (goals term)
  (prolog:solve-all goals term))

;;; Run the tests

(defun run-tests ()
  (test-begin "prolog")

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

    (test-equal "cons add binding empty" '((?x . 1)) (acons '?x 1 '()))
    (test-equal "cons add binding non-empty" '((?y . 2) (?x . 1)) 
                (acons '?y 2 '((?x . 1))))

    (let* ((bindings (acons '?x 'foo (acons '?y '(bar) '()))))
      (test-equal "substitute-bindings simple" 'foo 
                  (substitute-bindings bindings '?x))
      (test-equal "substitute-bindings with list" '(g foo (bar)) 
                  (substitute-bindings bindings '(g ?x ?y))))

    (let* ((cycle-bindings (acons '?x '?y (acons '?y '?x '()))))
      (test-equal "substitute-bindings simple cycle" '?x
                  (substitute-bindings cycle-bindings '?x)))

    (let* ((nested-bindings (acons '?x '(a ?y) (acons '?y '(b ?x) '()))))
      (test-equal "substitute-bindings nested cycle" '(a (b ?x))
                  (substitute-bindings nested-bindings '?x)))

    (test-equal "variables-in simple" '(?x ?y) 
                (variables-in '(f ?x (g ?y ?x))))
    (test-equal "variables-in with no vars" '() 
                (variables-in '(a b (c))))

    (let ((expr-with-anon (replace-anonymous-variables '(a ? (?x ?)))))
      (test-assert "anonymous vars replaced" 
                   (not (member '? expr-with-anon :test #'eq))))
    
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

    (test-assert "occurs-check simple" (failure-p (unify '?x '(foo ?x) '())))
    (test-assert "occurs-check nested" 
                 (failure-p (unify '?y '(bar (baz ?y)) '()))))

  ;; -----------------------------------------------------------
  ;; API function coverage
  ;; -----------------------------------------------------------
  (test-group "exports"
    ;; object->string
    (test-equal "object->string pair" "(1 2)" (object->string '(1 2)))

    ;; remove-clauses-with-arity
    (let ((*current-clause-database* '()))
      (<- (r 1))
      (<- (r 1 2))
      (remove-clauses-with-arity 'r 1)
      (test-equal "arity removal" 1 (length (get-clauses 'r))))

    ;; prove-all and success structures
    (let* ((query '((= ?x 1) (member ?y (1 2))))
           (res (prove-all query '()))
           (query-vars (variables-in query))
           (relevant-bindings (remove-if-not (lambda (binding)
                                               (member (car binding) query-vars :test #'eq))
                                             (success-bindings res))))
      (test-assert "prove-all success" (success-p res))
      (test-equal "prove-all binding count" 2 
                  (length relevant-bindings)))

    ;; min-arity
    (test-equal "min-arity count proper list" 3 (min-arity '(a b c)))
    (test-equal "min-arity count improper list" 2 (min-arity '(a b . c))))

  ;; -----------------------------------------------------------
  ;; Basic queries
  ;; -----------------------------------------------------------
  (test-group "queries"
    ;; Clear database for clean tests
    (setf *current-clause-database* '())
    
    ;; Re-initialize basic predicates
    (<-- true)
    (<- (= ?x ?x))
    
    ;; Simple facts
    (<- (parent john mary))
    (<- (parent john tom))
    (<- (parent mary ann))

    (test-equal "simple fact query"
                '((mary . ?who))
                (solve-first '((parent john ?who)) '(?who . ?who)))

    (test-equal "all solutions query"
                '(mary tom)
                (solve-all '((parent john ?x)) '?x))

    (test-equal "no solutions query"
                '()
                (solve-first '((parent ann ?x)) '?x))

    ;; Rules
    (<- (grandparent ?gp ?gc) (parent ?gp ?p) (parent ?p ?gc))

    (test-equal "rule query"
                'ann
                (solve-first '((grandparent john ?gc)) '?gc))

    ;; List operations
    (test-equal "member query"
                '(1 2 3)
                (solve-all '((member ?x (1 2 3))) '?x 10))

    (test-equal "append query"
                '(() (1 2 3 4))
                (solve-first '((append ?x ?y (1 2 3 4))) '(?x ?y)))

    ;; Arithmetic
    (test-equal "is evaluation"
                6
                (solve-first '((is ?x (+ 1 2 3))) '?x))

    ;; Type checking
    (test-equal "atom check success"
                't
                (solve-first '((atom foo)) 't))
    (test-equal "atom check failure"
                '()
                (solve-first '((atom (foo bar))) 't))
    (test-equal "var check"
                't
                (solve-first '((var ?x)) 't))
    (test-equal "ground check"
                't
                (solve-first '((ground (foo bar))) 't)))

  ;; -----------------------------------------------------------
  ;; Control structures
  ;; -----------------------------------------------------------
  (test-group "control"
    ;; Conjunctions
    (test-equal "and success"
                't
                (solve-first '((and (= ?x 1) (= ?y 2))) 't))

    ;; Disjunctions
    (test-equal "or alternatives"
                '(1 2 3)
                (solve-all '((or (= ?x 1) (= ?x 2) (= ?x 3))) '?x))

    ;; Negation
    (test-assert "not success"
                 (solve-first '((not (= 1 2))) 't))
    (test-assert "not failure"
                 (null (solve-first '((not (= 1 1))) 't)))

    ;; If-then-else
    (test-equal "if-then branch"
                'then
                (solve-first '((if (= 1 1) (= ?x then) (= ?x else))) '?x))
    (test-equal "if-else branch"
                'else
                (solve-first '((if (= 1 2) (= ?x then) (= ?x else))) '?x)))

  ;; -----------------------------------------------------------
  ;; Meta-predicates
  ;; -----------------------------------------------------------
  (test-group "meta-predicates"
    ;; findall
    (test-equal "findall basic"
                '(1 2 3)
                (solve-first '((findall ?x (member ?x (1 2 3)) ?list)) '?list))

    ;; Simple bagof test
    (<- (likes mary food))
    (<- (likes mary wine))
    (<- (likes john wine))
    
    (test-equal "bagof simple"
                '(food wine)
                (solve-first '((bagof ?x (likes mary ?x) ?list)) '?list)))

  (test-end))

;;; Main entry point
(defun main ()
  (format t "~%Testing Athena Prolog Engine (Common Lisp)~%")
  (format t "==========================================~%")
  (run-tests)
  (if (= *fail-count* 0)
      (progn
        (format t "~%All tests passed!~%")
        (sb-ext:exit :code 0))
      (progn
        (format t "~%Some tests failed!~%")
        (sb-ext:exit :code 1))))

;;; Run tests when loaded
(when (member "--run-tests" sb-ext:*posix-argv* :test #'string=)
  (main))