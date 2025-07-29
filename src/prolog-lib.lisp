;;; prolog-lib.lisp — Prolog standard library and built-in predicates
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file contains the standard Prolog predicates and library functions:
;;; - Basic unification and comparison predicates
;;; - Arithmetic and type checking predicates
;;; - Meta-predicates (bagof, findall, sort)
;;; - Control flow predicates (and, or, not, if)
;;; - List manipulation predicates (member, append, maplist)

(defpackage :prolog-lib
  (:use :cl :prolog-core)
  (:export
   ;; Re-export core symbols
   #:variable-p #:named-variable-p #:atom-p
   #:variables-in #:replace-anonymous-variables
   #:lookup-variable #:substitute-bindings #:unify
   #:*current-occurs-check*
   #:failure #:make-failure #:failure-p
   #:success #:make-success #:success-p
   #:success-bindings #:success-continuation
   #:cut-exception #:make-cut-exception #:cut-exception-p
   #:cut-exception-tag #:cut-exception-value
   #:*current-clause-database* #:get-clauses #:set-clauses
   #:add-clause #:remove-clauses-with-arity #:min-arity
   #:*current-bindings* #:*current-remaining-goals*
   #:prove #:prove-all #:call-with-current-choice-point
   #:*current-spy-mode* #:*current-spy-predicates*
   #:*current-spy-depth* #:*current-spy-indent-p*
   #:make-solution-stream #:run-query #:display-solution
   #:<- #:<-- #:?- #:define-predicate
   #:*current-dynamic-parameters* #:get-dynamic-parameter
   #:ground-p #:*current-lisp-environment*
   ;; Library specific
   #:*current-solution-accumulator*))

(in-package :prolog-lib)

(defparameter *current-solution-accumulator* '())

;;; Basic predicates

(define-predicate (= term1 term2)
  (let ((new-bindings (unify term1 term2 *current-bindings*)))
    (prove-all *current-remaining-goals* new-bindings)))

(define-predicate (== term1 term2)
  (let* ((substituted-term1 (substitute-bindings *current-bindings* term1))
         (substituted-term2 (substitute-bindings *current-bindings* term2)))
    (if (equal substituted-term1 substituted-term2)
        (let ((goals *current-remaining-goals*))
          (prove-all goals *current-bindings*))
        (make-failure))))

(define-predicate (! choice-point)
  (error (make-cut-exception choice-point
                             (prove-all *current-remaining-goals* *current-bindings*))))

(define-predicate (call pred-or-goal &rest args)
  (call-with-current-choice-point
   (lambda (choice-point)
     (let* ((goal (cond
                    ((null args) pred-or-goal)
                    ((symbolp pred-or-goal) (cons pred-or-goal args))
                    ((consp pred-or-goal) (append pred-or-goal args))
                    (t (error "call: invalid form ~S" (cons pred-or-goal args)))))
            (substituted-goal (substitute-bindings *current-bindings* goal))
            (cut-goals (insert-choice-point (list substituted-goal) choice-point))
            (next-goals (append cut-goals *current-remaining-goals*)))
       (prove-all next-goals *current-bindings*)))))

;;; Evaluation predicates

(define-predicate (--lisp-eval-internal result-variable expression)
  (let* ((scheme-expression (substitute-bindings *current-bindings* expression))
         (evaluated-result (eval scheme-expression))
         (result-term (substitute-bindings *current-bindings* result-variable))
         (new-bindings (unify result-term evaluated-result *current-bindings*)))
    (prove-all *current-remaining-goals* new-bindings)))

;;; Type checking predicates

(define-predicate (atom term)
  (let ((value (substitute-bindings *current-bindings* term)))
    (if (and (symbolp value) (not (variable-p value)))
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-predicate (atomic term)
  (let ((value (substitute-bindings *current-bindings* term)))
    (if (and (not (variable-p value)) (not (consp value)))
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-predicate (var term)
  (if (variable-p (substitute-bindings *current-bindings* term))
      (prove-all *current-remaining-goals* *current-bindings*)
      (make-failure)))

(define-predicate (ground term)
  (if (ground-p term)
      (prove-all *current-remaining-goals* *current-bindings*)
      (make-failure)))

(define-predicate (number term)
  (let ((value (substitute-bindings *current-bindings* term)))
    (if (numberp value)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

;;; Dynamic parameter predicates

(define-predicate (dynamic-put variable-symbol value-expression)
  (let* ((parameter (get-dynamic-parameter variable-symbol))
         (substituted-expression (substitute-bindings *current-bindings* value-expression))
         (evaluated-value (eval substituted-expression)))
    (setf (cdr parameter) evaluated-value)
    (prove-all *current-remaining-goals* *current-bindings*)))

(define-predicate (dynamic-get variable-symbol prolog-variable)
  (let* ((parameter (get-dynamic-parameter variable-symbol))
         (new-bindings (unify prolog-variable (cdr parameter) *current-bindings*)))
    (prove-all *current-remaining-goals* new-bindings)))

;;; Meta-predicates for solution collection

(define-predicate (--add-solution-with-vars-and-fail template &rest vars)
  (let* ((substituted-template (substitute-bindings *current-bindings* template))
         (substituted-vars (mapcar (lambda (v) (substitute-bindings *current-bindings* v)) vars))
         (entry (cons substituted-vars substituted-template))
         (new-solutions (cons entry *current-solution-accumulator*)))
    (setf *current-solution-accumulator* new-solutions))
  (make-failure))

(define-predicate (bagof template goal result-bag)
  (labels ((group-solutions (pairs)
             (let ((acc '()))
               (dolist (current-pair pairs (reverse acc))
                 (let* ((key (car current-pair))
                        (template (cdr current-pair))
                        (existing-group (assoc key acc :test #'equal)))
                   (if existing-group
                       (let* ((updated-group (cons key (append (cdr existing-group) (list template))))
                              (acc-without-old (remove key acc :key #'car :test #'equal)))
                         (setf acc (cons updated-group acc-without-old)))
                       (setf acc (acons key (list template) acc)))))))
           (enumerate-groups (grouped-solutions free-vars result-bag)
             (if (null grouped-solutions)
                 (make-failure)
                 (let* ((current-group (car grouped-solutions))
                        (free-var-values (car current-group))
                        (bag-of-templates (cdr current-group))
                        (next-group-thunk
                          (lambda ()
                            (enumerate-groups (cdr grouped-solutions) free-vars result-bag)))
                        (b1 (unify free-vars free-var-values *current-bindings*))
                        (b2 (unify result-bag bag-of-templates b1)))
                   (if (failure-p b2)
                       (funcall next-group-thunk)
                       (let* ((proof-stream (prove-all *current-remaining-goals* b2))
                              (continuation (success-continuation proof-stream))
                              (new-continuation (combine continuation next-group-thunk)))
                         (make-success :bindings (success-bindings proof-stream)
                                       :continuation new-continuation)))))))
    (let* ((substituted-goal (substitute-bindings *current-bindings* goal))
           (goal-vars (variables-in substituted-goal))
           (template-vars (variables-in (substitute-bindings *current-bindings* template)))
           (free-vars (remove-if (lambda (v) (member v template-vars :test #'eq)) goal-vars))
           (collector-goal `(--add-solution-with-vars-and-fail ,template ,@free-vars))
           (all-solutions
             (let ((*current-solution-accumulator* '()))
               (prove-all `(,substituted-goal ,collector-goal fail) *current-bindings*)
               (reverse *current-solution-accumulator*)))
           (grouped-solutions (group-solutions all-solutions)))
      (enumerate-groups grouped-solutions free-vars result-bag))))

(define-predicate (sort unsorted-list result-list)
  (labels ((object< (a b)
             (let ((sa (write-to-string a))
                   (sb (write-to-string b)))
               (string< sa sb))))
    (let* ((actual-list (substitute-bindings *current-bindings* unsorted-list))
           (unique-list (remove-duplicates actual-list :test #'equal))
           (sorted-list (sort (copy-list unique-list) #'object<))
           (new-bindings (unify result-list sorted-list *current-bindings*))
           (goals *current-remaining-goals*))
      (prove-all goals new-bindings))))

(define-predicate (--add-solution-and-fail template)
  (let* ((substituted-template (substitute-bindings *current-bindings* template))
         (new-solutions (cons substituted-template *current-solution-accumulator*)))
    (setf *current-solution-accumulator* new-solutions))
  (make-failure))

(define-predicate (findall template goal result-list)
  (let ((*current-solution-accumulator* '()))
    (let ((new-goals (list goal (list '--add-solution-and-fail template) 'fail)))
      (prove-all new-goals *current-bindings*))
    (let* ((reversed-solutions (reverse *current-solution-accumulator*))
           (new-bindings (unify result-list reversed-solutions *current-bindings*))
           (goals *current-remaining-goals*))
      (prove-all goals new-bindings))))

(define-predicate (fail)
  (make-failure))

;;; Standard clause database initialization and basic predicates

(defun initialize-standard-library ()
  "Initialize the standard Prolog library predicates"
  ;; Basic unification
  (<-- (= ?x ?x))
  
  ;; Truth
  (<-- true)

  (<- and true)
  (<- (and ?g . ?gs) (call ?g) (call (and . ?gs)))

  (<- or fail)
  (<- (or ?g . ?gs) (call ?g))
  (<- (or ?g . ?gs) (call (or . ?gs)))

  (<- (not ?goal) (call ?goal) ! (fail))
  (<- (not ?goal))

  (<- (if ?cond ?then ?else) (call ?cond) ! (call ?then))
  (<- (if ?cond ?then ?else) (call ?else))
  (<- (if ?cond ?then) (call ?cond) (call ?then))

  (<- (lisp ?result ?expression) (--lisp-eval-internal ?result ?expression))
  (<- (lisp ?expression) (--lisp-eval-internal ?_ ?expression))

  (<- (is ?result ?expression) (--lisp-eval-internal ?result ?expression))

  (<- (repeat))
  (<- (repeat) (repeat))

  (<- (member ?item (?item . ?_)))
  (<- (member ?item (?_ . ?rest)) (member ?item ?rest))

  (<- (append () ?list ?list))
  (<- (append (?head . ?tail) ?list (?head . ?result))
      (append ?tail ?list ?result))

  (<- (--all-null ()))
  (<- (--all-null (() . ?rest)) (--all-null ?rest))

  (<- (--get-heads () ()))
  (<- (--get-heads ((?h . ?t) . ?rest-lists) (?h . ?rest-heads))
      (--get-heads ?rest-lists ?rest-heads))

  (<- (--get-tails () ()))
  (<- (--get-tails ((?h . ?t) . ?rest-lists) (?t . ?rest-tails))
      (--get-tails ?rest-lists ?rest-tails))

  (<- (maplist ?pred . ?lists)
      (if (--all-null ?lists)
          true
          (and
           (--get-heads ?lists ?heads)
           (--get-tails ?lists ?tails)
           (call (?pred . ?heads))
           (call (maplist ?pred . ?tails)))))

  (<-- (setof ?template ?goal ?result-set)
       (bagof ?template ?goal ?result-bag)
       (sort ?result-bag ?result-set)))

;; Initialize the library when loaded
(initialize-standard-library)