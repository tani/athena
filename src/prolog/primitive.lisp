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

(defpackage :prolog/primitive
  (:use :common-lisp :prolog/core)
  (:export
   ;; Utility functions
   :object->string
   :ground-p

   ;; Special variables
   :*current-solution-accumulator*
   :*current-dynamic-parameters*

   ;; Helper functions for tests
   :define-predicate
   
   ;; Built-in predicates used by stdlib
   :call :true :! :fail))

(in-package :prolog/primitive)

;;; Special variables replacing Scheme parameters
(defparameter *current-solution-accumulator* '())
(defparameter *current-dynamic-parameters* '())

;;; Utility functions
(defun object->string (object)
  "Convert a Lisp object to its string representation."
  (with-output-to-string (stream)
    (write object :stream stream)))

(defun ground-p (term)
  "Check if a term is fully ground (contains no unbound variables)."
  (let ((resolved-term (substitute-bindings *current-bindings* term)))
    (cond
      ((variable-p resolved-term) nil)
      ((consp resolved-term)
       (and (ground-p (car resolved-term))
            (ground-p (cdr resolved-term))))
      (t t))))

;;; Predicate definition utility
(defmacro define-predicate ((name &rest arguments) &body body)
  "Define a predicate implemented as a Common Lisp function."
  `(set-clauses! ',name (lambda ,arguments ,@body)))

;;; Core Built-in Predicates (Phase 2)

;; Enhanced unification predicates
(define-predicate (= term1 term2)
  (let ((new-bindings (unify term1 term2 *current-bindings*)))
    (prove-all *current-remaining-goals* new-bindings)))

(define-predicate (== term1 term2)
  (let* ((substituted-term1 (substitute-bindings *current-bindings* term1))
         (substituted-term2 (substitute-bindings *current-bindings* term2)))
    (if (equal substituted-term1 substituted-term2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

;; Cut operation with proper exception handling
(define-predicate (! choice-point)
  (error (make-condition 'cut-exception
                         :tag choice-point
                         :value (prove-all *current-remaining-goals* *current-bindings*))))

;; Meta-call predicate with variadic support
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

;; Type checking predicates
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

(define-predicate (string term)
  (let ((value (substitute-bindings *current-bindings* term)))
    (if (stringp value)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

;; Control predicates
(define-predicate (fail)
  (make-failure))

;;; Evaluation System (Phase 3)

;; Lisp evaluation predicate
(define-predicate (--lisp-eval-internal result-variable expression)
  (let* ((lisp-expression (substitute-bindings *current-bindings* expression))
         (evaluated-result (handler-case
                               (eval lisp-expression)
                             (error (e)
                               (format t "Evaluation error: ~A~%" e)
                               nil)))
         (result-term (substitute-bindings *current-bindings* result-variable))
         (new-bindings (unify result-term evaluated-result *current-bindings*)))
    (if new-bindings
        (prove-all *current-remaining-goals* new-bindings)
        (make-failure))))

;; Dynamic parameter predicates
(define-predicate (dynamic-put variable-symbol value-expression)
  (let* ((substituted-expression (substitute-bindings *current-bindings* value-expression))
         (evaluated-value (eval substituted-expression))
         (*current-dynamic-parameters* (acons variable-symbol evaluated-value *current-dynamic-parameters*)))
    (prove-all *current-remaining-goals* *current-bindings*)))

(define-predicate (dynamic-get variable-symbol prolog-variable)
  (let* ((key-value (assoc variable-symbol *current-dynamic-parameters*))
         (value (if key-value (cdr key-value) nil))
         (new-bindings (unify prolog-variable value *current-bindings*)))
    (prove-all *current-remaining-goals* new-bindings)))

;;; Meta-predicates (Phase 4)

;; Solution collection helpers
(define-predicate (--add-solution-with-vars-and-fail template &rest vars)
  (let* ((substituted-template (substitute-bindings *current-bindings* template))
         (substituted-vars (mapcar (lambda (v) (substitute-bindings *current-bindings* v)) vars))
         (entry (cons substituted-vars substituted-template))
         (new-solutions (cons entry *current-solution-accumulator*)))
    (setf *current-solution-accumulator* new-solutions))
  (make-failure))

(define-predicate (--add-solution-and-fail template)
  (let* ((substituted-template (substitute-bindings *current-bindings* template))
         (new-solutions (cons substituted-template *current-solution-accumulator*)))
    (setf *current-solution-accumulator* new-solutions))
  (make-failure))

;; findall - collect all solutions
(define-predicate (findall template goal result-list)
  (let ((*current-solution-accumulator* '()))
    (let ((new-goals (list goal (list '--add-solution-and-fail template) 'fail)))
      (prove-all new-goals *current-bindings*))
    (let* ((reversed-solutions (reverse *current-solution-accumulator*))
           (new-bindings (unify result-list reversed-solutions *current-bindings*)))
      (prove-all *current-remaining-goals* new-bindings))))

;; sort - sort and deduplicate list
(define-predicate (sort unsorted-list result-list)
  (let* ((actual-list (substitute-bindings *current-bindings* unsorted-list))
         (unique-list (remove-duplicates actual-list :test #'equal))
         (sorted-list (sort (copy-list unique-list)
                            (lambda (a b)
                              (string< (object->string a) (object->string b)))))
         (new-bindings (unify result-list sorted-list *current-bindings*)))
    (prove-all *current-remaining-goals* new-bindings)))

;; bagof - simplified implementation for basic functionality
(define-predicate (bagof template goal result-bag)
  ;; bagof should fail when no solutions exist (unlike findall which returns empty list)
  (let ((*current-solution-accumulator* '()))
    (let ((new-goals (list goal (list '--add-solution-and-fail template) 'fail)))
      (prove-all new-goals *current-bindings*))
    (let ((reversed-solutions (reverse *current-solution-accumulator*)))
      ;; bagof fails if no solutions were found
      (if (null reversed-solutions)
          (make-failure)
          ;; If solutions exist, unify with result-bag
          (let ((new-bindings (unify result-bag reversed-solutions *current-bindings*)))
            (prove-all *current-remaining-goals* new-bindings))))))

;; setof - bagof + sort
(define-predicate (setof template goal result-set)
  (prove-all `((bagof ,template ,goal ?result-bag) (sort ?result-bag ,result-set)) *current-bindings*))

;; Aliases for Lisp evaluation
(define-predicate (is result-variable expression)
  (prove-all `((--lisp-eval-internal ,result-variable ,expression)) *current-bindings*))

(define-predicate (lisp result-variable expression)
  (prove-all `((--lisp-eval-internal ,result-variable ,expression)) *current-bindings*))

;; Simple true predicate for testing
(define-predicate (true)
  (prove-all *current-remaining-goals* *current-bindings*))
