;;; prolog-lib.lisp — Prolog standard library and built-in predicates
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file contains the standard Prolog predicates and library functions:
;;; - Basic unification and comparison predicates
;;; - Arithmetic and type checking predicates
;;; - Meta-predicates
;;; - Control flow predicates (and, or, not, if)
;;; - List manipulation predicates (member, append, maplist)

(defpackage :prolog/primitive
  (:use :common-lisp :prolog/core)
  (:export
    ;; Utility functions
    :object->string
    :ground-p

    ;; Special variables
    :*current-dynamic-parameters*

    ;; Helper functions for tests
    :define-predicate))

(in-package :prolog/primitive)

;;; Special variables replacing Scheme parameters
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

;; Enhanced cut operation with continuation value handling
(define-predicate (! choice-point)
  (let ((continuation-result (prove-all *current-remaining-goals* *current-bindings*)))
    (error (make-condition 'cut-exception
            :tag
            choice-point
            :value
            continuation-result))))

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
