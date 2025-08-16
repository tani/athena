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

;;; Predicate definition utility
(defmacro define-predicate ((name &rest arguments) &body body)
  "Define a predicate implemented as a Common Lisp function."
  `(set-clauses! ',name (lambda ,arguments ,@body)))

;;; Core Built-in Predicates (Phase 2)

;; Unification predicates
(define-predicate (= term1 term2)
  "Unification predicate: TERM1 = TERM2.
  Attempts to unify TERM1 and TERM2, updating the binding environment.
  Succeeds if unification is possible, fails otherwise."
  (let ((new-bindings (unify term1 term2 *current-bindings*)))
    (if (failure-p new-bindings)
        (make-failure)
        (prove-goal-sequence *current-remaining-goals* new-bindings))))

(define-predicate (== term1 term2)
  "Strict equality predicate: TERM1 == TERM2.
  Tests if TERM1 and TERM2 are identical after variable substitution.
  Does not perform unification, only tests existing equality."
  (let* ((substituted-term1 (substitute-bindings *current-bindings* term1))
         (substituted-term2 (substitute-bindings *current-bindings* term2)))
    (if (equal substituted-term1 substituted-term2)
        (prove-goal-sequence *current-remaining-goals* *current-bindings*)
        (make-failure))))

;; Control predicates
(define-predicate (! choice-point)
  "Cut predicate: !.
  Commits to the current choice, preventing backtracking to alternative clauses
  for the current goal. CHOICE-POINT identifies the choice point to cut."
  (let ((continuation-result (prove-goal-sequence *current-remaining-goals* *current-bindings*)))
    (error (make-condition 'cut-exception :tag choice-point :value continuation-result))))

(define-predicate (call pred &rest args)
  "Meta-call predicate: call(PRED, ARGS...).
  Dynamically calls PRED with additional ARGS appended.
  PRED can be an atom or a compound term."
  (call-with-current-choice-point
    (lambda (choice-point)
      (let* ((substituted-pred (substitute-bindings *current-bindings* pred))
             (substituted-args (substitute-bindings *current-bindings* args))
             (goal (cond
                     ((null substituted-args) substituted-pred)
                     ((symbolp substituted-pred) (cons substituted-pred substituted-args))
                     ((consp substituted-pred) (append substituted-pred substituted-args))
                     (t (error "call: Invalid form ~S" (cons substituted-pred substituted-args)))))
             (cut-goals (insert-choice-point (list goal) choice-point))
             (next-goals (append cut-goals *current-remaining-goals*)))
        (prove-goal-sequence next-goals *current-bindings*)))))

(define-predicate (var term)
  "Type predicate: var(TERM).
  Succeeds if TERM is an unbound variable."
  (if (variable-p (substitute-bindings *current-bindings* term))
      (prove-goal-sequence *current-remaining-goals* *current-bindings*)
      (make-failure)))

;; Type checking predicates
(define-predicate (atom term)
  (let ((value (substitute-bindings *current-bindings* term)))
    (if (and (symbolp value) (not (variable-p value)))
        (prove-goal-sequence *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-predicate (atomic term)
  (let ((value (substitute-bindings *current-bindings* term)))
    (if (and (not (variable-p value)) (not (consp value)))
        (prove-goal-sequence *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-predicate (ground term)
  (if (ground-p term)
      (prove-goal-sequence *current-remaining-goals* *current-bindings*)
      (make-failure)))

(define-predicate (number term)
  (let ((value (substitute-bindings *current-bindings* term)))
    (if (numberp value)
        (prove-goal-sequence *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-predicate (string term)
  (let ((value (substitute-bindings *current-bindings* term)))
    (if (stringp value)
        (prove-goal-sequence *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-predicate (fail)
  (make-failure))

;; Logical control predicates
(define-predicate (and &rest goals)
  (call-with-current-choice-point
    (lambda (choice-point)
      (let* ((cut-goals (insert-choice-point goals choice-point))
             (next-goals (append cut-goals *current-remaining-goals*)))
        (prove-goal-sequence next-goals *current-bindings*)))))

(define-predicate (or-2 goal1 goal2)
  (let* ((goals-1 (cons `(call ,goal1) *current-remaining-goals*))
         (try-goals-1 (lambda () (prove-goal-sequence goals-1 *current-bindings*)))
         (goals-2 (cons `(call ,goal2) *current-remaining-goals*))
         (try-goals-2 (lambda () (prove-goal-sequence goals-2 *current-bindings*)))
         (result-1 (funcall try-goals-1)))
    (if (failure-p result-1)
        (funcall try-goals-2)
        (let* ((success-bindings-1 (success-bindings result-1))
               (success-continuation-1 (success-continuation result-1))
               (new-continuation (merge-continuations success-continuation-1 try-goals-2)))
          (make-success :bindings success-bindings-1 :continuation new-continuation)))))

(define-predicate (or &rest goals)
  (let* ((or-2-goal (reduce (lambda (expr acc) `(or-2 ,expr ,acc)) 
                             goals 
                             :initial-value 'false
                             :from-end t))
         (next-goals (cons or-2-goal *current-remaining-goals*)))
    (prove-goal-sequence next-goals *current-bindings*)))

;;; Lisp Integration Predicates

;; Helper function for Lisp evaluation
(defun eval-lisp-expressions (expressions &optional result-handler)
  "Evaluate EXPRESSIONS as Lisp code for Prolog integration.
  RESULT-HANDLER processes the evaluation result if provided."
  (if (not (ground-p expressions))
      (make-failure)
      (let* ((lisp-expression (substitute-bindings *current-bindings* `(progn ,@expressions)))
             (evaluated-result (eval lisp-expression)))
        (if result-handler
            (funcall result-handler evaluated-result)
            (prove-goal-sequence *current-remaining-goals* *current-bindings*)))))

;; Lisp integration predicates
(define-predicate (lisp result-variable &rest expressions)
  "Evaluate EXPRESSIONS as Lisp and unify result with RESULT-VARIABLE."
  (eval-lisp-expressions
   expressions
   (lambda (evaluated-result)
     (let* ((result-term (substitute-bindings *current-bindings* result-variable))
            (new-bindings (unify result-term evaluated-result *current-bindings*)))
       (if (failure-p new-bindings)
           (make-failure)
           (prove-goal-sequence *current-remaining-goals* new-bindings))))))

(define-predicate (lisp! &rest expressions)
  "Evaluate EXPRESSIONS as Lisp for side effects, always succeeds."
  (eval-lisp-expressions expressions))

(define-predicate (lispp &rest expressions)
  "Evaluate EXPRESSIONS as Lisp and succeed if result is non-nil."
  (eval-lisp-expressions
   expressions
   (lambda (evaluated-result)
     (if (not evaluated-result)
         (make-failure)
         (prove-goal-sequence *current-remaining-goals* *current-bindings*)))))

;; Backward compatibility
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
        (prove-goal-sequence *current-remaining-goals* new-bindings)
        (make-failure))))

;; Dynamic parameter predicates
(define-predicate (dynamic-put variable-symbol value-expression)
  "Dynamic parameter predicate: dynamic-put(SYMBOL, VALUE).
  Stores VALUE under SYMBOL in the dynamic parameter store.
  VALUE-EXPRESSION is evaluated as Lisp code before storing."
  (let* ((substituted-expression (substitute-bindings *current-bindings* value-expression))
         (evaluated-value (eval substituted-expression))
         (*current-dynamic-parameters* 
          (acons variable-symbol evaluated-value *current-dynamic-parameters*)))
    (prove-goal-sequence *current-remaining-goals* *current-bindings*)))

(define-predicate (dynamic-get variable-symbol prolog-variable)
  "Dynamic parameter predicate: dynamic-get(SYMBOL, VAR).
  Retrieves the value associated with SYMBOL and unifies it with VAR."
  (let ((key-value (assoc variable-symbol *current-dynamic-parameters*)))
    (if (null key-value)
        (make-failure)
        (let ((new-bindings (unify prolog-variable (cdr key-value) *current-bindings*)))
          (if (failure-p new-bindings)
              (make-failure)
              (prove-goal-sequence *current-remaining-goals* new-bindings))))))
