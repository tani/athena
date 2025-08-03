;;; prolog-builtins.lisp — Built-in predicates for the Prolog engine
;;; Copyright © 2025 Masaya Taniguchi

(defpackage :prolog-builtins
  (:use :common-lisp :prolog-core)
  (:import-from :prolog-core
   :*current-bindings*
   :*current-remaining-goals*)
  (:export :initialize-builtins))

(in-package :prolog-builtins)

;;; Utility macro for defining predicates
(defmacro define-predicate ((name &rest arguments) &body body)
  `(set-clauses! ',name (lambda ,arguments ,@body)))

;;; Basic predicates

(define-predicate (= term1 term2)
  (let ((new-bindings (unify term1 term2 *current-bindings*)))
    (prove-all *current-remaining-goals* new-bindings)))

(define-predicate (== term1 term2)
  (let* ((substituted-term1 (substitute-bindings *current-bindings* term1))
         (substituted-term2 (substitute-bindings *current-bindings* term2)))
    (if (equal substituted-term1 substituted-term2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-predicate (! choice-point)
  (error (make-condition 'cut-exception
                         :tag choice-point
                         :value (prove-all *current-remaining-goals* *current-bindings*))))

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

;;; Arithmetic predicates

(define-predicate (is result expression)
  (let* ((substituted-expr (substitute-bindings *current-bindings* expression))
         (evaluated-result (handler-case
                               (eval-arithmetic substituted-expr)
                             (error () (return-from nil (make-failure)))))
         (new-bindings (unify result evaluated-result *current-bindings*)))
    (prove-all *current-remaining-goals* new-bindings)))

(defun eval-arithmetic (expr)
  (cond
    ((numberp expr) expr)
    ((variable-p expr) (error "Uninstantiated variable in arithmetic"))
    ((atom-p expr) (error "Invalid arithmetic expression"))
    ((consp expr)
     (let ((op (car expr))
           (args (mapcar #'eval-arithmetic (cdr expr))))
       (case op
         ((+) (apply #'+ args))
         ((-) (apply #'- args))
         ((*) (apply #'* args))
         ((/) (apply #'/ args))
         ((mod) (mod (first args) (second args)))
         ((abs) (abs (first args)))
         (otherwise (error "Unknown arithmetic operator ~S" op)))))
    (t (error "Invalid arithmetic expression"))))

;;; Control predicates

(define-predicate (fail)
  (make-failure))

;;; Initialize built-in predicates and rules
(defun initialize-builtins ()
  ;; Clear and set up basic facts
  (add-clause! '((true)))
  
  ;; Logical operators
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
  
  ;; List predicates
  (<- (member ?item (?item . ?)))
  (<- (member ?item (? . ?rest)) (member ?item ?rest))
  
  (<- (append () ?list ?list))
  (<- (append (?head . ?tail) ?list (?head . ?result))
      (append ?tail ?list ?result))
  
  ;; Repeat
  (<- (repeat))
  (<- (repeat) (repeat)))