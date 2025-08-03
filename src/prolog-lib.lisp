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
  (:use :common-lisp :prolog-core)
  (:import-from :prolog-core
   :*current-bindings*
   :*current-remaining-goals*
   :*current-clause-database*)
  (:export
   ;; Main initialization
   :initialize-standard-library
   
   ;; Utility functions
   :object->string
   :ground-p
   
   ;; Dynamic parameter functions
   :get-dynamic-parameter
   :set-dynamic-parameter  
   :get-dynamic-parameter-value
   
   ;; Special variables
   :*current-solution-accumulator*
   :*current-dynamic-parameters*
   
   ;; Helper functions for tests
   :define-predicate
   
   ;; Built-in predicates (will be available via the clause database)
   ;; Type checking: atom, atomic, var, ground, number, string
   ;; Logic: and, or, not, if, fail, true, repeat
   ;; Meta: call, bagof, findall, setof, sort
   ;; List: member, append, maplist
   ;; Evaluation: is, lisp
   ;; Dynamic: dynamic-put, dynamic-get
   ))

(in-package :prolog-lib)

;;; Special variables replacing Scheme parameters
(defparameter *current-solution-accumulator* '())
(defparameter *current-lisp-environment* nil) ; Will use current package
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

;;; Dynamic parameter management
(defun get-dynamic-parameter (variable-symbol)
  "Get or create a dynamic parameter for the given symbol."
  (let ((entry (assoc variable-symbol *current-dynamic-parameters* :test #'eq)))
    (if entry
        (cdr entry)
        (let* ((new-parameter (list nil)) ; Use list for mutable container
               (new-entry (cons variable-symbol new-parameter)))
          (push new-entry *current-dynamic-parameters*)
          new-parameter))))

(defun set-dynamic-parameter (variable-symbol value)
  "Set the value of a dynamic parameter."
  (let ((parameter (get-dynamic-parameter variable-symbol)))
    (setf (car parameter) value)))

(defun get-dynamic-parameter-value (variable-symbol)
  "Get the current value of a dynamic parameter."
  (let ((parameter (get-dynamic-parameter variable-symbol)))
    (car parameter)))

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
         (evaluated-value (handler-case
                              (eval substituted-expression)
                            (error (e)
                              (format t "Dynamic evaluation error: ~A~%" e)
                              nil))))
    (set-dynamic-parameter variable-symbol evaluated-value)
    (prove-all *current-remaining-goals* *current-bindings*)))

(define-predicate (dynamic-get variable-symbol prolog-variable)
  (let* ((value (get-dynamic-parameter-value variable-symbol))
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
  ;; For now, implement bagof as findall (simplified version)
  ;; This provides the basic functionality without the complex grouping logic
  (let ((*current-solution-accumulator* '()))
    (let ((new-goals (list goal (list '--add-solution-and-fail template) 'fail)))
      (prove-all new-goals *current-bindings*))
    (let* ((reversed-solutions (reverse *current-solution-accumulator*))
           (new-bindings (unify result-bag reversed-solutions *current-bindings*)))
      (prove-all *current-remaining-goals* new-bindings))))

;; setof - bagof + sort
(define-predicate (setof template goal result-set)
  (prove-all `((bagof ,template ,goal ?result-bag) (sort ?result-bag ,result-set)) *current-bindings*))

;;; Standard Logic Predicates (Phase 5)

;; Note: Control flow predicates (true, repeat, and, or, not, if) are implemented 
;; as clauses in the initialization function rather than as function-based predicates

;; All list manipulation predicates and helpers are implemented as clauses in initialization
;; to avoid conflicts with function-based implementations

;;; Initialization function - Phase 5
(defun initialize-standard-library ()
  "Initialize all standard library predicates and clauses."
  ;; Basic fact
  (<- true)
  
  ;; Control flow clauses
  (<- (and) true)
  (<- (and ?goal) (call ?goal))
  (<- (and ?goal . ?goals) (call ?goal) (call (and . ?goals)))
  
  (<- (or ?goal) (call ?goal))
  (<- (or ?goal . ?goals) (call ?goal))
  (<- (or ?goal . ?goals) (call (or . ?goals)))
  
  (<- (not ?goal) (call ?goal) ! (fail))
  (<- (not ?goal))
  
  (<- (if ?cond ?then ?else) (call ?cond) ! (call ?then))
  (<- (if ?cond ?then ?else) (call ?else))
  (<- (if ?cond ?then) (call ?cond) (call ?then))
  
  ;; Repeat predicate
  (<- (repeat))
  (<- (repeat) (repeat))
  
  ;; List manipulation clauses
  (<- (member ?item (?item . ?|_|)))
  (<- (member ?item (?|_| . ?rest)) (member ?item ?rest))
  
  (<- (append () ?list ?list))
  (<- (append (?head . ?tail) ?list (?head . ?result))
      (append ?tail ?list ?result))
  
  ;; Maplist helper clauses
  (<- (--all-null ()))
  (<- (--all-null (() . ?rest)) (--all-null ?rest))
  
  (<- (--get-heads () ()))
  (<- (--get-heads ((?head . ?|_|) . ?rest) (?head . ?heads))
      (--get-heads ?rest ?heads))
      
  (<- (--get-tails () ()))
  (<- (--get-tails ((?|_| . ?tail) . ?rest) (?tail . ?tails))
      (--get-tails ?rest ?tails))
  
  (<- (maplist ?pred . ?lists)
      (if (--all-null ?lists)
          true
          (and (--get-heads ?lists ?heads)
               (--get-tails ?lists ?tails)
               (call (?pred . ?heads))
               (call (maplist ?pred . ?tails)))))
  
  ;; Add clause-based predicates for evaluation
  (<- (lisp ?result ?expression) (--lisp-eval-internal ?result ?expression))
  (<- (lisp ?expression) (--lisp-eval-internal |?| ?expression))
  (<- (is ?result ?expression) (--lisp-eval-internal ?result ?expression))
  
  (format t "Prolog standard library Phase 5 initialized.~%"))