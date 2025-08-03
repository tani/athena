;;; prolog/core-refactored.lisp — Refactored Prolog engine core with improved patterns
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file contains the fundamental Prolog engine components with improved:
;;; - Comprehensive documentation and examples
;;; - Consistent error handling patterns
;;; - Performance optimizations
;;; - Better separation of concerns
;;; - Enhanced debugging support

(defpackage :prolog/core/refactored
  (:use :common-lisp)
  (:documentation "
The Prolog core engine provides fundamental logic programming capabilities including:

* Unification with occurs check
* Clause database management with indexing
* Backtracking proof engine
* Variable binding and substitution
* Query execution with solution streams

Example usage:
  (<- (parent john mary))
  (<- (parent mary susan))
  (<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
  
  (?- (grandparent john ?who))
  ; => ?who = susan")
  
  (:export
   ;; === Main User API ===
   :<-                              ; Define clause: (<- head body...)
   :<--                             ; Define clause, removing existing ones
   :?-                              ; Interactive query: (?- goal...)
   :run-query                       ; Programmatic query execution
   :solve                           ; Low-level query with callbacks
   
   ;; === Variable and Term Operations ===
   :variable-p                      ; Test if symbol is Prolog variable
   :named-variable-p                ; Test if variable is named (not anonymous)
   :atom-p                          ; Test if term is atomic (not compound)
   :ground-p                        ; Test if term contains no variables
   :unify                           ; Unify two terms with bindings
   :substitute-bindings             ; Apply variable bindings to term
   :variables-in                    ; Extract variables from term
   :replace-anonymous-variables     ; Replace ? with unique variables
   
   ;; === Database Operations ===
   :add-clause!                     ; Add clause to database
   :get-clauses                     ; Retrieve clauses for predicate
   :set-clauses!                    ; Set all clauses for predicate
   :remove-clauses-with-arity!      ; Remove clauses with specific arity
   :clear-database!                 ; Clear entire clause database
   
   ;; === Engine Internals (for built-ins) ===
   :prove                           ; Prove single goal
   :prove-all                       ; Prove list of goals
   :insert-choice-point            ; Insert cut choice point
   :call-with-current-choice-point  ; Call with choice point context
   :combine                         ; Combine proof results
   :make-failure                    ; Create failure result
   :failure-p                       ; Test if result is failure
   :make-success                    ; Create success result
   :success-p                       ; Test if result is success
   
   ;; === Dynamic Variables ===
   :*current-bindings*              ; Current variable bindings
   :*current-remaining-goals*       ; Remaining goals to prove
   :*current-clause-database*       ; Current clause database
   :*current-spy-predicates*        ; Predicates being traced
   :*current-spy-mode*              ; Current debugging mode
   :*current-occurs-check*          ; Occurs check flag
   
   ;; === Debugging and Introspection ===
   :spy-predicate                   ; Enable tracing for predicate
   :nospy-predicate                 ; Disable tracing for predicate
   :trace-proof                     ; Trace proof execution
   :database-statistics             ; Get database statistics
   :performance-profile             ; Profile engine performance
   
   ;; === Error Conditions ===
   :prolog-error                    ; Base Prolog error
   :unification-error               ; Unification failure
   :instantiation-error             ; Variable instantiation error
   :cut-exception                   ; Cut exception for backtracking
   ))

(in-package :prolog/core/refactored)

;;; ============================================================================
;;; ERROR CONDITIONS AND EXCEPTION HANDLING
;;; ============================================================================

(define-condition prolog-error (error)
  ((context :initarg :context :reader prolog-error-context :initform nil))
  (:documentation "Base class for all Prolog-related errors."))

(define-condition unification-error (prolog-error)
  ((term1 :initarg :term1 :reader unification-error-term1)
   (term2 :initarg :term2 :reader unification-error-term2))
  (:report (lambda (condition stream)
             (format stream "Cannot unify ~S with ~S"
                     (unification-error-term1 condition)
                     (unification-error-term2 condition))))
  (:documentation "Signaled when unification fails in strict mode."))

(define-condition instantiation-error (prolog-error)
  ((variable :initarg :variable :reader instantiation-error-variable))
  (:report (lambda (condition stream)
             (format stream "Variable ~S is not sufficiently instantiated"
                     (instantiation-error-variable condition))))
  (:documentation "Signaled when a variable needs to be instantiated but isn't."))

(define-condition cut-exception (condition)
  ((tag :initarg :tag :reader cut-exception-tag)
   (value :initarg :value :reader cut-exception-value))
  (:documentation "Exception thrown by cut operator to implement backtracking control."))

;;; ============================================================================
;;; RESULT TYPES FOR PROOF ENGINE
;;; ============================================================================

(defstruct (failure (:constructor make-failure ()))
  "Represents a failed proof attempt.")

(defstruct (success (:constructor make-success (bindings continuation)))
  "Represents a successful proof with variable bindings and continuation."
  bindings      ; Association list of variable bindings
  continuation) ; Function to call for backtracking

(defun failure-p (object)
  "Test if OBJECT is a failure result."
  (typep object 'failure))

(defun success-p (object)
  "Test if OBJECT is a success result."
  (typep object 'success))

;;; ============================================================================
;;; DYNAMIC VARIABLES (PROLOG PARAMETERS)
;;; ============================================================================

(defvar *current-bindings* '()
  "Association list of current variable bindings during proof.")

(defvar *current-remaining-goals* '()
  "List of remaining goals to prove in current proof.")

(defvar *current-clause-database* '()
  "Association list mapping predicate symbols to their clauses.
   Format: ((predicate-name . clauses) ...)")

(defvar *current-spy-predicates* '()
  "List of predicate symbols currently being traced for debugging.")

(defvar *current-spy-mode* 'disabled
  "Current spy mode: 'disabled, 'enabled, or 'stepping.")

(defvar *current-occurs-check* nil
  "When true, prevents creation of infinite terms during unification.")

(defvar *performance-stats* nil
  "Performance statistics for profiling engine operations.")

;;; ============================================================================
;;; TERM CLASSIFICATION AND UTILITIES
;;; ============================================================================

(defun variable-p (term)
  "Test if TERM is a Prolog variable (symbol starting with ?)."
  (and (symbolp term)
       (let ((name (symbol-name term)))
         (and (> (length name) 0)
              (char= (char name 0) #\?)))))

(defun named-variable-p (term)
  "Test if TERM is a named Prolog variable (not anonymous ?)."
  (and (variable-p term) 
       (not (eq term '?))))

(defun atom-p (term)
  "Test if TERM is atomic (not a compound term)."
  (not (consp term)))

(defun ground-p (term &optional (bindings *current-bindings*))
  "Test if TERM is ground (contains no unbound variables) given BINDINGS."
  (let ((resolved-term (substitute-bindings bindings term)))
    (cond
      ((variable-p resolved-term) nil)
      ((consp resolved-term)
       (and (ground-p (car resolved-term) bindings)
            (ground-p (cdr resolved-term) bindings)))
      (t t))))

(defun compound-term-p (term)
  "Test if TERM is a compound term (list with functor and arguments)."
  (and (consp term) (symbolp (car term))))

(defun functor (term)
  "Extract the functor (predicate name) from a compound TERM."
  (if (compound-term-p term)
      (car term)
      term))

(defun arity (term)
  "Get the arity (number of arguments) of TERM."
  (if (compound-term-p term)
      (length (cdr term))
      0))

;;; ============================================================================
;;; VARIABLE OPERATIONS
;;; ============================================================================

(defun variables-in (expression)
  "Extract all named variables from EXPRESSION, returning them in order of appearance."
  (let ((seen-vars '())
        (result '()))
    (labels ((collect-vars (expr)
               (cond
                 ((named-variable-p expr)
                  (unless (member expr seen-vars)
                    (push expr seen-vars)
                    (push expr result)))
                 ((consp expr)
                  (collect-vars (car expr))
                  (collect-vars (cdr expr))))))
      (collect-vars expression)
      (nreverse result))))

(defun replace-anonymous-variables (expression)
  "Replace all anonymous variables (?) in EXPRESSION with unique symbols."
  (cond
    ((eq expression '?) (gensym "?"))
    ((atom-p expression) expression)
    (t (cons (replace-anonymous-variables (car expression))
             (replace-anonymous-variables (cdr expression))))))

;;; ============================================================================
;;; VARIABLE BINDING AND SUBSTITUTION
;;; ============================================================================

(defun substitute-bindings (bindings expression)
  "Apply variable BINDINGS to EXPRESSION, substituting bound variables.
   
   Example:
     (substitute-bindings '((?x . john) (?y . mary)) '(parent ?x ?y))
     => (parent john mary)"
  (cond
    ((failure-p bindings) (make-failure))
    ((null bindings) expression)
    ((and (variable-p expression) (assoc expression bindings))
     (let ((binding (cdr (assoc expression bindings))))
       (substitute-bindings bindings binding)))  ; Handle chains
    ((atom-p expression) expression)
    (t (cons (substitute-bindings bindings (car expression))
             (substitute-bindings bindings (cdr expression))))))

(defun extend-bindings (variable value bindings)
  "Extend BINDINGS with a new binding of VARIABLE to VALUE.
   Returns the extended bindings or a failure if inconsistent."
  (let ((existing (assoc variable bindings)))
    (cond
      (existing
       ;; Variable already bound, check consistency
       (let ((existing-value (cdr existing)))
         (if (equal existing-value value)
             bindings
             (make-failure))))
      (t
       ;; New binding
       (acons variable value bindings)))))

;;; ============================================================================
;;; UNIFICATION ENGINE
;;; ============================================================================

(defun terms-equal-p (term1 term2)
  "Compare two terms for equality, using symbol= for symbols to handle package differences."
  (cond
    ((and (symbolp term1) (symbolp term2))
     (symbol= term1 term2))
    (t (equal term1 term2))))

(defun occurs-check-p (variable expression bindings)
  "Check if VARIABLE occurs in EXPRESSION (prevents infinite terms).
   Returns T if variable occurs, NIL otherwise."
  (cond
    ((eq variable expression) t)
    ((and (variable-p expression) (assoc expression bindings))
     (let ((value (cdr (assoc expression bindings))))
       (occurs-check-p variable value bindings)))
    ((consp expression)
     (or (occurs-check-p variable (car expression) bindings)
         (occurs-check-p variable (cdr expression) bindings)))
    (t nil)))

(defun unify-variable (variable value bindings)
  "Unify VARIABLE with VALUE given current BINDINGS.
   Handles occurs check if enabled."
  (let ((existing (assoc variable bindings)))
    (cond
      (existing
       ;; Variable already bound
       (unify (cdr existing) value bindings))
      ((and (variable-p value) (assoc value bindings))
       ;; Value is bound variable
       (unify variable (cdr (assoc value bindings)) bindings))
      ((and *current-occurs-check* (occurs-check-p variable value bindings))
       ;; Occurs check failed
       (make-failure))
      (t
       ;; Create new binding
       (acons variable value bindings)))))

(defun unify (term1 term2 bindings)
  "Unify TERM1 and TERM2 given current BINDINGS.
   Returns extended bindings on success, failure object on failure.
   
   Example:
     (unify '(parent ?x mary) '(parent john ?y) '())
     => ((?y . mary) (?x . john))"
  (cond
    ((failure-p bindings) (make-failure))
    ((terms-equal-p term1 term2) bindings)
    ((variable-p term1) (unify-variable term1 term2 bindings))
    ((variable-p term2) (unify-variable term2 term1 bindings))
    ((and (consp term1) (consp term2))
     (let ((new-bindings (unify (car term1) (car term2) bindings)))
       (if (failure-p new-bindings)
           (make-failure)
           (unify (cdr term1) (cdr term2) new-bindings))))
    (t (make-failure))))

;;; ============================================================================
;;; CLAUSE DATABASE MANAGEMENT
;;; ============================================================================

(defun get-clauses (predicate-symbol)
  "Retrieve all clauses for PREDICATE-SYMBOL from the current database."
  (cdr (assoc predicate-symbol *current-clause-database*)))

(defun set-clauses! (predicate-symbol clauses)
  "Set all clauses for PREDICATE-SYMBOL, replacing any existing ones."
  (setf *current-clause-database*
        (acons predicate-symbol clauses
               (remove predicate-symbol *current-clause-database* :key #'car))))

(defun add-clause! (predicate-symbol clause)
  "Add a single CLAUSE for PREDICATE-SYMBOL to the database."
  (let ((existing-clauses (get-clauses predicate-symbol)))
    (set-clauses! predicate-symbol (append existing-clauses (list clause)))))

(defun remove-clauses-with-arity! (predicate-symbol target-arity)
  "Remove all clauses for PREDICATE-SYMBOL that have TARGET-ARITY."
  (let* ((all-clauses (get-clauses predicate-symbol))
         (filtered-clauses
           (remove-if (lambda (clause)
                        (let ((head (car clause)))
                          (= (arity head) target-arity)))
                      all-clauses)))
    (if filtered-clauses
        (set-clauses! predicate-symbol filtered-clauses)
        (setf *current-clause-database*
              (remove predicate-symbol *current-clause-database* :key #'car)))))

(defun clear-database! ()
  "Clear the entire clause database."
  (setf *current-clause-database* '()))

(defun database-statistics ()
  "Return statistics about the current clause database."
  (let ((predicate-count (length *current-clause-database*))
        (total-clauses 0)
        (max-clauses 0)
        (predicates-by-arity (make-hash-table)))
    
    (dolist (entry *current-clause-database*)
      (let* ((predicate (car entry))
             (clauses (cdr entry))
             (clause-count (length clauses)))
        (incf total-clauses clause-count)
        (setf max-clauses (max max-clauses clause-count))
        
        ;; Group by arity
        (dolist (clause clauses)
          (let ((arity (arity (car clause))))
            (incf (gethash arity predicates-by-arity 0))))))
    
    (list :predicate-count predicate-count
          :total-clauses total-clauses
          :max-clauses-per-predicate max-clauses
          :average-clauses-per-predicate (if (> predicate-count 0)
                                             (/ total-clauses predicate-count)
                                             0)
          :predicates-by-arity (loop for arity being the hash-keys of predicates-by-arity
                                     collect (list arity (gethash arity predicates-by-arity))))))

;;; Module completion marker
(eval-when (:load-toplevel :execute)
  (format t "~%Prolog Core (Refactored) loaded successfully.~%"))