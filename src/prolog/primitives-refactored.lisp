;;; prolog/primitives-refactored.lisp — Refactored built-in predicates with improved patterns
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file contains built-in Prolog predicates with improved:
;;; - Comprehensive documentation with examples
;;; - Consistent error handling and validation
;;; - Performance optimizations
;;; - Better code organization by category
;;; - Enhanced debugging support

(defpackage :prolog/primitives/refactored
  (:use :common-lisp :prolog/core/refactored)
  (:documentation "
Built-in Prolog predicates organized by category:

UNIFICATION PREDICATES:
  =/2     - Unification
  ==/2    - Strict equality (no unification)
  \\=/2    - Not unifiable
  \\==/2   - Not strictly equal

TYPE PREDICATES:
  var/1     - Test if term is unbound variable
  nonvar/1  - Test if term is not unbound variable
  atom/1    - Test if term is atom
  atomic/1  - Test if term is atomic
  compound/1 - Test if term is compound
  number/1  - Test if term is number
  integer/1 - Test if term is integer
  float/1   - Test if term is float
  ground/1  - Test if term is ground

CONTROL PREDICATES:
  true/0   - Always succeeds
  fail/0   - Always fails
  !/0      - Cut (commit to current choice)
  call/1   - Meta-call
  call/N   - Meta-call with additional arguments

ARITHMETIC PREDICATES:
  is/2     - Arithmetic evaluation
  >/2      - Greater than
  </2      - Less than
  >=/2     - Greater than or equal
  =</2     - Less than or equal
  =:=/2    - Arithmetic equality
  =\\=/2    - Arithmetic inequality

META-PREDICATES:
  findall/3 - Find all solutions
  bagof/3   - Collect solutions with duplicates
  setof/3   - Collect unique solutions
  once/1    - Succeed at most once
  ignore/1  - Ignore failure")
  
  (:export
   ;; === Predicate Definition Utilities ===
   :define-primitive-predicate      ; Define a primitive predicate
   :define-deterministic-predicate  ; Define a deterministic predicate
   :define-meta-predicate          ; Define a meta-predicate
   
   ;; === Unification Predicates ===
   :=    :==    :\\=    :\\==
   
   ;; === Type Testing Predicates ===
   :var :nonvar :atom :atomic :compound :number :integer :float :ground
   
   ;; === Control Predicates ===
   :true :fail :! :call :once :ignore
   
   ;; === Arithmetic Predicates ===
   :is :> :< :>= :=< :=:= :=\\=
   
   ;; === Meta-predicates ===
   :findall :bagof :setof
   
   ;; === Utility Functions ===
   :evaluate-arithmetic-expression  ; Evaluate arithmetic safely
   :check-instantiation            ; Check variable instantiation
   :type-error-unless              ; Type checking utility
   ))

(in-package :prolog/primitives/refactored)

;;; ============================================================================
;;; PREDICATE DEFINITION UTILITIES
;;; ============================================================================

(defvar *primitive-predicates* (make-hash-table :test 'equal)
  "Hash table storing metadata about primitive predicates.")

(defstruct predicate-info
  "Metadata about a primitive predicate."
  name                    ; Predicate name/arity
  implementation         ; Function implementing the predicate
  deterministic-p        ; True if predicate is deterministic
  documentation          ; Documentation string
  example               ; Usage example
  performance-notes)    ; Performance characteristics

(defmacro define-primitive-predicate ((name &rest args) &body options-and-body)
  "Define a primitive predicate with comprehensive metadata.
  
  Options:
    :documentation STRING   - Documentation for the predicate
    :example STRING        - Usage example
    :deterministic BOOLEAN - Whether predicate is deterministic
    :performance STRING    - Performance notes
  
  Example:
    (define-primitive-predicate (var ?term)
      :documentation \"Test if TERM is an unbound variable\"
      :example \"(var ?X) succeeds if ?X is unbound\"
      :deterministic t
      (if (variable-p (substitute-bindings *current-bindings* ?term))
          (prove-all *current-remaining-goals* *current-bindings*)
          (make-failure)))"
  
  (let* ((predicate-name/arity (format nil "~A/~D" name (length args)))
         (options '())
         (body '())
         (parsing-options t))
    
    ;; Separate options from body
    (dolist (item options-and-body)
      (if (and parsing-options (keywordp item))
          (progn
            (push item options)
            (push (car (cdr (member item options-and-body))) options))
          (progn
            (setf parsing-options nil)
            (push item body))))
    
    (let ((doc (getf options :documentation))
          (example (getf options :example))
          (deterministic (getf options :deterministic))
          (performance (getf options :performance)))
      
      `(progn
         ;; Store predicate metadata
         (setf (gethash ,predicate-name/arity *primitive-predicates*)
               (make-predicate-info
                 :name ,predicate-name/arity
                 :implementation ',name
                 :deterministic-p ,deterministic
                 :documentation ,doc
                 :example ,example
                 :performance-notes ,performance))
         
         ;; Define the actual predicate
         (set-clauses! ',name
                       (lambda ,args
                         ,@(when doc (list doc))
                         ,@body))
         
         ;; Export symbol if not already exported
         (export ',name)))))

(defmacro define-deterministic-predicate ((name &rest args) &body body)
  "Define a deterministic primitive predicate (shorthand)."
  `(define-primitive-predicate (,name ,@args)
     :deterministic t
     ,@body))

(defmacro define-meta-predicate ((name &rest args) &body body)
  "Define a meta-predicate that manipulates goals."
  `(define-primitive-predicate (,name ,@args)
     :deterministic nil
     ,@body))

;;; ============================================================================
;;; UTILITY FUNCTIONS FOR PREDICATES
;;; ============================================================================

(defun check-instantiation (term &optional (error-if-var t))
  "Check if TERM is sufficiently instantiated.
   If ERROR-IF-VAR is true, signals an error for unbound variables."
  (let ((resolved (substitute-bindings *current-bindings* term)))
    (when (and error-if-var (variable-p resolved))
      (error 'instantiation-error :variable resolved))
    (not (variable-p resolved))))

(defun type-error-unless (test term type-name)
  "Signal a type error unless TEST is true for TERM."
  (unless test
    (error 'type-error :expected-type type-name :datum term)))

(defun evaluate-arithmetic-expression (expr)
  "Safely evaluate an arithmetic expression, handling variables."
  (let ((resolved (substitute-bindings *current-bindings* expr)))
    (cond
      ((numberp resolved) resolved)
      ((variable-p resolved)
       (error 'instantiation-error :variable resolved))
      ((and (consp resolved) (member (car resolved) '(+ - * / mod abs)))
       (handler-case
           (eval resolved)
         (error (e)
           (error 'prolog-error :context (format nil "Arithmetic error: ~A" e)))))
      (t
       (error 'type-error :expected-type 'number :datum resolved)))))

;;; ============================================================================
;;; UNIFICATION PREDICATES
;;; ============================================================================

(define-deterministic-predicate (= term1 term2)
  :documentation "Unify TERM1 with TERM2"
  :example "(= ?X foo) binds ?X to foo"
  :performance "O(n) where n is term size"
  (let ((new-bindings (unify term1 term2 *current-bindings*)))
    (if (failure-p new-bindings)
        (make-failure)
        (prove-all *current-remaining-goals* new-bindings))))

(define-deterministic-predicate (== term1 term2)
  :documentation "Test strict equality (no unification) between TERM1 and TERM2"
  :example "(== foo foo) succeeds, (== ?X foo) fails if ?X unbound"
  :performance "O(n) where n is term size"
  (let* ((resolved1 (substitute-bindings *current-bindings* term1))
         (resolved2 (substitute-bindings *current-bindings* term2)))
    (if (terms-equal-p resolved1 resolved2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (\\= term1 term2)
  :documentation "Test that TERM1 and TERM2 cannot be unified"
  :example "(\\= foo bar) succeeds, (\\= ?X ?X) fails"
  (let ((unify-result (unify term1 term2 *current-bindings*)))
    (if (failure-p unify-result)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (\\== term1 term2)
  :documentation "Test that TERM1 and TERM2 are not strictly equal"
  :example "(\\== foo bar) succeeds, (\\== foo foo) fails"
  (let* ((resolved1 (substitute-bindings *current-bindings* term1))
         (resolved2 (substitute-bindings *current-bindings* term2)))
    (if (terms-equal-p resolved1 resolved2)
        (make-failure)
        (prove-all *current-remaining-goals* *current-bindings*))))

;;; ============================================================================
;;; TYPE TESTING PREDICATES
;;; ============================================================================

(define-deterministic-predicate (var term)
  :documentation "Test if TERM is an unbound variable"
  :example "(var ?X) succeeds if ?X is unbound"
  :performance "O(1)"
  (if (variable-p (substitute-bindings *current-bindings* term))
      (prove-all *current-remaining-goals* *current-bindings*)
      (make-failure)))

(define-deterministic-predicate (nonvar term)
  :documentation "Test if TERM is not an unbound variable"
  :example "(nonvar foo) succeeds, (nonvar ?X) fails if ?X unbound"
  :performance "O(1)"
  (if (variable-p (substitute-bindings *current-bindings* term))
      (make-failure)
      (prove-all *current-remaining-goals* *current-bindings*)))

(define-deterministic-predicate (atom term)
  :documentation "Test if TERM is an atom (symbol)"
  :example "(atom foo) succeeds, (atom 123) fails"
  :performance "O(1)"
  (let ((resolved (substitute-bindings *current-bindings* term)))
    (if (and (symbolp resolved) (not (variable-p resolved)))
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (atomic term)
  :documentation "Test if TERM is atomic (not compound)"
  :example "(atomic foo) and (atomic 123) succeed, (atomic (f a)) fails"
  :performance "O(1)"
  (let ((resolved (substitute-bindings *current-bindings* term)))
    (if (and (not (variable-p resolved)) (atom-p resolved))
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (compound term)
  :documentation "Test if TERM is a compound term"
  :example "(compound (f a)) succeeds, (compound foo) fails"
  :performance "O(1)"
  (let ((resolved (substitute-bindings *current-bindings* term)))
    (if (compound-term-p resolved)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (number term)
  :documentation "Test if TERM is a number"
  :example "(number 123) and (number 3.14) succeed"
  :performance "O(1)"
  (let ((resolved (substitute-bindings *current-bindings* term)))
    (if (numberp resolved)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (integer term)
  :documentation "Test if TERM is an integer"
  :example "(integer 123) succeeds, (integer 3.14) fails"
  :performance "O(1)"
  (let ((resolved (substitute-bindings *current-bindings* term)))
    (if (integerp resolved)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (float term)
  :documentation "Test if TERM is a float"
  :example "(float 3.14) succeeds, (float 123) fails"
  :performance "O(1)"
  (let ((resolved (substitute-bindings *current-bindings* term)))
    (if (floatp resolved)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (ground term)
  :documentation "Test if TERM is ground (contains no variables)"
  :example "(ground foo) succeeds, (ground (f ?X)) fails if ?X unbound"
  :performance "O(n) where n is term size"
  (if (ground-p term)
      (prove-all *current-remaining-goals* *current-bindings*)
      (make-failure)))

;;; ============================================================================
;;; CONTROL PREDICATES
;;; ============================================================================

(define-deterministic-predicate (true)
  :documentation "Always succeeds"
  :example "(true) always succeeds"
  :performance "O(1)"
  (prove-all *current-remaining-goals* *current-bindings*))

(define-deterministic-predicate (fail)
  :documentation "Always fails"
  :example "(fail) always fails"
  :performance "O(1)"
  (make-failure))

(define-primitive-predicate (! choice-point)
  :documentation "Cut - commit to current choice point"
  :example "(a, !, b) - once a succeeds, don't backtrack past the cut"
  :deterministic t
  (error (make-condition 'cut-exception
                         :tag choice-point
                         :value (prove-all *current-remaining-goals* *current-bindings*))))

(define-meta-predicate (call goal)
  :documentation "Meta-call - call GOAL as if it were written directly"
  :example "(call (parent john ?X)) is equivalent to (parent john ?X)"
  (call-with-current-choice-point
   (lambda (choice-point)
     (let* ((resolved-goal (substitute-bindings *current-bindings* goal))
            (goals-with-cut (insert-choice-point (list resolved-goal) choice-point)))
       (prove-all (append goals-with-cut *current-remaining-goals*) 
                  *current-bindings*)))))

(define-meta-predicate (once goal)
  :documentation "Succeed with GOAL at most once (commit after first success)"
  :example "(once (member ?X [a,b,c])) binds ?X to a only"
  (call-with-current-choice-point
   (lambda (choice-point)
     (let* ((resolved-goal (substitute-bindings *current-bindings* goal))
            (cut-goal (list '! choice-point))
            (goals-with-cut (list resolved-goal cut-goal)))
       (prove-all (append goals-with-cut *current-remaining-goals*)
                  *current-bindings*)))))

(define-meta-predicate (ignore goal)
  :documentation "Execute GOAL but always succeed (ignore failure)"
  :example "(ignore (fail)) succeeds"
  (handler-case
      (prove-all (list goal) *current-bindings*)
    (error () nil))
  (prove-all *current-remaining-goals* *current-bindings*))

;;; ============================================================================
;;; ARITHMETIC PREDICATES
;;; ============================================================================

(define-deterministic-predicate (is result expression)
  :documentation "Arithmetic evaluation - bind RESULT to value of EXPRESSION"
  :example "(is ?X (+ 2 3)) binds ?X to 5"
  :performance "O(n) where n is expression complexity"
  (let* ((evaluated-result (evaluate-arithmetic-expression expression))
         (result-term (substitute-bindings *current-bindings* result))
         (new-bindings (unify result-term evaluated-result *current-bindings*)))
    (if (failure-p new-bindings)
        (make-failure)
        (prove-all *current-remaining-goals* new-bindings))))

(define-deterministic-predicate (> expr1 expr2)
  :documentation "Arithmetic greater than"
  :example "(> 5 3) succeeds"
  :performance "O(n) where n is expression complexity"
  (let ((val1 (evaluate-arithmetic-expression expr1))
        (val2 (evaluate-arithmetic-expression expr2)))
    (if (> val1 val2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (< expr1 expr2)
  :documentation "Arithmetic less than"
  :example "(< 3 5) succeeds"
  :performance "O(n) where n is expression complexity"
  (let ((val1 (evaluate-arithmetic-expression expr1))
        (val2 (evaluate-arithmetic-expression expr2)))
    (if (< val1 val2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (>= expr1 expr2)
  :documentation "Arithmetic greater than or equal"
  :example "(>= 5 5) succeeds"
  :performance "O(n) where n is expression complexity"
  (let ((val1 (evaluate-arithmetic-expression expr1))
        (val2 (evaluate-arithmetic-expression expr2)))
    (if (>= val1 val2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (=< expr1 expr2)
  :documentation "Arithmetic less than or equal"
  :example "(=< 3 5) succeeds"
  :performance "O(n) where n is expression complexity"
  (let ((val1 (evaluate-arithmetic-expression expr1))
        (val2 (evaluate-arithmetic-expression expr2)))
    (if (<= val1 val2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (=:= expr1 expr2)
  :documentation "Arithmetic equality"
  :example "(=:= (+ 2 3) 5) succeeds"
  :performance "O(n) where n is expression complexity"
  (let ((val1 (evaluate-arithmetic-expression expr1))
        (val2 (evaluate-arithmetic-expression expr2)))
    (if (= val1 val2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

(define-deterministic-predicate (=\\= expr1 expr2)
  :documentation "Arithmetic inequality"
  :example "(=\\= 3 5) succeeds"
  :performance "O(n) where n is expression complexity"
  (let ((val1 (evaluate-arithmetic-expression expr1))
        (val2 (evaluate-arithmetic-expression expr2)))
    (if (/= val1 val2)
        (prove-all *current-remaining-goals* *current-bindings*)
        (make-failure))))

;;; ============================================================================
;;; META-PREDICATES (SOLUTION COLLECTION)
;;; ============================================================================

;; Note: These would need the solution collection infrastructure from the original
;; but show the improved documentation and structure pattern

(define-meta-predicate (findall template goal solutions)
  :documentation "Find all solutions to GOAL, collecting TEMPLATE instances"
  :example "(findall ?X (parent john ?X) ?Children) collects all John's children"
  :performance "O(n*m) where n is solutions, m is template complexity"
  ;; Implementation would go here using solution collection framework
  (error "findall/3 implementation requires solution collection framework"))

;;; ============================================================================
;;; INTROSPECTION AND DEBUGGING
;;; ============================================================================

(defun list-primitive-predicates ()
  "List all defined primitive predicates with their metadata."
  (let ((predicates '()))
    (maphash (lambda (name info)
               (push (list name 
                          (predicate-info-deterministic-p info)
                          (predicate-info-documentation info))
                     predicates))
             *primitive-predicates*)
    (sort predicates #'string< :key #'first)))

(defun predicate-help (predicate-name)
  "Display help for a primitive predicate."
  (let ((info (gethash predicate-name *primitive-predicates*)))
    (if info
        (format t "~%~A~%~A~%~@[Example: ~A~]~%~@[Performance: ~A~]~%"
                predicate-name
                (or (predicate-info-documentation info) "No documentation available")
                (predicate-info-example info)
                (predicate-info-performance-notes info))
        (format t "~%No help available for ~A~%" predicate-name))))

;;; Module completion
(eval-when (:load-toplevel :execute)
  (format t "~%Prolog Primitives (Refactored) loaded successfully.~%")
  (format t "Use (list-primitive-predicates) to see all available predicates.~%"))