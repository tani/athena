;;; prolog-core.lisp — Prolog engine core implementation in Common Lisp
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file contains the fundamental Prolog engine components:
;;; - Unification and variable binding
;;; - Clause database management
;;; - Proof engine and backtracking
;;; - Solution streams and query execution

(defpackage :prolog/core
  (:use :common-lisp)
  (:export
    ;; Main API
    :<-
    :<--
    :?-
    :run-query
    :solve

    ;; Variables and unification
    :variable-p
    :named-variable-p
    :atom-p
    :unify
    :substitute-bindings
    :variables-in
    :replace-anonymous-variables

    ;; Database operations
    :add-clause!
    :get-clauses
    :set-clauses!
    :remove-clauses-with-arity!

    ;; Prover internals (needed by builtins)
    :prove
    :prove-all
    :insert-choice-point
    :call-with-current-choice-point
    :combine
    :*current-bindings*
    :*current-remaining-goals*

    ;; Parameters (special variables)
    :*current-clause-database*
    :*current-spy-predicates*
    :*current-spy-mode*
    :*current-occurs-check*

    ;; Failure/success types
    :make-failure
    :failure-p
    :make-success
    :success-p
    :success-bindings
    :success-continuation

    ;; Exceptions
    :cut-exception
    :cut-exception-tag
    :cut-exception-value))

(in-package :prolog/core)

(defun symbol= (a b)
  (string= (symbol-name a) (symbol-name b)))

(defun terms-equal-p (term1 term2)
  "Compare two terms for equality, using symbol= for symbols and equal for other types"
  (cond
    ((and (symbolp term1) (symbolp term2))
      (symbol= term1 term2))
    (t (equal term1 term2))))

;;; Failure and success types
(defstruct failure)

(defstruct success
  bindings
  continuation)

;;; Cut exception for implementing Prolog's cut (!)
(define-condition cut-exception (error)
  ((tag :initarg :tag :reader cut-exception-tag)
    (value :initarg :value :reader cut-exception-value)))

;;; Special variables (replacing Scheme parameters)
(defparameter *current-clause-database* '())
(defparameter *current-spy-predicates* '())
(defparameter *current-spy-mode* 'prompt) ; 'prompt, 'always, or 'disabled
(defparameter *current-occurs-check* t)
(defparameter *current-bindings* '())
(defparameter *current-remaining-goals* '())

; Utility functions
(defun call-with-current-choice-point (proc)
  (let ((tag (gensym "CHOICE-POINT-")))
    (handler-case
      (funcall proc tag)
      (cut-exception (e)
        (if (eq tag (cut-exception-tag e))
          (cut-exception-value e)
          (error e))))))

;;; Variables and unification helpers
(defun variable-p (term)
  (and (symbolp term)
    (let ((symbol-string (symbol-name term)))
      (and (> (length symbol-string) 0)
        (char= (char symbol-string 0) #\?)))))

(defun named-variable-p (term)
  (and (variable-p term) (not (and (symbolp term) (symbol= term '?)))))

(defun atom-p (term)
  (not (consp term)))

(defun lookup-variable (variable bindings)
  (cdr (assoc variable bindings :test #'symbol=)))

(defun substitute-bindings (bindings expression &optional visited)
  (cond
    ((failure-p bindings) (make-failure))
    ((null bindings) expression)
    ((and (variable-p expression) (assoc expression bindings :test #'symbol=))
      (if (member expression visited :test #'symbol=)
        expression
        (let ((value (lookup-variable expression bindings)))
          (substitute-bindings bindings value (cons expression visited)))))
    ((atom-p expression) expression)
    (t
      (cons (substitute-bindings bindings (car expression) visited)
        (substitute-bindings bindings (cdr expression) visited)))))

(defun variables-in (expression)
  (labels ((collect-unique-if (predicate tree accumulator)
             (cond
               ((atom-p tree)
                 (if (and (funcall predicate tree)
                      (not (member tree accumulator :test #'symbol=)))
                   (cons tree accumulator)
                   accumulator))
               (t
                 (let ((accumulator-after-car
                         (collect-unique-if predicate (car tree) accumulator)))
                   (collect-unique-if predicate (cdr tree) accumulator-after-car))))))
    (reverse (collect-unique-if #'named-variable-p expression '()))))

(defun replace-anonymous-variables (expression)
  (cond
    ((and (symbolp expression) (symbol= expression '?)) (gensym "?"))
    ((atom-p expression) expression)
    (t (cons (replace-anonymous-variables (car expression))
        (replace-anonymous-variables (cdr expression))))))

;;; Occurs check for unification
(defun occurs-check-p (variable expression bindings)
  (cond
    ((and (symbolp expression) (symbol= variable expression)) t)
    ((and (variable-p expression) (assoc expression bindings :test #'symbol=))
      (let ((value (lookup-variable expression bindings)))
        (occurs-check-p variable value bindings)))
    ((consp expression)
      (or (occurs-check-p variable (car expression) bindings)
        (occurs-check-p variable (cdr expression) bindings)))
    (t nil)))

;;; Forward declarations to avoid warnings
(declaim (ftype function prove prove-all))

;;; Unification
(defun unify-var (variable value bindings)
  (cond
    ((assoc variable bindings :test #'symbol=)
      (let ((bound-term (lookup-variable variable bindings)))
        (unify bound-term value bindings)))
    ((and (variable-p value) (assoc value bindings :test #'symbol=))
      (let ((bound-term (lookup-variable value bindings)))
        (unify variable bound-term bindings)))
    ((and *current-occurs-check* (occurs-check-p variable value bindings))
      (make-failure))
    (t (acons variable value bindings))))

(defun unify (term1 term2 bindings)
  (cond
    ((failure-p bindings) (make-failure))
    ((terms-equal-p term1 term2) bindings)
    ((variable-p term1) (unify-var term1 term2 bindings))
    ((variable-p term2) (unify-var term2 term1 bindings))
    ((and (consp term1) (consp term2))
      (let ((car-bindings (unify (car term1) (car term2) bindings)))
        (unify (cdr term1) (cdr term2) car-bindings)))
    (t (make-failure))))

;;; Clause database operations
(defun get-clauses (predicate-symbol)
  (let ((entry (assoc predicate-symbol *current-clause-database* :test #'symbol=)))
    (if entry (cdr entry) '())))

(defun set-clauses! (predicate-symbol clauses)
  (let* ((cleaned-db (remove predicate-symbol *current-clause-database* :test #'symbol= :key #'car))
         (new-db (acons predicate-symbol clauses cleaned-db)))
    (setf *current-clause-database* new-db)))

(defun add-clause! (clause)
  (let* ((predicate-symbol (caar clause))
         (current-clauses (get-clauses predicate-symbol))
         (new-clauses (append current-clauses (list clause))))
    (set-clauses! predicate-symbol new-clauses)))

;;; Helper functions for clause management
(defun min-arity (args)
  (labels ((count-args (lst count)
             (cond
               ((consp lst) (count-args (cdr lst) (1+ count)))
               ((null lst) count)
               (t count))))
    (count-args args 0)))

(defun proper-list-p (lst)
  "Check if LST is a proper list (ends with NIL, not a dotted pair)"
  (cond
    ((null lst) t)
    ((atom lst) nil)
    ((consp lst) (proper-list-p (cdr lst)))
    (t nil)))

(defun remove-clauses-with-arity! (predicate-symbol arity)
  (labels ((has-different-arity-p (clause)
             (not (= (min-arity (cdar clause)) arity))))
    (let* ((current-clauses (get-clauses predicate-symbol))
           (new-clauses (remove-if-not #'has-different-arity-p current-clauses)))
      (set-clauses! predicate-symbol new-clauses))))

;;; Variable renaming for clauses
(defun sublis* (alist tree)
  (if (atom-p tree)
    (let ((binding (and (symbolp tree) (assoc tree alist :test #'symbol=))))
      (if binding (cdr binding) tree))
    (cons (sublis* alist (car tree))
      (sublis* alist (cdr tree)))))

(defun make-renaming-pair (variable)
  (let ((var-string (symbol-name variable)))
    (cons variable (gensym var-string))))

(defun rename-vars (expression)
  (let* ((variables (variables-in expression))
         (alist (mapcar #'make-renaming-pair variables)))
    (sublis* alist expression)))

;;; Macros for defining clauses
(defmacro <- (&rest clause-parts)
  (let ((clause (if (consp (car clause-parts))
                 clause-parts
                 (cons (list (car clause-parts)) (cdr clause-parts)))))
    `(add-clause! (replace-anonymous-variables ',clause))))

(defmacro <-- (&rest clause-parts)
  (let* ((head (car clause-parts))
         (body (cdr clause-parts))
         (name (if (consp head) (car head) head))
         (args (if (consp head) (cdr head) '()))
         (arity (min-arity args)))
    `(progn
      (remove-clauses-with-arity! ',name ,arity)
      (add-clause! (replace-anonymous-variables ',(cons head body))))))

;;; Query macro (simplified for now)
(defmacro ?- (&rest goals)
  `(run-query (replace-anonymous-variables ',goals)))

;;; Spy/debugging support
(defun spy-prompt (goal bindings)
  (let ((resolved (substitute-bindings bindings goal)))
    (format t "Spy on ~S? [l=leap c=creep n=nodebug] " resolved)
    (force-output *standard-output*)
    (case (read-char)
      ((#\l) (setf *current-spy-mode* 'always) t)
      ((#\c) t)
      ((#\n) (setf *current-spy-mode* 'disabled) nil)
      (otherwise t))))

(defun spy-message (kind goal bindings)
  (let ((resolved (substitute-bindings bindings goal)))
    (format t "~A: ~S~%" kind resolved)))

(defun with-spy (goal bindings thunk)
  (let* ((predicate-symbol (if (consp goal) (car goal) goal))
         (spy-p (member predicate-symbol *current-spy-predicates* :test #'symbol=))
         (mode *current-spy-mode*)
         (show-p (and spy-p
                  (case mode
                    ((always) t)
                    ((prompt) (spy-prompt goal bindings))
                    (otherwise nil))))
         (result nil))
    (let ((*current-spy-mode* (if show-p mode 'disabled)))
      (unwind-protect
        (progn
          (when show-p
            (spy-message "CALL" goal bindings))
          (setf result (funcall thunk))
          result)
        (when show-p
          (if (or (not result) (failure-p result))
            (spy-message "FAIL" goal bindings)
            (spy-message "EXIT" goal (success-bindings result))))))
    result))

;;; Prover engine
(defun prove-all (goals bindings)
  (cond
    ((failure-p bindings) (make-failure))
    ((null goals)
      (let ((terminal-cont (lambda () (make-failure))))
        (make-success :bindings bindings :continuation terminal-cont)))
    (t (prove (car goals) (cdr goals) bindings))))

(defun insert-choice-point (clause choice-point)
  (labels ((insert-cut-term (term)
             (cond
               ((and (consp term) (symbol= '! (car term))) (list '! choice-point))
               ((and (atom-p term) (symbol= '! term)) (list '! choice-point))
               (t term))))
    (mapcar #'insert-cut-term clause)))

(defun process-one (goal clause bindings remaining-goals)
  (let* ((renamed-clause (rename-vars clause))
         (clause-head (car renamed-clause))
         (clause-body (cdr renamed-clause))
         (new-bindings (unify goal clause-head bindings)))
    (if (failure-p new-bindings)
      (make-failure)
      (prove-all (append clause-body remaining-goals) new-bindings))))

(defun combine (continuation-a continuation-b)
  (lambda ()
    (let ((result-a (funcall continuation-a)))
      (if (or (not result-a) (failure-p result-a))
        (funcall continuation-b)
        (let* ((bindings (success-bindings result-a))
               (result-continuation (success-continuation result-a))
               (new-continuation (combine result-continuation continuation-b)))
          (make-success :bindings bindings :continuation new-continuation))))))

(defun try-clauses (goal bindings remaining-goals all-clauses)
  (call-with-current-choice-point
    (lambda (choice-point)
      (let ((goal-arity (if (consp goal) (length (cdr goal)) 0)))
        (labels ((clause-match-p (clause)
                   (let* ((required (min-arity (cdar clause)))
                          (variadic-p (not (proper-list-p (cdar clause)))))
                     (and (>= goal-arity required)
                       (or variadic-p (= goal-arity required)))))
                 (try-one-by-one (clauses-to-try)
                   (if (null clauses-to-try)
                     (make-failure)
                     (let* ((current-clause (insert-choice-point (car clauses-to-try) choice-point))
                            (remaining-clauses (cdr clauses-to-try))
                            (try-next-clause (lambda () (try-one-by-one remaining-clauses)))
                            (result (process-one goal current-clause bindings remaining-goals)))
                       (if (failure-p result)
                         (funcall try-next-clause)
                         (let* ((result-bindings (success-bindings result))
                                (result-continuation (success-continuation result))
                                (new-continuation (combine result-continuation try-next-clause)))
                           (make-success :bindings result-bindings
                             :continuation
                             new-continuation)))))))
          (let ((clauses (remove-if-not #'clause-match-p all-clauses)))
            (try-one-by-one clauses)))))))

(defun prove (goal remaining-goals bindings)
  (with-spy goal bindings
    (lambda ()
      (let* ((*current-remaining-goals* remaining-goals)
             (*current-bindings* bindings)
             (predicate-symbol (if (consp goal) (car goal) goal))
             (predicate-handler (get-clauses predicate-symbol))
             (args (if (consp goal) (cdr goal) '()))
             (goal-arity (length args)))
        (if (functionp predicate-handler)
          (apply predicate-handler args)
          (let ((goal-for-unify (if (consp goal) goal (list goal))))
            (if (and (not (null predicate-handler))
                 (< goal-arity (apply #'min (mapcar (lambda (c) (min-arity (cdar c)))
                                             predicate-handler))))
              (make-failure)
              (try-clauses goal-for-unify bindings remaining-goals predicate-handler))))))))

(defun solve (goals on-success on-failure)
  (labels ((initial-continuation ()
             (call-with-current-choice-point
               (lambda (choice-point)
                 (let* ((prepared-goals (replace-anonymous-variables goals))
                        (cut-goals (insert-choice-point prepared-goals choice-point)))
                   (prove-all cut-goals '())))))
           (retrieve-success-bindings (result)
             (let* ((bindings (success-bindings result))
                    (query-variables (variables-in goals)))
               (mapcar (lambda (v) (cons v (substitute-bindings bindings v)))
                 query-variables)))
           (execute-success-continuation (result)
             (funcall (success-continuation result))))
    (do ((result (initial-continuation) (execute-success-continuation result)))
      ((not (success-p result)) (funcall on-failure))
      (funcall on-success (retrieve-success-bindings result)))))

(defun display-solution (bindings)
  (if (null bindings)
    (progn
      (terpri)
      (format t "Yes"))
    (dolist (var-val bindings)
      (terpri)
      (format t "~A = ~A" (car var-val) (cdr var-val)))))

(defun continue-prompt-p ()
  (terpri)
  (format t "Continue ? (y/n) ")
  (force-output *standard-output*)
  (case (read-char)
    ((#\y) t)
    ((#\n) nil)
    (otherwise
      (format t " Type y for more, or n to stop.~%")
      (continue-prompt-p))))

(defun run-query (goals)
  (block query-exit
    (solve goals
      (lambda (solution)
        (display-solution solution)
        (unless (continue-prompt-p)
          (return-from query-exit)))
      (lambda ()
        (format t "No.~%")
        (return-from query-exit)))))
