;;; prolog-core.lisp — Prolog engine core implementation in Common Lisp
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file contains the fundamental Prolog engine components:
;;; - Unification and variable binding
;;; - Clause database management
;;; - Proof engine and backtracking
;;; - Solution streams and query execution

(defpackage :prolog-core
  (:use :cl)
  (:export
   ;; Variable handling
   #:variable-p #:named-variable-p #:atom-p
   #:variables-in #:replace-anonymous-variables
   ;; Bindings and unification
   #:lookup-variable #:substitute-bindings #:unify
   #:*current-occurs-check*
   ;; Failure/success structures
   #:failure #:make-failure #:failure-p
   #:success #:make-success #:success-p
   #:success-bindings #:success-continuation
   ;; Cut exception
   #:cut-exception #:make-cut-exception #:cut-exception-p
   #:cut-exception-tag #:cut-exception-value
   ;; Clause database
   #:*current-clause-database* #:get-clauses #:set-clauses
   #:add-clause #:remove-clauses-with-arity #:min-arity
   ;; Proof engine
   #:*current-bindings* #:*current-remaining-goals*
   #:prove #:prove-all #:call-with-current-choice-point
   #:insert-choice-point #:combine
   ;; Spy/debug support
   #:*current-spy-mode* #:*current-spy-predicates*
   #:*current-spy-depth* #:*current-spy-indent-p*
   ;; Solution streams
   #:make-solution-stream #:run-query #:display-solution
   ;; Macros
   #:<- #:<-- #:?- #:define-predicate
   ;; Dynamic parameters
   #:*current-dynamic-parameters* #:get-dynamic-parameter
   ;; Utilities
   #:ground-p #:*current-lisp-environment*
   #:object->string))

(in-package :prolog-core)

;;; Utility functions

(defun object->string (object)
  "Convert any object to its string representation"
  (with-output-to-string (stream)
    (write object :stream stream)))

;;; Failure and success structures

(defstruct failure)

(defstruct success
  bindings
  continuation)

;;; Cut exception

(define-condition cut-exception (error)
  ((tag :initarg :tag :reader cut-exception-tag)
   (value :initarg :value :reader cut-exception-value)))

(defun make-cut-exception (tag value)
  (make-condition 'cut-exception :tag tag :value value))

(defun cut-exception-p (obj)
  (typep obj 'cut-exception))

;;; Forward declarations for mutually recursive functions
(declaim (ftype function unify prove-all try-clauses make-solution-stream))

;;; Gensym with counter
(let ((counter 0))
  (defun %gensym (prefix)
    (let* ((counter-string (write-to-string counter))
           (name (concatenate 'string prefix counter-string)))
      (incf counter)
      (intern name))))

;;; Choice point handling
(defun call-with-current-choice-point (proc)
  (let ((tag (%gensym "CHOICE-POINT-")))
    (handler-case
        (funcall proc tag)
      (cut-exception (exception)
        (if (eq tag (cut-exception-tag exception))
            (cut-exception-value exception)
            (error exception))))))

;;; Bindings and unification

(defun variable-p (term)
  (and (symbolp term)
       (let ((symbol-string (symbol-name term)))
         (and (> (length symbol-string) 0)
              (char= (char symbol-string 0) #\?)))))

(defun named-variable-p (term)
  (and (variable-p term) (not (equal (symbol-name term) "?"))))

(defun atom-p (term)
  (not (consp term)))

(defun lookup-variable (variable bindings)
  (cdr (assoc variable bindings :test #'eq)))

(defun substitute-bindings (bindings expression &optional (visited '()))
  (cond
    ((failure-p bindings) (make-failure))
    ((null bindings) expression)
    ((and (variable-p expression) (assoc expression bindings :test #'eq))
     (if (member expression visited :test #'eq)
         expression
         (let ((value (lookup-variable expression bindings)))
           (substitute-bindings bindings value (cons expression visited)))))
    ((atom-p expression) expression)
    (t
     (let* ((substituted-car (substitute-bindings bindings (car expression) visited))
            (substituted-cdr (substitute-bindings bindings (cdr expression) visited)))
       (cons substituted-car substituted-cdr)))))

(defun variables-in (expression)
  (labels ((collect-unique-if (predicate tree accumulator)
             (if (atom-p tree)
                 (if (and (funcall predicate tree)
                          (not (member tree accumulator :test #'eq)))
                     (cons tree accumulator)
                     accumulator)
                 (let ((accumulator-after-car
                         (collect-unique-if predicate (car tree) accumulator)))
                   (collect-unique-if predicate (cdr tree) accumulator-after-car)))))
    (reverse (collect-unique-if #'named-variable-p expression '()))))

(defun replace-anonymous-variables (expression)
  (cond
    ((and (symbolp expression) (equal (symbol-name expression) "?")) 
     (%gensym "?"))
    ((atom-p expression) expression)
    (t
     (let* ((car-replaced (replace-anonymous-variables (car expression)))
            (cdr-replaced (replace-anonymous-variables (cdr expression))))
       (cons car-replaced cdr-replaced)))))

(defparameter *current-occurs-check* t)

(defun occurs-check-p (variable expression bindings)
  (cond
    ((eq variable expression) t)
    ((and (variable-p expression) (assoc expression bindings :test #'eq))
     (let ((value (lookup-variable expression bindings)))
       (occurs-check-p variable value bindings)))
    ((consp expression)
     (or (occurs-check-p variable (car expression) bindings)
         (occurs-check-p variable (cdr expression) bindings)))
    (t nil)))

(defun unify-var (variable value bindings)
  (cond
    ((assoc variable bindings :test #'eq)
     (let ((bound-term (lookup-variable variable bindings)))
       (unify bound-term value bindings)))
    ((and (variable-p value) (assoc value bindings :test #'eq))
     (let ((bound-term (lookup-variable value bindings)))
       (unify variable bound-term bindings)))
    ((and *current-occurs-check* (occurs-check-p variable value bindings))
     (make-failure))
    (t (acons variable value bindings))))

(defun unify (term1 term2 bindings)
  (cond
    ((failure-p bindings) (make-failure))
    ((equal term1 term2) bindings)
    ((variable-p term1) (unify-var term1 term2 bindings))
    ((variable-p term2) (unify-var term2 term1 bindings))
    ((and (consp term1) (consp term2))
     (let ((car-bindings (unify (car term1) (car term2) bindings)))
       (unify (cdr term1) (cdr term2) car-bindings)))
    (t (make-failure))))

;;; Clause database

(defparameter *current-clause-database* '())

(defun get-clauses (predicate-symbol)
  (let ((entry (assoc predicate-symbol *current-clause-database* :test #'eq)))
    (if entry (cdr entry) '())))

(defun set-clauses (predicate-symbol clauses)
  (let* ((cleaned-db (remove predicate-symbol *current-clause-database*
                             :key #'car :test #'eq))
         (new-db (acons predicate-symbol clauses cleaned-db)))
    (setf *current-clause-database* new-db)))

(defun add-clause (clause)
  (let* ((predicate-symbol (caar clause))
         (current-clauses (get-clauses predicate-symbol))
         (new-clauses (append current-clauses (list clause))))
    (set-clauses predicate-symbol new-clauses)))

(defun min-arity (args)
  (loop for lst on args
        counting (consp lst)))

(defmacro <- (head &body body)
  (let ((processed-head (if (consp head) head (list head))))
    `(add-clause (replace-anonymous-variables '(,processed-head ,@body)))))

(defun remove-clauses-with-arity (predicate-symbol arity)
  (let ((current-clauses (get-clauses predicate-symbol)))
    (cond
      ((functionp current-clauses)
       ;; It's a defined predicate function, remove it entirely
       (set-clauses predicate-symbol '()))
      ((listp current-clauses)
       ;; It's a list of clauses, filter by arity
       (labels ((has-different-arity-p (clause)
                  (not (= (min-arity (cdar clause)) arity))))
         (let ((new-clauses (remove-if-not #'has-different-arity-p current-clauses)))
           (set-clauses predicate-symbol new-clauses))))
      (t
       ;; Unknown type, clear it
       (set-clauses predicate-symbol '())))))

(defmacro <-- (head &body body)
  (let* ((processed-head (if (consp head) head (list head)))
         (name (car processed-head))
         (args (cdr processed-head))
         (arity (length args)))
    `(progn
       (remove-clauses-with-arity ',name ,arity)
       (add-clause (replace-anonymous-variables '(,processed-head ,@body))))))

;;; Prover engine

(defparameter *current-bindings* '())
(defparameter *current-remaining-goals* '())

(defun sublis* (alist tree)
  (if (atom-p tree)
      (let ((binding (assoc tree alist :test #'eq)))
        (if binding (cdr binding) tree))
      (cons (sublis* alist (car tree))
            (sublis* alist (cdr tree)))))

(defun make-renaming-pair (variable)
  (let ((var-string (symbol-name variable)))
    (cons variable (%gensym var-string))))

(defun rename-vars (expression)
  (let* ((variables (variables-in expression))
         (alist (mapcar #'make-renaming-pair variables)))
    (sublis* alist expression)))

;;; Spy support

(defparameter *current-spy-mode* 'prompt)  ; 'prompt, 'always, or 'disabled
(defparameter *current-spy-predicates* '())
(defparameter *current-spy-depth* 0)
(defparameter *current-spy-indent-p* t)
(defparameter *current-lisp-environment* nil)

(defun simple-repl ()
  (format t "Entering break; type 'continue to resume~%")
  (loop
    (format t "debug> ")
    (finish-output)
    (let ((form (read)))
      (when (eq form 'continue)
        (return))
      (let ((result (eval form)))
        (write result)
        (terpri)))))

(defun spy-prompt (goal bindings)
  (let ((resolved (substitute-bindings bindings goal)))
    (format t "Spy on ~S? [l=leap c=creep n=nodebug b=break] " resolved)
    (finish-output)
    (case (read)
      ((l) (setf *current-spy-mode* 'always) t)
      ((c) t)
      ((n) (setf *current-spy-mode* 'disabled) nil)
      ((b) (simple-repl) t)
      (otherwise t))))

(defun spy-message (kind goal bindings)
  (let ((resolved (substitute-bindings bindings goal)))
    (when *current-spy-indent-p*
      (dotimes (i *current-spy-depth*)
        (write-char #\Space)))
    (format t "~A: ~S~%" kind resolved)))

(defun with-spy (goal bindings thunk)
  (let* ((predicate-symbol (if (consp goal) (car goal) goal))
         (spy-p (member predicate-symbol *current-spy-predicates* :test #'eq))
         (mode *current-spy-mode*)
         (show-p (and spy-p
                      (case mode
                        ((always) t)
                        ((prompt) (spy-prompt goal bindings))
                        (otherwise nil))))
         (result nil))
    (let ((*current-spy-mode* (if show-p mode 'disabled)))
      (when show-p
        (spy-message "CALL" goal bindings))
      (let ((*current-spy-depth* (1+ *current-spy-depth*)))
        (setf result (funcall thunk)))
      (when show-p
        (if (or (not result) (failure-p result))
            (spy-message "FAIL" goal bindings)
            (spy-message "EXIT" goal (success-bindings result)))))
    result))

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

(defun prove (goal bindings remaining-goals)
  (with-spy
   goal
   bindings
   (lambda ()
     (let ((*current-remaining-goals* remaining-goals)
           (*current-bindings* bindings))
       (let* ((predicate-symbol (if (consp goal) (car goal) goal))
              (predicate-handler (get-clauses predicate-symbol))
              (args (if (consp goal) (cdr goal) '()))
              (goal-arity (length args)))
         (if (functionp predicate-handler)
             (apply predicate-handler args)
             (let ((goal-for-unify (if (consp goal) goal (list goal))))
               (if (and (not (null predicate-handler))
                        (< goal-arity (apply #'min
                                             (mapcar (lambda (c) (min-arity (cdar c)))
                                                     predicate-handler))))
                   (make-failure)
                   (try-clauses goal-for-unify bindings remaining-goals
                                predicate-handler)))))))))

(defun insert-choice-point (clause choice-point)
  (labels ((insert-cut-term (term)
             (cond
               ((and (consp term) (eq '! (car term))) (list '! choice-point))
               ((and (atom-p term) (eq '! term)) (list '! choice-point))
               (t term))))
    (mapcar #'insert-cut-term clause)))

(defun try-clauses (goal bindings remaining-goals all-clauses)
  (call-with-current-choice-point
   (lambda (choice-point)
     (let ((goal-arity (if (consp goal) (length (cdr goal)) 0)))
       (labels ((clause-match-p (clause)
                  (let* ((required (min-arity (cdar clause)))
                         (variadic-p (not (listp (cdar clause)))))
                    (and (>= goal-arity required)
                         (or variadic-p (= goal-arity required)))))
                (try-one-by-one (clauses-to-try)
                  (if (null clauses-to-try)
                      (make-failure)
                      (let* ((current-clause (insert-choice-point
                                              (car clauses-to-try) choice-point))
                             (remaining-clauses (cdr clauses-to-try))
                             (try-next-clause (lambda () (try-one-by-one remaining-clauses)))
                             (result (process-one goal current-clause bindings
                                                  remaining-goals)))
                        (if (failure-p result)
                            (funcall try-next-clause)
                            (let* ((result-bindings (success-bindings result))
                                   (result-continuation (success-continuation result))
                                   (new-continuation (combine result-continuation
                                                              try-next-clause)))
                              (make-success :bindings result-bindings
                                            :continuation new-continuation)))))))
         (let ((clauses (remove-if-not #'clause-match-p all-clauses)))
           (try-one-by-one clauses)))))))

(defun prove-all (goals bindings)
  (cond
    ((failure-p bindings) (make-failure))
    ((null goals)
     (let ((terminal-cont (lambda () (make-failure))))
       (make-success :bindings bindings :continuation terminal-cont)))
    (t (prove (car goals) bindings (cdr goals)))))

;;; Interactive query syntax (?-)

(defmacro ?- (&rest goals)
  `(run-query (replace-anonymous-variables ',goals)))

(defun display-solution (bindings)
  (if (null bindings)
      (format t "~%Yes")
      (dolist (var-val bindings)
        (let ((var (car var-val))
              (val (cdr var-val)))
          (format t "~%~A = ~A" var val)))))

(defun continue-prompt-p ()
  (format t "~%Continue ? (y/n) ")
  (finish-output)
  (case (read)
    ((y) t)
    ((n) nil)
    (otherwise
     (format t " Type y for more, or n to stop.~%")
     (continue-prompt-p))))

(defun run-query (goals &optional (max-solutions 10))
  (loop with ss = (make-solution-stream goals)
        with count = 0
        for solution = (funcall ss)
        while (and solution (< count max-solutions))
        do (display-solution solution)
           (incf count)
           (when (< count max-solutions)
             (unless (continue-prompt-p)
               (return)))
        finally (when (= count max-solutions)
                  (format t "~%[Stopped after ~A solutions]~%" max-solutions))
                (when (= count 0)
                  (format t "No.~%"))))

;;; Macro for defining pure Lisp predicates

(defmacro define-predicate ((name &rest arguments) &body body)
  `(set-clauses ',name (lambda ,arguments ,@body)))

(defun ground-p (term)
  (let ((resolved-term (substitute-bindings *current-bindings* term)))
    (cond
      ((variable-p resolved-term) nil)
      ((consp resolved-term)
       (and (ground-p (car resolved-term))
            (ground-p (cdr resolved-term))))
      (t t))))

(defparameter *current-dynamic-parameters* '())

(defun get-dynamic-parameter (variable-symbol)
  (let ((entry (assoc variable-symbol *current-dynamic-parameters* :test #'eq)))
    (if entry
        (cdr entry)
        (let ((new-parameter (cons variable-symbol nil)))
          (push new-parameter *current-dynamic-parameters*)
          new-parameter))))

;;; Solution streams

(defun make-solution-stream (goals)
  (let ((current-result nil)
        (initialized nil))
    (lambda ()
      (unless initialized
        (setf initialized t)
        (setf current-result
              (call-with-current-choice-point
               (lambda (choice-point)
                 (let* ((prepared-goals (replace-anonymous-variables goals))
                        (cut-goals (insert-choice-point prepared-goals choice-point)))
                   (prove-all cut-goals '()))))))
      
      (when (and current-result (success-p current-result))
        (let* ((bindings (success-bindings current-result))
               (query-variables (variables-in goals))
               (solution (mapcar (lambda (v)
                                   (cons v (substitute-bindings bindings v)))
                                 query-variables))
               (continuation (success-continuation current-result)))
          (setf current-result (funcall continuation))
          solution)))))