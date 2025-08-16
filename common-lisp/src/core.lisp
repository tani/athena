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
    :prove-goal
    :prove-goal-sequence
    :prove
    :prove-all
    :insert-choice-point
    :call-with-current-choice-point
    :merge-continuations
    :wrap-success-with-cut-handler
    :*current-bindings*
    :*current-remaining-goals*
    :*current-dynamic-parameters*

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
    :cut-exception-value
    
    ;; Utility functions  
    :ground-p
    
    ;; Enhanced API
    :prolog-solve
    :define-predicate
    :define-predicate!))

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

; Cut & Choice Point Handling
(defun wrap-success-with-cut-handler (result tag)
  "Wrap RESULT so that any future cut with TAG is caught and handled.
  If RESULT is a success, its continuation is wrapped; wrapping is
  applied recursively to all subsequent successes."
  (if (success-p result)
      (let ((bindings (success-bindings result))
            (cont (success-continuation result)))
        (make-success
         :bindings bindings
         :continuation
         (lambda ()
           (handler-case
               (let ((next (funcall cont)))
                 (wrap-success-with-cut-handler next tag))
             (cut-exception (err)
               (if (eq tag (cut-exception-tag err))
                   (wrap-success-with-cut-handler
                    (cut-exception-value err) tag)
                   (error err)))))))
      result))

(defun call-with-current-choice-point (proc)
  "Execute PROC with a fresh choice point tag for cut handling.
  Ensures the cut handler remains active across continuations."
  (let ((tag (gensym "CHOICE-POINT-")))
    (handler-case
        (let ((result (funcall proc tag)))
          (wrap-success-with-cut-handler result tag))
      (cut-exception (err)
        (if (eq tag (cut-exception-tag err))
            (wrap-success-with-cut-handler
             (cut-exception-value err) tag)
            (error err))))))

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
  #+sbcl
  (declare (optimize (sb-ext:inhibit-warnings 3)))
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


;;; Query macro (simplified for now)
(defmacro ?- (&rest goals)
  `(run-query (replace-anonymous-variables ',goals)))

;;; Spy/debugging support
(defun spy-prompt (goal bindings)
  (let ((resolved (substitute-bindings bindings goal))
        (*package* (find-package :prolog/core)))
    (format t "Spy on ~S? [l=leap c=creep n=nodebug] " resolved)
    (force-output *standard-output*)
    (case (read)
      ((l) (setf *current-spy-mode* 'always) t)
      ((c) t)
      ((n) (setf *current-spy-mode* 'disabled) nil)
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

;;; Proof Engine Core
(defun prove-goal-sequence (goals bindings)
  "Prove a sequence of GOALS with the given BINDINGS.
  GOALS is a list of goals to prove in sequence.
  BINDINGS is the current variable binding environment.
  
  Returns success with terminal continuation if all goals are proven,
  otherwise delegates to `prove-goal' for the goal sequence.
  Base case for empty goal lists returns success with a terminal continuation."
  (cond
    ((failure-p bindings) (make-failure))
    ((null goals)
     (let ((terminal-cont (lambda () (make-failure))))
       (make-success :bindings bindings :continuation terminal-cont)))
    (t (prove-goal goals bindings))))

(defun insert-choice-point (clause choice-point)
  "Insert CHOICE-POINT tag into cut operators (!) within CLAUSE.
  CLAUSE is the clause to modify.
  CHOICE-POINT is the unique tag to associate with cuts in this clause.
  
  Transforms bare ! atoms and (!) lists to include the choice point tag
  for proper cut semantics. Used to link cuts to their originating choice points."
  (labels ((insert-cut-term (term)
             (cond
               ((and (consp term) (symbol= '! (car term)) (consp (cdr term)))
                (error "Invalid choice-point insertion happened"))
               ((and (consp term) (symbol= '! (car term)) (null (cdr term)))
                (list '! choice-point))
               ((and (atom-p term) (symbol= '! term))
                (list '! choice-point))
               (t term))))
    (mapcar #'insert-cut-term clause)))

(defun apply-clause-to-goal (goals bindings clause)
  "Apply CLAUSE to prove the first goal in GOALS with BINDINGS.
  GOALS is the list of goals, where the first will be unified with the
  clause head. BINDINGS is the current variable binding environment.
  CLAUSE is the clause to apply (head + body).
  
  Attempts to unify the goal with the clause head, then proves the
  clause body plus remaining goals if unification succeeds.
  Returns success object on success, failure object on failure."
  (let* ((goal (car goals))
         (remaining-goals (cdr goals))
         (goal-for-unify (if (consp goal) goal (list goal)))
         (renamed-clause (rename-vars clause))
         (clause-head (car renamed-clause))
         (clause-body (cdr renamed-clause))
         (new-bindings (unify goal-for-unify clause-head bindings)))
    (if (failure-p new-bindings)
        (make-failure)
        (prove-goal-sequence (append clause-body remaining-goals) new-bindings))))

(defun merge-continuations (continuation-a continuation-b)
  "Merge two backtracking continuations into a single continuation.
  CONTINUATION-A is the first continuation to try.
  CONTINUATION-B is the second continuation to try if the first fails.
  
  Returns a continuation that first tries CONTINUATION-A, and if that fails
  or produces no more solutions, tries CONTINUATION-B. This is the core
  backtracking mechanism that enables trying alternative solutions."
  (lambda ()
    (let ((result-a (funcall continuation-a)))
      (if (or (not result-a) (failure-p result-a))
          (funcall continuation-b)
          (let* ((bindings (success-bindings result-a))
                 (result-continuation (success-continuation result-a))
                 (new-continuation (merge-continuations result-continuation continuation-b)))
            (make-success :bindings bindings :continuation new-continuation))))))

(defun search-matching-clauses (goals bindings all-clauses)
  "Search through ALL-CLAUSES to find matches for the first goal in GOALS.
  GOALS is the list of goals to prove.
  BINDINGS is the current variable binding environment.
  ALL-CLAUSES is the list of clauses to try.
  
  Tries each clause with backtracking. Uses choice points to handle cut
  operations correctly. Returns success object with continuation for
  backtracking, or failure if no clauses match."
  (call-with-current-choice-point
    (lambda (choice-point)
      (labels ((try-one-by-one (clauses-to-try)
                 (if (null clauses-to-try)
                     (make-failure)
                     (let* ((current-clause (insert-choice-point (car clauses-to-try) choice-point))
                            (remaining-clauses (cdr clauses-to-try))
                            (try-next-clause (lambda () (try-one-by-one remaining-clauses)))
                            (result (apply-clause-to-goal goals bindings current-clause)))
                       (if (failure-p result)
                           (funcall try-next-clause)
                           (let* ((result-bindings (success-bindings result))
                                  (result-continuation (success-continuation result))
                                  (new-continuation (merge-continuations result-continuation try-next-clause)))
                             (make-success :bindings result-bindings :continuation new-continuation)))))))
        (try-one-by-one all-clauses)))))

(defun prove-goal (goals bindings)
  "Prove the first goal in GOALS with the given BINDINGS.
  GOALS is a list of goals where the first will be proven.
  BINDINGS is the current variable binding environment.
  
  Looks up the predicate handler (clauses or built-in function) and attempts
  resolution. Handles spy tracing and delegates to appropriate resolution method.
  This is the core resolution function that drives the proof search."
  (let* ((goal (car goals))
         (remaining-goals (cdr goals))
         (predicate-symbol (if (consp goal) (car goal) goal))
         (predicate-handler (get-clauses predicate-symbol))
         (args (if (consp goal) (cdr goal) '())))
    (with-spy goal bindings
      (lambda ()
        (cond
          ((functionp predicate-handler)
           (let* ((*current-remaining-goals* remaining-goals)
                  (*current-bindings* bindings))
             (apply predicate-handler args)))
          (t (search-matching-clauses goals bindings predicate-handler)))))))

(defun solve (goals on-success on-failure)
  "Solve GOALS and call callbacks for each solution.
  GOALS is the list of goals to solve.
  ON-SUCCESS is a function called with variable bindings for each solution found.
  ON-FAILURE is a function called once when no more solutions exist.
  
  This is the core solution iterator that drives the proof search and handles
  solution enumeration through backtracking."
  (labels ((initial-continuation ()
             (call-with-current-choice-point
               (lambda (_choice-point)
                 (let* ((prepared-goals (replace-anonymous-variables goals)))
                   (prove-goal-sequence prepared-goals '())))))
           (retrieve-success-bindings (result)
             (let* ((bindings (success-bindings result))
                    (query-variables (variables-in goals))
                    (make-binding-pair (lambda (v) (cons v (substitute-bindings bindings v)))))
               (mapcar make-binding-pair query-variables)))
           (execute-success-continuation (result)
             (funcall (success-continuation result))))
    (do ((result (initial-continuation) (execute-success-continuation result)))
        ((not (success-p result)) (funcall on-failure))
      (funcall on-success (retrieve-success-bindings result)))))

(defun display-solution (bindings)
  (if (null bindings)
    (format t "~%Yes")
    (dolist (var-val bindings)
      (format t "~%~A = ~A" (car var-val) (cdr var-val)))))

(defun continue-prompt-p ()
  (let ((*package* (find-package :prolog/core)))
    (format t "~%Continue ? (y/n) ")
    (force-output *standard-output*)
    (case (read)
      ((y) t)
      ((n) nil)
      (otherwise
        (format t " Type y for more, or n to stop.~%")
        (continue-prompt-p)))))

(defun run-query (goals)
  "Execute GOALS as an interactive query with user continuation prompt.
  GOALS is the list of goals to execute.
  
  Displays each solution and prompts whether to continue searching for more.
  Shows 'No' if no solutions exist. This is the main entry point for
  interactive queries initiated by `?-'."
  (block query-exit
    (solve goals
      (lambda (solution)
        (display-solution solution)
        (unless (continue-prompt-p)
          (return-from query-exit)))
      (lambda ()
        (format t "No.~%")
        (return-from query-exit)))))

;;; Utility functions
(defun ground-p (term)
  "Check if TERM is fully ground (contain no unbound variables).
  TERM is the term to check for groundness.
  
  A term is ground if it contains no variables or all variables in it
  are bound to ground terms. Uses the current binding environment
  from `*current-bindings*'.
  
  Returns non-nil if TERM is ground, nil otherwise."
  (let ((resolved-term (substitute-bindings *current-bindings* term)))
    (cond
      ((variable-p resolved-term) nil)
      ((consp resolved-term)
       (and (ground-p (car resolved-term))
            (ground-p (cdr resolved-term))))
      (t t))))

;;; Convenience macros and public API improvements

;; Enhanced solve function matching eprolog-solve
(defun prolog-solve (goals &key (on-success (lambda (bindings) (declare (ignore bindings)))) 
                              (on-failure (lambda ())))
  "Solve GOALS and call callbacks for each solution.
  GOALS is the list of goals to solve.
  
  Keyword arguments:
  :ON-SUCCESS - Function called with variable bindings for each solution found.
                Defaults to a no-op function.
  :ON-FAILURE - Function called once when no more solutions exist.
                Defaults to a no-op function.
  
  This is the core solution iterator that drives the proof search and handles
  solution enumeration through backtracking."
  (solve goals on-success on-failure))

;; Convenience aliases
(defmacro define-predicate (head &rest body)
  "Define a Prolog clause (fact or rule) and add it to the clause database.
  HEAD is the predicate head, a list of (NAME . ARGS), or a symbol for
  predicates with no arguments.
  BODY is the optional list of goals forming the rule body.
  
  If BODY is empty, defines a fact. If BODY is non-empty, defines a rule.
  Anonymous variables are automatically replaced with unique variables."
  (let* ((head-list (if (consp head) head (list head)))
         (clause (cons head-list body)))
    `(add-clause! (replace-anonymous-variables ',clause))))

(defmacro define-predicate! (head &rest body)
  "Define a Prolog clause, replacing existing clauses with the same arity.
  HEAD is the predicate head, a list of (NAME . ARGS), or a symbol for
  predicates with no arguments.
  BODY is the optional list of goals forming the rule body.
  
  Similar to `define-predicate' but removes existing
  clauses for the predicate with the same arity before adding the new
  clause. Used for predicate redefinition."
  (let* ((head-list (if (consp head) head (list head)))
         (name (car head-list))
         (args (cdr head-list))
         (arity (min-arity args))
         (clause (cons head-list body)))
    `(progn
       (remove-clauses-with-arity! ',name ,arity)
       (add-clause! (replace-anonymous-variables ',clause)))))

;; Aliases for convenience (matching eprolog)
(defmacro <- (&rest clause-parts)
  "Alias for defining clauses."
  (let ((clause (if (consp (car clause-parts))
                 clause-parts
                 (cons (list (car clause-parts)) (cdr clause-parts)))))
    `(add-clause! (replace-anonymous-variables ',clause))))

(defmacro <-- (&rest clause-parts)
  "Alias for defining clauses with replacement."
  (let* ((head (car clause-parts))
         (body (cdr clause-parts))
         (name (if (consp head) (car head) head))
         (args (if (consp head) (cdr head) '()))
         (arity (min-arity args))
         (clause (if (consp head)
                  (cons head body)
                  (cons (list head) body))))
    `(progn
      (remove-clauses-with-arity! ',name ,arity)
      (add-clause! (replace-anonymous-variables ',clause)))))

;;; Backward compatibility aliases
(defun prove (goals bindings)
  "Backward compatibility wrapper for prove-goal."
  (prove-goal goals bindings))

(defun prove-all (goals bindings)
  "Backward compatibility wrapper for prove-goal-sequence."
  (prove-goal-sequence goals bindings))
