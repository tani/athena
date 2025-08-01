;; prolog-core.scm — Prolog engine core implementation
;; Copyright © 2025 Masaya Taniguchi
;; Released under the GNU General Public License v3.0
;;
;; This file contains the fundamental Prolog engine components:
;; - Unification and variable binding
;; - Clause database management
;; - Proof engine and backtracking
;; - Solution streams and query execution

(begin
  (define gensym
    (let ((counter 0))
      (lambda (prefix)
        (let* ((counter-string (number->string counter))
               (name (string-append prefix counter-string)))
          (set! counter (+ counter 1))
          (string->symbol name)))))

  (define (call-with-current-choice-point proc)
    (let ((tag (gensym "choice-point-")))
      (guard (exception
              ((and
                  (cut-exception? exception)
                  (eq? tag (cut-exception-tag exception)))
               (cut-exception-value exception))
              (else
                (raise exception)))
        (proc tag))))

  ;; Bindings and unification

  (define (variable? term)
    (and (symbol? term)
      (let ((symbol-string (symbol->string term)))
        (and (> (string-length symbol-string) 0)
          (let ((first-char (string-ref symbol-string 0)))
            (char=? first-char #\?))))))

  (define (named-variable? term)
    (and (variable? term) (not (eq? term '?))))

  (define (atom? term)
    (not (pair? term)))

  (define (lookup-variable variable bindings)
    (cdr (assoc variable bindings)))

  (define (substitute-bindings bindings expression . visited)
    (let ((visited (if (pair? visited) (car visited) '())))
      (cond
        ((failure? bindings) (make-failure))
        ((null? bindings) expression)
        ((and (variable? expression) (assoc expression bindings))
         (if (member expression visited)
           expression
           (let ((value (lookup-variable expression bindings)))
             (substitute-bindings bindings value (cons expression visited)))))
        ((atom? expression) expression)
        (else
          (let* ((substituted-car (substitute-bindings bindings (car expression) visited))
                 (substituted-cdr (substitute-bindings bindings (cdr expression) visited)))
            (cons substituted-car substituted-cdr))))))

  (define (variables-in expression)
    (define (collect-unique-if predicate tree)
      (let loop ((subtree tree) (accumulator '()))
        (if (atom? subtree)
          (if (and (predicate subtree) (not (memq subtree accumulator)))
            (cons subtree accumulator)
            accumulator)
          (let ((accumulator-after-car (loop (car subtree) accumulator)))
            (loop (cdr subtree) accumulator-after-car)))))
    (reverse (collect-unique-if named-variable? expression)))

  (define (replace-anonymous-variables expression)
    (cond
      ((eq? expression '?) (gensym "?"))
      ((atom? expression) expression)
      (else
        (let* ((car-replaced (replace-anonymous-variables (car expression)))
               (cdr-replaced (replace-anonymous-variables (cdr expression))))
          (cons car-replaced cdr-replaced)))))

  (define current-occurs-check (make-parameter #t))

  (define (occurs-check? variable expression bindings)
    (cond
      ((eq? variable expression) #t)
      ((and (variable? expression) (assoc expression bindings))
       (let ((value (lookup-variable expression bindings)))
         (occurs-check? variable value bindings)))
      ((pair? expression)
       (let ((car-occurs-check? (occurs-check? variable (car expression) bindings))
             (cdr-occurs-check? (occurs-check? variable (cdr expression) bindings)))
         (or car-occurs-check? cdr-occurs-check?)))
      (else #f)))

  (define (unify-var variable value bindings)
    (cond
      ((assoc variable bindings)
       (let ((bound-term (lookup-variable variable bindings)))
         (unify bound-term value bindings)))
      ((and (variable? value) (assoc value bindings))
       (let ((bound-term (lookup-variable value bindings)))
         (unify variable bound-term bindings)))
      ((and (current-occurs-check) (occurs-check? variable value bindings))
       (make-failure))
      (else (alist-cons variable value bindings))))

  (define (unify term1 term2 bindings)
    (cond
      ((failure? bindings) (make-failure))
      ((equal? term1 term2) bindings)
      ((variable? term1) (unify-var term1 term2 bindings))
      ((variable? term2) (unify-var term2 term1 bindings))
      ((and (pair? term1) (pair? term2))
       (let ((car-bindings (unify (car term1) (car term2) bindings)))
         (unify (cdr term1) (cdr term2) car-bindings)))
      (else (make-failure))))

  ;; Clause database

  (define current-clause-database (make-parameter '()))

  (define (get-clauses predicate-symbol)
    (let ((entry (assoc predicate-symbol (current-clause-database))))
      (if entry (cdr entry) '())))

  (define (set-clauses! predicate-symbol clauses)
    (let* ((current-db (current-clause-database))
           (cleaned-db (alist-delete predicate-symbol current-db eq?))
           (new-db (alist-cons predicate-symbol clauses cleaned-db)))
      (current-clause-database new-db)))

  (define (add-clause! clause)
    (let* ((predicate-symbol (caar clause))
           (current-clauses (get-clauses predicate-symbol))
           (new-clauses (append current-clauses (list clause))))
      (set-clauses! predicate-symbol new-clauses)))

  (define (min-arity args)
    (let loop ((lst args) (count 0))
      (cond
        ((pair? lst) (loop (cdr lst) (+ count 1)))
        ((null? lst) count)
        (else count))))

  (define-syntax <-
    (syntax-rules ()
      ((_ (name . args) . body)
       (add-clause! (replace-anonymous-variables '((name . args) . body))))
      ((_ name . body)
       (add-clause! (replace-anonymous-variables '((name) . body))))))

  (define (remove-clauses-with-arity! predicate-symbol arity)
    (define (has-different-arity? clause)
      (not (= (min-arity (cdar clause)) arity)))
    (let* ((current-clauses (get-clauses predicate-symbol))
           (new-clauses (filter has-different-arity? current-clauses)))
      (set-clauses! predicate-symbol new-clauses)))

  (define-syntax <--
    (syntax-rules ()
      ((_ (name . args) . body)
       (let ((arity (min-arity 'args)))
         (remove-clauses-with-arity! 'name arity)
         (add-clause! (replace-anonymous-variables '((name . args) . body)))))
      ((_ name . body)
       (let ((arity 0))
         (remove-clauses-with-arity! 'name arity)
         (add-clause! (replace-anonymous-variables '((name) . body)))))))

  ;; Prover engine

  (define current-bindings (make-parameter '()))

  (define current-remaining-goals (make-parameter '()))

  (define (sublis alist tree)
    (if (atom? tree)
      (let ((binding (assoc tree alist)))
        (if binding (cdr binding) tree))
      (let ((car-sublis (sublis alist (car tree)))
            (cdr-sublis (sublis alist (cdr tree))))
        (cons car-sublis cdr-sublis))))

  (define (make-renaming-pair variable)
    (let ((var-string (symbol->string variable)))
      (cons variable (gensym var-string))))

  (define (rename-vars expression)
    (let* ((variables (variables-in expression))
           (alist (map make-renaming-pair variables)))
      (sublis alist expression)))

  ;; Spy support

  (define current-spy-mode
    ;; Possible values: 'prompt, 'always, or 'disabled
    (make-parameter 'prompt))

  (define current-spy-predicates (make-parameter '()))

  (define (spy-prompt goal bindings)
    (let ((resolved (substitute-bindings bindings goal)))
      (display "Spy on ")
      (write resolved)
      (display "? [l=leap c=creep n=nodebug] ")
      (flush-output-port (current-output-port))
      (case (read)
        ((l) (current-spy-mode 'always) #t)
        ((c) #t)
        ((n) (current-spy-mode 'disabled) #f)
        (else #t))))

  (define (spy-message kind goal bindings)
    (let ((resolved (substitute-bindings bindings goal)))
      (display kind)
      (display ": ")
      (write resolved)
      (newline)))

  (define (with-spy goal bindings thunk)
    (let* ((predicate-symbol (if (pair? goal) (car goal) goal))
           (spy? (memq predicate-symbol (current-spy-predicates)))
           (mode (current-spy-mode))
           (show? (and
                   spy?
                   (case mode
                     ((always) #t)
                     ((prompt) (spy-prompt goal bindings))
                     (else #f))))
           (result #f))
      (parameterize ((current-spy-mode (if show? mode 'disabled)))
        (dynamic-wind
          (lambda ()
            (when show?
              (spy-message "CALL" goal bindings)))
          (lambda () (set! result (thunk)))
          (lambda ()
            (when show?
              (if (or (not result) (failure? result))
                (spy-message "FAIL" goal bindings)
                (spy-message "EXIT" goal (success-bindings result)))))))
      result))

  (define (process-one goal clause bindings remaining-goals)
    (let* ((renamed-clause (rename-vars clause))
           (clause-head (car renamed-clause))
           (clause-body (cdr renamed-clause))
           (new-bindings (unify goal clause-head bindings)))
      (if (failure? new-bindings)
        (make-failure)
        (prove-all (append clause-body remaining-goals) new-bindings))))

  (define (combine continuation-a continuation-b)
    (lambda ()
      (let ((result-a (continuation-a)))
        (if (or (not result-a) (failure? result-a))
          (continuation-b)
          (let* ((bindings (success-bindings result-a))
                 (result-continuation (success-continuation result-a))
                 (new-continuation (combine result-continuation continuation-b)))
            (make-success bindings new-continuation))))))

  (define (prove goal bindings remaining-goals)
    (with-spy
      goal
      bindings
      (lambda ()
        (parameterize ((current-remaining-goals remaining-goals)
                       (current-bindings bindings))
          (let* ((predicate-symbol (if (pair? goal) (car goal) goal))
                 (predicate-handler (get-clauses predicate-symbol))
                 (args (if (pair? goal) (cdr goal) '()))
                 (goal-arity (length args))
                 (get-min-arity (lambda (c) (min-arity (cdar c)))))
            (if (procedure? predicate-handler)
              (apply predicate-handler args)
              (let ((goal-for-unify (if (pair? goal) goal (list goal))))
                (if (and
                     (not (null? predicate-handler))
                     (< goal-arity (apply min (map get-min-arity predicate-handler))))
                  (make-failure)
                  (try-clauses goal-for-unify bindings remaining-goals predicate-handler)))))))))
  (define (insert-choice-point clause choice-point)
    (define (insert-cut-term term)
      (cond
        ((and (pair? term) (eq? '! (car term))) (list '! choice-point))
        ((and (atom? term) (eq? '! term)) (list '! choice-point))
        (else term)))
    (map insert-cut-term clause))

  (define (try-clauses goal bindings remaining-goals all-clauses)
    (call-with-current-choice-point
      (lambda (choice-point)
        (define goal-arity (if (pair? goal) (length (cdr goal)) 0))
        (define (clause-match? clause)
          (let* ((required (min-arity (cdar clause)))
                 (variadic? (not (list? (cdar clause)))))
            (and
              (>= goal-arity required)
              (or variadic? (= goal-arity required)))))
        (define (try-one-by-one clauses-to-try)
          (if (null? clauses-to-try)
            (make-failure)
            (let* ((current-clause (insert-choice-point (car clauses-to-try) choice-point))
                   (remaining-clauses (cdr clauses-to-try))
                   (try-next-clause (lambda () (try-one-by-one remaining-clauses)))
                   (result (process-one goal current-clause bindings remaining-goals)))
              (if (failure? result)
                (try-next-clause)
                (let* ((result-bindings (success-bindings result))
                       (result-continuation (success-continuation result))
                       (new-continuation (combine result-continuation try-next-clause)))
                  (make-success result-bindings new-continuation))))))
        (let ((clauses (filter clause-match? all-clauses)))
          (try-one-by-one clauses)))))

  (define (prove-all goals bindings)
    (cond
      ((failure? bindings) (make-failure))
      ((null? goals)
       (let ((terminal-cont (lambda () (make-failure))))
         (make-success bindings terminal-cont)))
      (else (prove (car goals) bindings (cdr goals)))))

  (define (solve goals on-success on-failure)
    (define (initial-continuation)
      (call-with-current-choice-point
        (lambda (choice-point)
          (let* ((prepared-goals (replace-anonymous-variables goals))
                 (cut-goals (insert-choice-point prepared-goals choice-point)))
            (prove-all cut-goals '())))))
    (define (retrieve-success-bindings result)
      (let* ((bindings (success-bindings result))
             (query-variables (variables-in goals))
             (make-pair (lambda (v) (cons v (substitute-bindings bindings v)))))
        (map make-pair query-variables)))
    (define (execute-success-continuation result)
      ((success-continuation result)))
    (let loop ((result (initial-continuation)))
      (if (success? result)
        (begin
          (on-success (retrieve-success-bindings result))
          (loop (execute-success-continuation result)))
        (on-failure))))

  (define (display-solution bindings)
    (if (null? bindings)
      (begin
        (newline)
        (display "Yes"))
      (for-each
        (lambda (var-val)
          (let ((var (car var-val))
                (val (cdr var-val)))
            (newline)
            (display var)
            (display " = ")
            (display val)))
        bindings)))

  (define (continue-prompt?)
    (newline)
    (display "Continue ? (y/n) ")
    (flush-output-port (current-output-port))
    (case (read)
      ((y) #t)
      ((n) #f)
      (else
        (display " Type y for more, or n to stop.")
        (newline)
        (continue-prompt?))))

  (define (run-query goals)
    (call/cc
      (lambda (exit)
        (solve
          goals
          (lambda (solution)
            (display-solution solution)
            (unless (continue-prompt?) (exit)))
          (lambda ()
            (display "No.")
            (newline)
            (exit))))))

  ;; Interactive query syntax (?-)

  (define-syntax ?-
    (syntax-rules ()
      ((_ . goals)
       (run-query (replace-anonymous-variables 'goals))))))
