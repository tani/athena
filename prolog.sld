(define-library (prolog)
  (export
    variable?
    named-variable?
    atom?
    failure
    *empty-bindings*
    failure?
    extend-bindings
    substitute-bindings
    variables-in
    replace-anonymous-variables
    unify
    *prompt-stack*
    *failure-object*
    clause-database
    add-clause!
    get-clauses
    define-predicate
    object->string
    <-
    <--
    ?-
    prove-all
    reset
    shift)
  (import (scheme base)
          (scheme eval)
          (scheme write)
          (only (srfi 1) alist-delete filter delete-duplicates))
  (cond-expand
   (chicken
    (import scheme (only (srfi 132) list-sort)))
   (guile
    (import (only (rnrs sorting (6)) list-sort)))
   (gauche
    (import (only (scheme sort) list-sort)))
   (chibi
    (import (only (scheme sort) list-sort)))
   (gambit
    (import (only (srfi 132) list-sort)))
   (sagittarius
    (import (only (scheme sort) list-sort))))
  (begin
    (define gensym-counter 0)
    (define (gensym prefix)
      (let ((name (string-append prefix (number->string gensym-counter))))
        (set! gensym-counter (+ gensym-counter 1))
        (string->symbol name)))

    (define (object->string term)
      (parameterize ((current-output-port (open-output-string)))
        (display term)
        (get-output-string (current-output-port))))

    (define *prompt-stack* (make-parameter (list)))

    (define-syntax reset
      (syntax-rules ()
        ((_ body ...)
         (call/cc
          (lambda (prompt-continuation)
            (parameterize ((*prompt-stack* (cons prompt-continuation (*prompt-stack*))))
              (begin body ...)))))))

    (define-syntax shift
      (syntax-rules ()
        ((_ k-arg body ...)
         (call/cc
          (lambda (escape-continuation)
            (let ((prompts (*prompt-stack*)))
              (if (null? prompts)
                  (error "shift: no corresponding reset found")
                  (let ((prompt (car prompts))
                        (rest-prompts (cdr prompts)))
                    (parameterize ((*prompt-stack* rest-prompts))
                      (prompt
                       (let ((k-arg (lambda (value)
                                      (escape-continuation value))))
                         body ...)))))))))))

    ;; Core data structures and helpers
    (define *failure-tag* (list 'fail))
    (define *failure-object* (cons *failure-tag* #f))
    (define *empty-bindings* '((#t . #t)))

    (define (success bindings continuation)
      (cons bindings continuation))

    (define (failure)
      *failure-object*)

    (define (variable? term)
      (and (symbol? term)
           (let ((symbol-string (symbol->string term)))
             (and (> (string-length symbol-string) 0)
                  (char=? (string-ref symbol-string 0) #\?)))))

    (define (named-variable? term)
      (and (variable? term) (not (eq? term '?))))

    (define (atom? term)
      (not (pair? term)))

    (define (failure? result)
      (and (pair? result)
           (eq? (car result) *failure-tag*)))

    (define (get-binding variable bindings)
      (assoc variable bindings))

    (define (lookup-variable variable bindings)
      (cdr (get-binding variable bindings)))

    (define (extend-bindings variable value bindings)
      (cons (cons variable value)
            (if (eq? bindings *empty-bindings*)
                (list)
                bindings)))

    (define (substitute-bindings bindings expression)
      (cond
       ((failure? bindings) (failure))
       ((eq? bindings *empty-bindings*) expression)
       ((and (variable? expression)
             (get-binding expression bindings))
        (substitute-bindings bindings
                             (lookup-variable expression bindings)))
       ((atom? expression) expression)
       (else
        (cons (substitute-bindings bindings (car expression))
              (substitute-bindings bindings (cdr expression))))))

    (define (collect-unique-if predicate tree)
      (let loop ((subtree tree) (found (list)))
        (if (atom? subtree)
            (if (and (predicate subtree) (not (memq subtree found)))
                (cons subtree found)
                found)
            (loop (cdr subtree) (loop (car subtree) found)))))

    (define (variables-in expression)
      (reverse (collect-unique-if named-variable? expression)))

    (define (replace-anonymous-variables expression)
      (cond
       ((eq? expression '?) (gensym "?"))
       ((atom? expression) expression)
       (else
        (cons (replace-anonymous-variables (car expression))
              (replace-anonymous-variables (cdr expression))))))

    (define current-bindings (make-parameter *empty-bindings*))
    (define current-goals (make-parameter (list)))
    (define *occurs-check* (make-parameter #t))

    (define (unify term1 term2 bindings)
      (cond
       ((failure? bindings) (failure))
       ((equal? term1 term2) bindings)
       ((variable? term1) (unify-variable term1 term2 bindings))
       ((variable? term2) (unify-variable term2 term1 bindings))
       ((and (pair? term1) (pair? term2))
        (unify (cdr term1) (cdr term2)
               (unify (car term1) (car term2) bindings)))
       (else (failure))))

    (define (unify-variable variable value bindings)
      (cond
       ((get-binding variable bindings)
        (unify (lookup-variable variable bindings) value bindings))
       ((and (variable? value) (get-binding value bindings))
        (unify variable (lookup-variable value bindings) bindings))
       ((and (*occurs-check*)
             (occurs-check? variable value bindings))
        (failure))
       (else
        (extend-bindings variable value bindings))))

    (define (occurs-check? variable expression bindings)
      (cond
       ((eq? variable expression) #t)
       ((and (variable? expression)
             (get-binding expression bindings))
        (occurs-check? variable (lookup-variable expression bindings) bindings))
       ((pair? expression)
        (or (occurs-check? variable (car expression) bindings)
            (occurs-check? variable (cdr expression) bindings)))
       (else #f)))

    (define clause-database (make-parameter (list)))

    (define (get-clauses predicate)
      (let ((entry (assoc predicate (clause-database))))
        (if entry (cdr entry) (list))))

    (define (set-clauses! predicate clauses)
      (clause-database
            (cons (cons predicate clauses)
                  (alist-delete predicate (clause-database) eq?))))

    (define (add-clause! clause)
      (let ((predicate (caar clause)))
        (set-clauses! predicate
                      (append (get-clauses predicate)
                              (list clause)))))

    (define-syntax <-
      (syntax-rules ()
        ((_ head . body)
         (add-clause! (replace-anonymous-variables '(head . body))))))

    (define (remove-clauses-with-arity! predicate arity)
      (let ((test (lambda (clause) (not (= (length (cdar clause)) arity)))))
        (set-clauses! predicate (filter test (get-clauses predicate)))))

    (define-syntax <--
      (syntax-rules ()
        ((_ head . body)
         (begin
           (remove-clauses-with-arity!
            (car 'head)
            (length (cdr 'head)))
           (add-clause! (replace-anonymous-variables '(head . body)))))))

    ;; Prolog engine
    (define (sublis alist tree)
      (if (atom? tree)
          (let ((binding (assoc tree alist)))
            (if binding (cdr binding) tree))
          (cons (sublis alist (car tree))
                (sublis alist (cdr tree)))))

    (define (rename-variables expression)
      (let ((variables (variables-in expression))
            (renamer (lambda (v) (cons v (gensym (symbol->string v))))))
        (sublis (map renamer variables) expression)))

    (define (process-one-clause goal clause bindings other-goals)
      (let* ((renamed-clause (rename-variables clause))
             (head (car renamed-clause))
             (body (cdr renamed-clause))
             (new-bindings (unify goal head bindings)))
        (prove-all (append body other-goals) new-bindings)))

    (define (combine-choices choice-a choice-b)
      (lambda ()
        (let ((result-a (and choice-a (choice-a))))
          (if (or (not result-a) (failure? result-a))
              (and choice-b (choice-b))
              result-a))))

    (define (try-clauses goal bindings other-goals clauses)
      (if (null? clauses)
          (failure)
          (let* ((current-clause (car clauses))
                 (remaining-clauses (cdr clauses))
                 (result (process-one-clause goal current-clause bindings other-goals))
                 (try-remaining-clauses
                   (lambda () (try-clauses goal bindings other-goals remaining-clauses))))

            (if (failure? result)
                (try-remaining-clauses)
                (let ((solution-bindings (car result))
                      (more-solutions-from-this-clause (cdr result)))
                  (success solution-bindings
                           (combine-choices more-solutions-from-this-clause
                                            try-remaining-clauses)))))))

    (define (prove goal bindings other-goals)
      (parameterize ((current-goals other-goals)
                     (current-bindings bindings))
        (let* ((predicate (if (pair? goal) (car goal) goal))
               (clauses (get-clauses predicate)))
          (if (procedure? clauses)
              (apply clauses (if (pair? goal) (cdr goal) (list)))
              (reset (try-clauses goal bindings other-goals clauses))))))

    (define (prove-all goals bindings)
      (cond
       ((failure? bindings) (failure))
       ((null? goals) (success bindings #f))
       (else (prove (car goals)
                    bindings
                    (cdr goals)))))

    (define-syntax ?-
      (syntax-rules ()
        ((_ . goals)
         (run-query (replace-anonymous-variables 'goals)))))

    (define (run-query goals)
      (let ((query-variables (variables-in goals)))
        (reset
         (let loop ((query-continuation (lambda ()
                                          (prove-all goals *empty-bindings*))))
           (let ((result (query-continuation)))
             (if (failure? result)
                 (begin (display "No.")
                        (newline)
                        (newline))
                 (let ((solution-bindings (car result))
                       (next-continuation (cdr result)))
                   (display-solution query-variables solution-bindings)
                   (if (and next-continuation (continue-prompt?))
                       (loop next-continuation)
                       (newline)))))))))

    (define (display-solution variables bindings)
      (if (null? variables)
          (display "Yes.")
          (for-each (lambda (variable)
                      (display variable)
                      (display " = ")
                      (write (substitute-bindings bindings variable))
                      (newline))
                    variables)))

    (define (continue-prompt?)
      (display " ")
      (flush-output-port)
      (let ((input-char (read-char)))
        (case input-char
          ((#\;) #t)
          ((#\.) #f)
          ((#\newline) (continue-prompt?))
          (else (display " Type ; for more, or . to stop.")
                (newline)
                (continue-prompt?)))))

    (define-syntax define-predicate
      (syntax-rules ()
        ((_ (name . args) . body)
         (set-clauses! 'name (lambda args . body)))))

    (define (resolve-binding bindings term)
      (let ((value (substitute-bindings bindings term)))
        (if (variable? value)
            value
            (substitute-bindings bindings value))))

    (define (ground? bindings term)
      (let ((resolved-term (resolve-binding bindings term)))
        (cond ((variable? resolved-term) #f)
              ((pair? resolved-term)
               (and (ground? bindings (car resolved-term))
                    (ground? bindings (cdr resolved-term))))
              (else #t))))

    (define *dynamic-parameters* (make-parameter (list)))

    (define (get-dynamic-parameter var-symbol)
      (let* ((alist (*dynamic-parameters*))
             (entry (assoc var-symbol alist)))
        (if entry
            (cdr entry)
            (let ((new-param (make-parameter #f)))
              (*dynamic-parameters* (cons (cons var-symbol new-param) alist))
              new-param))))

    (define-predicate (fail) (failure))

    (define-predicate (cut) (shift k (prove-all (current-goals) (current-bindings))))

    (define-predicate (= term1 term2)
      (prove-all (current-goals) (unify term1 term2 (current-bindings))))

    (define-predicate (== term1 term2)
      (if (equal? (substitute-bindings (current-bindings) term1)
                  (substitute-bindings (current-bindings) term2))
          (prove-all (current-goals) (current-bindings))
          (failure)))

    (define-predicate (call goal)
      (prove-all (cons (substitute-bindings (current-bindings) goal) (current-goals))
                 (current-bindings)))

    (cond-expand
      (gambit (define (environment x) 5))
      (else))

    (define current-lisp-environment (make-parameter (environment '(scheme base))))
    (define-predicate (lisp-eval-internal result-var expression)
      (let* ((ctx-bindings (current-bindings))
             (expr-scm (substitute-bindings ctx-bindings expression))
             (evaluated (eval expr-scm (current-lisp-environment)))
             (res-term (substitute-bindings ctx-bindings result-var))
             (new-bindings (unify res-term evaluated ctx-bindings)))
        (prove-all (current-goals) new-bindings)))

    (define-predicate (atom term)
      (let ((v (substitute-bindings (current-bindings) term)))
        (if (and (symbol? v) (not (variable? v)))
            (prove-all (current-goals) (current-bindings))
            (failure))))

    (define-predicate (atomic term)
      (let ((v (substitute-bindings (current-bindings) term)))
        (if (and (not (variable? v)) (not (pair? v)))
            (prove-all (current-goals) (current-bindings))
            (failure))))

    (define-predicate (var term)
      (if (variable? (resolve-binding (current-bindings) term))
          (prove-all (current-goals) (current-bindings))
          (failure)))

    (define-predicate (ground term)
      (if (ground? (current-bindings) term)
          (prove-all (current-goals) (current-bindings))
          (failure)))

    (define-predicate (number term)
      (let ((v (substitute-bindings (current-bindings) term)))
        (if (number? v)
            (prove-all (current-goals) (current-bindings))
            (failure))))

    (define-predicate (dynamic-put var-symbol value-form)
      (let* ((param (get-dynamic-parameter var-symbol))
             (bindings (current-bindings))
             (substituted-form (substitute-bindings bindings value-form))
             (evaluated-value (eval substituted-form (current-lisp-environment))))
        (param evaluated-value)
        (prove-all (current-goals) bindings)))

    (define-predicate (dynamic-get var-symbol prolog-var)
      (let* ((param (get-dynamic-parameter var-symbol))
             (value (param))
             (new-bindings (unify prolog-var value (current-bindings))))
        (prove-all (current-goals) new-bindings)))

    (<-- (lisp ?result ?expression) (lisp-eval-internal ?result ?expression))
    (<-- (is ?result ?expression) (lisp-eval-internal ?result ?expression))
    (<-- (lisp ?expression) (lisp-eval-internal ? ?expression))

    (define solutions-accumulator (make-parameter (list)))
    (define-predicate (add-solution-and-fail template)
      (solutions-accumulator
       (cons (substitute-bindings (current-bindings) template)
             (solutions-accumulator)))
      (failure))

    (define-predicate (bagof template goal result-bag)
      (parameterize ((solutions-accumulator (list)))
        (reset (prove-all (list goal (list 'add-solution-and-fail template) 'fail) (current-bindings)))
        (prove-all (current-goals) (unify result-bag (reverse (solutions-accumulator)) (current-bindings)))))

    (define-predicate (setof template goal result-set)
      (parameterize ((solutions-accumulator (list)))
        (reset (prove-all (list goal (list 'add-solution-and-fail template) 'fail) (current-bindings)))
        (let* ((sorter (lambda (a b) (string<? (object->string a) (object->string b))))
               (values (list-sort sorter (delete-duplicates (reverse (solutions-accumulator)) equal?)))
               (new-bindings (unify result-set values (current-bindings))))
          (prove-all (current-goals) new-bindings))))

    (<-- (or ?goal-a ?goal-b) (call ?goal-a))
    (<- (or ?goal-a ?goal-b) (call ?goal-b))
    (<-- (member ?item (?item . ?)))
    (<- (member ?item (? . ?rest)) (member ?item ?rest))
    (<-- (append () ?list ?list))
    (<- (append (?h . ?t) ?list2 (?h . ?result)) (append ?t ?list2 ?result))
    (<-- (repeat))
    (<- (repeat) (repeat))
    (<-- (true))
    (<-- (if ?cond ?then ?else) (call ?cond) (cut) (call ?then))
    (<- (if ?cond ?then ?else) (call ?else))
    (<-- (if ?cond ?then) (call ?cond) (call ?then))
    (<-- (not ?goal) (call ?goal) (cut) (fail))
    (<- (not ?goal))
  )
)
