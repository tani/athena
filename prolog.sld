;; prolog.scm — Mini‑Prolog engine with R7RS records
;; ------------------------------------------------------------
;; Copyright © 2025 Masaya Taniguchi
;; Released under the GNU General Public License v3.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Library declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-library (prolog)
  ;; Public symbols -----------------------------------------------------------
  (export
    ;; basic helpers
    variable? named-variable? atom?
    failure? success?
    *empty-bindings*
    extend-bindings substitute-bindings variables-in
    replace-anonymous-variables unify object->string
    ;; delimited continuations
    reset shift *prompt-stack*
    ;; clause DB API
    clause-database add-clause! get-clauses <- <-- define-predicate
    ;; prover / query
    prove-all ?-
    ;; accessors for <success>
    success-bindings success-continuation)

  ;; Imports ------------------------------------------------------------------
  (import (scheme base)
          (scheme eval)
          (scheme write)
          (only (srfi 1) alist-delete filter delete-duplicates))

  ;; Optional SRFI‑132 list-sort for each implementation
  (cond-expand
   (chicken     (import scheme (only (srfi 132) list-sort)))
   (guile       (import (only (rnrs sorting (6)) list-sort)))
   (gauche      (import (only (scheme sort) list-sort)))
   (chibi       (import (only (scheme sort) list-sort)))
   (gambit      (import (only (srfi 132) list-sort)))
   (sagittarius (import (only (scheme sort) list-sort))))

  ;; Implementation -----------------------------------------------------------
  (begin

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 1. Records for success / failure objects
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-record-type <failure>
      (make-failure)
      failure?)

    (define-record-type <success>
      (make-success bindings continuation)
      success?
      (bindings     success-bindings)
      (continuation success-continuation))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 2. Utility procedures
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; gensym -----------------------------------------------------------------
    (define gensym-counter 0)
    (define (gensym prefix)
      (let ((name (string-append prefix (number->string gensym-counter))))
        (set! gensym-counter (+ gensym-counter 1))
        (string->symbol name)))

    ;; object->string ---------------------------------------------------------
    (define (object->string object)
      (parameterize ((current-output-port (open-output-string)))
        (write object)
        (get-output-string (current-output-port))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 3. reset / shift
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define *prompt-stack* (make-parameter '()))

    (define-syntax reset
      (syntax-rules ()
        ((_ body ...)
         (call/cc
          (lambda (prompt)
            (parameterize ((*prompt-stack* (cons prompt (*prompt-stack*))))
              body ...))))))

    (define-syntax shift
      (syntax-rules ()
        ((_ k body ...)
         (call/cc
          (lambda (escape)
            (let ((prompt-stack (*prompt-stack*)))
              (if (null? prompt-stack)
                  (error "shift: no enclosing reset")
                  (let ((prompt (car prompt-stack)))
                    (parameterize ((*prompt-stack* (cdr prompt-stack)))
                      (prompt
                       (let ((k (lambda (value) (escape value))))
                         body ...)))))))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 4. Bindings and unification
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define *empty-bindings* '((#t . #t)))

    ;; predicates -------------------------------------------------------------
    (define (variable? term)
      (and (symbol? term)
           (let ((symbol-string (symbol->string term)))
             (and (> (string-length symbol-string) 0)
                  (char=? (string-ref symbol-string 0) #\?)))))

    (define (named-variable? term) (and (variable? term) (not (eq? term '?))))
    (define (atom? term) (not (pair? term)))

    ;; binding helpers --------------------------------------------------------
    (define (get-binding variable bindings) (assoc variable bindings))
    (define (lookup-variable variable bindings) (cdr (get-binding variable bindings)))

    (define (extend-bindings variable value bindings)
      (cons (cons variable value) (if (eq? bindings *empty-bindings*) '() bindings)))

    (define (substitute-bindings bindings expression)
      (cond ((failure? bindings) (make-failure))
            ((eq? bindings *empty-bindings*) expression)
            ((and (variable? expression) (get-binding expression bindings))
             (substitute-bindings bindings (lookup-variable expression bindings)))
            ((atom? expression) expression)
            (else (cons (substitute-bindings bindings (car expression))
                        (substitute-bindings bindings (cdr expression))))))

    ;; variable collection ----------------------------------------------------
    (define (collect-unique-if predicate tree)
      (let loop ((subtree tree) (accumulator '()))
        (if (atom? subtree)
            (if (and (predicate subtree) (not (memq subtree accumulator)))
                (cons subtree accumulator)
                accumulator)
            (loop (cdr subtree) (loop (car subtree) accumulator)))))

    (define (variables-in expression) (reverse (collect-unique-if named-variable? expression)))

    (define (replace-anonymous-variables expression)
      (cond ((eq? expression '?) (gensym "?"))
            ((atom? expression) expression)
            (else (cons (replace-anonymous-variables (car expression))
                        (replace-anonymous-variables (cdr expression))))))

    ;; unify ------------------------------------------------------------------
    (define *occurs-check* (make-parameter #t))

    (define (occurs-check? variable expression bindings)
      (cond ((eq? variable expression) #t)
            ((and (variable? expression) (get-binding expression bindings))
             (occurs-check? variable (lookup-variable expression bindings) bindings))
            ((pair? expression)
             (or (occurs-check? variable (car expression) bindings)
                 (occurs-check? variable (cdr expression) bindings)))
            (else #f)))

    (define (unify-var variable value bindings)
      (cond ((get-binding variable bindings) (unify (lookup-variable variable bindings) value bindings))
            ((and (variable? value) (get-binding value bindings))
             (unify variable (lookup-variable value bindings) bindings))
            ((and (*occurs-check*) (occurs-check? variable value bindings)) (make-failure))
            (else (extend-bindings variable value bindings))))

    (define (unify term1 term2 bindings)
      (cond ((failure? bindings) (make-failure))
            ((equal? term1 term2) bindings)
            ((variable? term1) (unify-var term1 term2 bindings))
            ((variable? term2) (unify-var term2 term1 bindings))
            ((and (pair? term1) (pair? term2))
             (unify (cdr term1) (cdr term2) (unify (car term1) (car term2) bindings)))
            (else (make-failure))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5. Clause database
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define clause-database (make-parameter '()))

    (define (get-clauses predicate-symbol)
      (let ((entry (assoc predicate-symbol (clause-database)))) (if entry (cdr entry) '())))

    (define (set-clauses! predicate-symbol clauses)
      (clause-database (cons (cons predicate-symbol clauses)
                             (alist-delete predicate-symbol (clause-database) eq?))))

    (define (add-clause! clause)
      (let ((predicate-symbol (caar clause)))
        (set-clauses! predicate-symbol (append (get-clauses predicate-symbol) (list clause)))))

    ;; syntactic sugar --------------------------------------------------------
    (define-syntax <-
      (syntax-rules () ((_ head . body)
                        (add-clause! (replace-anonymous-variables '(head . body))))))

    (define (remove-clauses-with-arity! predicate-symbol arity)
      (set-clauses! predicate-symbol (filter (lambda (clause) (not (= (length (cdar clause)) arity)))
                                             (get-clauses predicate-symbol))))

    (define-syntax <--
      (syntax-rules () ((_ head . body)
                        (begin (remove-clauses-with-arity! (car 'head) (length (cdr 'head)))
                               (add-clause! (replace-anonymous-variables '(head . body)))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 6. Prover engine
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define current-bindings (make-parameter *empty-bindings*))
    (define current-goals    (make-parameter '()))

    ;; variable‑renaming ------------------------------------------------------
    (define (sublis alist tree)
      (if (atom? tree)
          (let ((binding (assoc tree alist))) (if binding (cdr binding) tree))
          (cons (sublis alist (car tree)) (sublis alist (cdr tree)))))

    (define (rename-vars expression)
      (let* ((variables (variables-in expression))
             (alist (map (lambda (variable) (cons variable (gensym (symbol->string variable)))) variables)))
        (sublis alist expression)))

    ;; core recursion ---------------------------------------------------------
    (define (process-one goal clause bindings remaining-goals)
      (let* ((renamed-clause (rename-vars clause))
             (head           (car renamed-clause))
             (clause-body    (cdr renamed-clause))
             (new-bindings   (unify goal head bindings)))
        (prove-all (append clause-body remaining-goals) new-bindings)))

    (define (combine continuation-a continuation-b)
      (lambda ()
        (let ((result-a (and continuation-a (continuation-a))))
          (if (or (not result-a) (failure? result-a))
              (and continuation-b (continuation-b))
              result-a))))

    (define (try-clauses goal bindings remaining-goals clauses)
      (if (null? clauses)
          (make-failure)
          (let* ((current-clause   (car clauses))
                 (remaining-clauses (cdr clauses))
                 (result            (process-one goal current-clause bindings remaining-goals))
                 (next-continuation (lambda () (try-clauses goal bindings remaining-goals remaining-clauses))))
            (if (failure? result)
                (next-continuation)
                (make-success
                  (success-bindings result)
                  (combine (success-continuation result) next-continuation))))))

    (define (prove goal bindings remaining-goals)
      (parameterize ((current-goals remaining-goals) (current-bindings bindings))
        (let* ((predicate-symbol (if (pair? goal) (car goal) goal))
               (clauses          (get-clauses predicate-symbol)))
          (if (procedure? clauses)
              (apply clauses (if (pair? goal) (cdr goal) '()))
              (reset (try-clauses goal bindings remaining-goals clauses))))))

    (define (prove-all goals bindings)
      (cond ((failure? bindings) (make-failure))
            ((null? goals) (make-success bindings #f))
            (else (prove (car goals) bindings (cdr goals)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 7. Interactive query syntax (?-)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax ?-
      (syntax-rules () ((_ . goals)
                        (run-query (replace-anonymous-variables 'goals)))))

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
      (display " ") (flush-output-port)
      (case (read-char)
        ((#\;) #t)
        ((#\.) #f)
        ((#\newline) (continue-prompt?))
        (else (display " Type ; for more, or . to stop.") (newline) (continue-prompt?))))

    (define (run-query goals)
      (let ((variables (variables-in goals)))
        (reset (let loop ((continuation (lambda () (prove-all goals *empty-bindings*))))
                 (let ((result (continuation)))
                   (if (failure? result)
                       (begin (display "No.") (newline) (newline))
                       (begin (display-solution variables (success-bindings result))
                              (newline)
                              (when (and (success-continuation result) (continue-prompt?))
                                (loop (success-continuation result))))))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 8. Macro for defining pure Scheme predicates
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax define-predicate
      (syntax-rules () ((_ (name . arguments) . body)
                        (set-clauses! 'name (lambda arguments . body)))))

    (define (resolve-binding bindings term)
      (let ((resolved-value (substitute-bindings bindings term)))
        (if (variable? resolved-value)
            resolved-value
            (substitute-bindings bindings resolved-value))))

    (define (ground? bindings term)
      (let ((resolved-term (resolve-binding bindings term)))
        (cond ((variable? resolved-term) #f)
              ((pair? resolved-term)
               (and (ground? bindings (car resolved-term))
                    (ground? bindings (cdr resolved-term))))
              (else #t))))

    (define *dynamic-parameters* (make-parameter (list)))

    (define (get-dynamic-parameter variable-symbol)
      (let* ((alist (*dynamic-parameters*))
             (entry (assoc variable-symbol alist)))
        (if entry
            (cdr entry)
            (let ((new-parameter (make-parameter #f)))
              (*dynamic-parameters* (cons (cons variable-symbol new-parameter) alist))
              new-parameter))))

    (define-predicate (fail) (make-failure))

    (define-predicate (cut) (shift k (prove-all (current-goals) (current-bindings))))

    (define-predicate (= term1 term2)
      (prove-all (current-goals) (unify term1 term2 (current-bindings))))

    (define-predicate (== term1 term2)
      (if (equal? (substitute-bindings (current-bindings) term1)
                  (substitute-bindings (current-bindings) term2))
          (prove-all (current-goals) (current-bindings))
          (make-failure)))

    (define-predicate (call goal)
      (prove-all (cons (substitute-bindings (current-bindings) goal) (current-goals))
                 (current-bindings)))

    (cond-expand
      (gambit (define (environment x) 5))
      (else))

    (define current-lisp-environment (make-parameter (environment '(scheme base))))
    (define-predicate (lisp-eval-internal result-variable expression)
      (let* ((context-bindings (current-bindings))
             (scheme-expression (substitute-bindings context-bindings expression))
             (evaluated-result (eval scheme-expression (current-lisp-environment)))
             (result-term (substitute-bindings context-bindings result-variable))
             (new-bindings (unify result-term evaluated-result context-bindings)))
        (prove-all (current-goals) new-bindings)))

    (define-predicate (atom term)
      (let ((value (substitute-bindings (current-bindings) term)))
        (if (and (symbol? value) (not (variable? value)))
            (prove-all (current-goals) (current-bindings))
            (make-failure))))

    (define-predicate (atomic term)
      (let ((value (substitute-bindings (current-bindings) term)))
        (if (and (not (variable? value)) (not (pair? value)))
            (prove-all (current-goals) (current-bindings))
            (make-failure))))

    (define-predicate (var term)
      (if (variable? (resolve-binding (current-bindings) term))
          (prove-all (current-goals) (current-bindings))
          (make-failure)))

    (define-predicate (ground term)
      (if (ground? (current-bindings) term)
          (prove-all (current-goals) (current-bindings))
          (make-failure)))

    (define-predicate (number term)
      (let ((value (substitute-bindings (current-bindings) term)))
        (if (number? value)
            (prove-all (current-goals) (current-bindings))
            (make-failure))))

    (define-predicate (dynamic-put variable-symbol value-expression)
      (let* ((parameter (get-dynamic-parameter variable-symbol))
             (bindings (current-bindings))
             (substituted-expression (substitute-bindings bindings value-expression))
             (evaluated-value (eval substituted-expression (current-lisp-environment))))
        (parameter evaluated-value)
        (prove-all (current-goals) bindings)))

    (define-predicate (dynamic-get variable-symbol prolog-variable)
      (let* ((parameter (get-dynamic-parameter variable-symbol))
             (value (parameter))
             (new-bindings (unify prolog-variable value (current-bindings))))
        (prove-all (current-goals) new-bindings)))

    (<-- (lisp ?result ?expression) (lisp-eval-internal ?result ?expression))
    (<-- (is ?result ?expression) (lisp-eval-internal ?result ?expression))
    (<-- (lisp ?expression) (lisp-eval-internal ? ?expression))

    (define solutions-accumulator (make-parameter (list)))
    (define-predicate (add-solution-and-fail template)
      (solutions-accumulator
       (cons (substitute-bindings (current-bindings) template)
             (solutions-accumulator)))
      (make-failure))

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
