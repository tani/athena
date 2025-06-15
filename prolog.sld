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
    failure? success? failure success
    *failure-object* *empty-bindings*
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

    ;; Convenience wrappers ---------------------------------------------------
    (define (failure) (make-failure))
    (define (success b k) (make-success b k))
    (define *failure-object* (failure))      ; kept for backward‑compat tests

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
    (define (object->string x)
      (parameterize ((current-output-port (open-output-string)))
        (write x)
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
            (let ((ps (*prompt-stack*)))
              (if (null? ps)
                  (error "shift: no enclosing reset")
                  (let ((prompt (car ps)))
                    (parameterize ((*prompt-stack* (cdr ps)))
                      (prompt
                       (let ((k (lambda (v) (escape v))))
                         body ...)))))))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 4. Bindings and unification
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define *empty-bindings* '((#t . #t)))

    ;; predicates -------------------------------------------------------------
    (define (variable? x)
      (and (symbol? x)
           (let ((s (symbol->string x)))
             (and (> (string-length s) 0)
                  (char=? (string-ref s 0) #\?)))))

    (define (named-variable? x) (and (variable? x) (not (eq? x '?))))
    (define (atom? x) (not (pair? x)))

    ;; binding helpers --------------------------------------------------------
    (define (get-binding v bs) (assoc v bs))
    (define (lookup-variable v bs) (cdr (get-binding v bs)))

    (define (extend-bindings v val bs)
      (cons (cons v val) (if (eq? bs *empty-bindings*) '() bs)))

    (define (substitute-bindings bs expr)
      (cond ((failure? bs) (failure))
            ((eq? bs *empty-bindings*) expr)
            ((and (variable? expr) (get-binding expr bs))
             (substitute-bindings bs (lookup-variable expr bs)))
            ((atom? expr) expr)
            (else (cons (substitute-bindings bs (car expr))
                        (substitute-bindings bs (cdr expr))))))

    ;; variable collection ----------------------------------------------------
    (define (collect-unique-if pred tree)
      (let loop ((t tree) (acc '()))
        (if (atom? t)
            (if (and (pred t) (not (memq t acc))) (cons t acc) acc)
            (loop (cdr t) (loop (car t) acc)))))

    (define (variables-in expr) (reverse (collect-unique-if named-variable? expr)))

    (define (replace-anonymous-variables expr)
      (cond ((eq? expr '?) (gensym "?"))
            ((atom? expr) expr)
            (else (cons (replace-anonymous-variables (car expr))
                        (replace-anonymous-variables (cdr expr))))))

    ;; unify ------------------------------------------------------------------
    (define *occurs-check* (make-parameter #t))

    (define (occurs-check? v expr bs)
      (cond ((eq? v expr) #t)
            ((and (variable? expr) (get-binding expr bs))
             (occurs-check? v (lookup-variable expr bs) bs))
            ((pair? expr)
             (or (occurs-check? v (car expr) bs)
                 (occurs-check? v (cdr expr) bs)))
            (else #f)))

    (define (unify-var v value bs)
      (cond ((get-binding v bs) (unify (lookup-variable v bs) value bs))
            ((and (variable? value) (get-binding value bs))
             (unify v (lookup-variable value bs) bs))
            ((and (*occurs-check*) (occurs-check? v value bs)) (failure))
            (else (extend-bindings v value bs))))

    (define (unify a b bs)
      (cond ((failure? bs) (failure))
            ((equal? a b) bs)
            ((variable? a) (unify-var a b bs))
            ((variable? b) (unify-var b a bs))
            ((and (pair? a) (pair? b))
             (unify (cdr a) (cdr b) (unify (car a) (car b) bs)))
            (else (failure))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5. Clause database
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define clause-database (make-parameter '()))

    (define (get-clauses pred)
      (let ((e (assoc pred (clause-database)))) (if e (cdr e) '())))

    (define (set-clauses! pred clauses)
      (clause-database (cons (cons pred clauses)
                             (alist-delete pred (clause-database) eq?))))

    (define (add-clause! clause)
      (let ((pred (caar clause)))
        (set-clauses! pred (append (get-clauses pred) (list clause)))))

    ;; syntactic sugar --------------------------------------------------------
    (define-syntax <-
      (syntax-rules () ((_ head . body)
                        (add-clause! (replace-anonymous-variables '(head . body))))))

    (define (remove-clauses-with-arity! pred arity)
      (set-clauses! pred (filter (lambda (c) (not (= (length (cdar c)) arity)))
                                 (get-clauses pred))))

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
    (define (sublis al tree)
      (if (atom? tree)
          (let ((b (assoc tree al))) (if b (cdr b) tree))
          (cons (sublis al (car tree)) (sublis al (cdr tree)))))

    (define (rename-vars expr)
      (let* ((vs (variables-in expr))
             (al (map (lambda (v) (cons v (gensym (symbol->string v)))) vs)))
        (sublis al expr)))

    ;; core recursion ---------------------------------------------------------
    (define (process-one goal clause bs rest)
      (let* ((cl      (rename-vars clause))
             (head    (car cl))
             (body    (cdr cl))
             (new-bs  (unify goal head bs)))
        (prove-all (append body rest) new-bs)))

    (define (combine a b)
      (lambda ()
        (let ((ra (and a (a))))
          (if (or (not ra) (failure? ra)) (and b (b)) ra))))

    (define (try-clauses goal bs rest clauses)
      (if (null? clauses)
          (failure)
          (let* ((cl  (car clauses))
                 (rem (cdr clauses))
                 (res (process-one goal cl bs rest))
                 (next (lambda () (try-clauses goal bs rest rem))))
            (if (failure? res)
                (next)
                (success (success-bindings res)
                         (combine (success-continuation res) next))))))

    (define (prove goal bs rest)
      (parameterize ((current-goals rest) (current-bindings bs))
        (let* ((pred (if (pair? goal) (car goal) goal))
               (cls  (get-clauses pred)))
          (if (procedure? cls)
              (apply cls (if (pair? goal) (cdr goal) '()))
              (reset (try-clauses goal bs rest cls))))))

    (define (prove-all goals bs)
      (cond ((failure? bs) (failure))
            ((null? goals) (success bs #f))
            (else (prove (car goals) bs (cdr goals)))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 7. Interactive query syntax (?-)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax ?-
      (syntax-rules () ((_ . goals)
                        (run-query (replace-anonymous-variables 'goals)))))

    (define (display-solution vars bs)
      (if (null? vars)
          (display "Yes.")
          (for-each (lambda (v) (display v) (display " = ")
                      (write (substitute-bindings bs v)) (newline)) vars)))

    (define (continue-prompt?)
      (display " ") (flush-output-port)
      (case (read-char)
        ((#\;) #t) ((#\.) #f) ((#\newline) (continue-prompt?))
        (else (display " Type ; for more, or . to stop.") (newline) (continue-prompt?))))

    (define (run-query goals)
      (let ((vars (variables-in goals)))
        (reset (let loop ((k (lambda () (prove-all goals *empty-bindings*))))
                 (let ((r (k)))
                   (if (failure? r)
                       (begin (display "No.") (newline) (newline))
                       (begin (display-solution vars (success-bindings r))
                              (newline)
                              (when (and (success-continuation r) (continue-prompt?))
                                (loop (success-continuation r))))))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 8. Macro for defining pure Scheme predicates
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax define-predicate
      (syntax-rules () ((_ (name . args) . body)
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
