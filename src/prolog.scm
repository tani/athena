;; prolog.scm — Prolog engine core implementation (highly commonized)
;; Copyright © 2025 Masaya Taniguchi
;; Released under the GNU General Public License v3.0
;;
;; This file is intended to be included by a version-specific (R6RS/R7RS) wrapper.

(begin

  ;; Utility procedures
  (define-syntax define*
    (syntax-rules ()
      ((define* (name . args) body ...)
       (define (name . args)
         (display (quote name))
         (display " <= ")
         (write (list . args))
         (newline)
         (let ((result (begin body ...)))
           (display (quote name))
           (display " => ")
           (write result)
           (newline)
           result)))))

  (define %gensym
    (let ((counter 0))
      (lambda (prefix)
        (let* ((counter-string (number->string counter))
               (name (string-append prefix counter-string)))
          (set! counter (+ counter 1))
          (string->symbol name)))))

  (define (call-with-current-choice-point proc)
    (let ((tag (%gensym "choice-point-")))
      (guard (exception
              ((and (cut-exception? exception)
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
     ((eq? expression '?) (%gensym "?"))
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

  (define primitive-clause-database (make-parameter '()))

  (define standard-clause-database (make-parameter '()))

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
      (cons variable (%gensym var-string))))

  (define (rename-vars expression)
    (let* ((variables (variables-in expression))
           (alist (map make-renaming-pair variables)))
      (sublis alist expression)))

  ;; Spy support

  (define current-spy-mode
    ;; Possible values: 'prompt, 'always, or 'disabled
    (make-parameter 'prompt))

  (define current-spy-predicates (make-parameter '()))
  (define current-spy-depth (make-parameter 0))
  (define current-spy-indent? (make-parameter #t))

  (define (simple-repl)
    (display "Entering break; type 'continue to resume")
    (newline)
    (let loop ()
      (display "debug> ")
      (flush-output-port (current-output-port))
      (let ((form (read)))
        (unless (eq? form 'continue)
          (let ((result (eval form (current-lisp-environment))))
            (write result)
            (newline))
          (loop)))))

  (define (spy-prompt goal bindings)
    (let ((resolved (substitute-bindings bindings goal)))
      (display "Spy on ")
      (write resolved)
      (display "? [l=leap c=creep n=nodebug b=break] ")
      (flush-output-port (current-output-port))
      (case (read)
        ((l) (current-spy-mode 'always) #t)
        ((c) #t)
        ((n) (current-spy-mode 'disabled) #f)
        ((b) (simple-repl) #t)
        (else #t))))

  (define (spy-message kind goal bindings)
    (let ((resolved (substitute-bindings bindings goal)))
      (when (current-spy-indent?)
        (do ((i 0 (+ i 1)))
            ((= i (current-spy-depth)))
          (display " ")))
      (display kind)
      (display ": ")
      (write resolved)
      (newline)))

  (define (with-spy goal bindings thunk)
    (let* ((predicate-symbol (if (pair? goal) (car goal) goal))
           (spy? (memq predicate-symbol (current-spy-predicates)))
           (mode (current-spy-mode))
           (show? (and spy?
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
          (lambda ()
            (parameterize ((current-spy-depth (+ (current-spy-depth) 1)))
              (set! result (thunk))
              result))
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
                  (if (and (not (null? predicate-handler))
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
           (and (>= goal-arity required)
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

  ;; Interactive query syntax (?-)

  (define-syntax ?-
    (syntax-rules ()
      ((_ . goals)
       (run-query (replace-anonymous-variables 'goals)))))

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
    (let loop ((ss (make-solution-stream goals)))
      (if (stream-null? ss)
          (begin (display "No.") (newline))
          (begin
            (display-solution (stream-car ss))
            (when (continue-prompt?)
              (loop (stream-cdr ss)))))))

  ;; Macro for defining pure Scheme predicates

  (define-syntax define-predicate
    (syntax-rules ()
      ((_ (name . arguments) . body)
       (set-clauses! 'name (lambda arguments . body)))))

  (define (ground? term)
    (let ((resolved-term (substitute-bindings (current-bindings)  term)))
      (cond
       ((variable? resolved-term) #f)
       ((pair? resolved-term)
        (let ((car-ground? (ground? (car resolved-term)))
              (cdr-ground? (ground? (cdr resolved-term))))
          (and car-ground? cdr-ground?)))
       (else #t))))

  (define current-dynamic-parameters (make-parameter '()))

  (define (get-dynamic-parameter variable-symbol)
    (let ((entry (assoc variable-symbol (current-dynamic-parameters))))
      (if entry
          (cdr entry)
          (let* ((new-parameter (make-parameter #f))
                 (new-dynamic-parameters (alist-cons variable-symbol new-parameter (current-dynamic-parameters))))
            (current-dynamic-parameters new-dynamic-parameters)
            new-parameter))))

  (define-stream (make-solution-stream goals)
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
    (stream-unfold
      retrieve-success-bindings
      success?
      execute-success-continuation
      (initial-continuation)))

  (define current-solution-accumulator (make-parameter '()))

  (define current-lisp-environment (make-parameter (interaction-environment)))

  (define-predicate (= term1 term2)
    (let ((new-bindings (unify term1 term2 (current-bindings))))
      (prove-all (current-remaining-goals) new-bindings)))

  (define-predicate (== term1 term2)
    (let* ((substituted-term1 (substitute-bindings (current-bindings) term1))
           (substituted-term2 (substitute-bindings (current-bindings) term2)))
      (if (equal? substituted-term1 substituted-term2)
          (let ((goals (current-remaining-goals)))
            (prove-all goals (current-bindings)))
          (make-failure))))

  (define-predicate (! choice-point)
    (raise
     (make-cut-exception
      choice-point
      (prove-all (current-remaining-goals) (current-bindings)))))

  (define-predicate (call pred-or-goal . args)
    (call-with-current-choice-point
      (lambda (choice-point)
        (let* ((goal (cond
                      ((null? args) pred-or-goal)
                      ((symbol? pred-or-goal) (cons pred-or-goal args))
                      ((pair? pred-or-goal) (append pred-or-goal args))
                      (else (error "call: invalid form" (cons pred-or-goal args)))))
               (substituted-goal (substitute-bindings (current-bindings) goal))
               (cut-goals (insert-choice-point (list substituted-goal) choice-point))
               (next-goals (append cut-goals (current-remaining-goals))))
          (prove-all next-goals (current-bindings))))))

  (define-predicate (--lisp-eval-internal result-variable expression)
    (let* ((scheme-expression (substitute-bindings (current-bindings) expression))
           (evaluated-result (eval scheme-expression (current-lisp-environment)))
           (result-term (substitute-bindings (current-bindings) result-variable))
           (new-bindings (unify result-term evaluated-result (current-bindings))))
      (prove-all (current-remaining-goals) new-bindings)))

  (define-predicate (atom term)
    (let ((value (substitute-bindings (current-bindings) term)))
      (if (and (symbol? value) (not (variable? value)))
          (prove-all (current-remaining-goals) (current-bindings))
          (make-failure))))

  (define-predicate (atomic term)
    (let ((value (substitute-bindings (current-bindings) term)))
      (if (and (not (variable? value)) (not (pair? value)))
          (prove-all (current-remaining-goals) (current-bindings))
          (make-failure))))

  (define-predicate (var term)
    (if (variable? (substitute-bindings (current-bindings) term))
        (prove-all (current-remaining-goals) (current-bindings))
        (make-failure)))

  (define-predicate (ground term)
    (if (ground? term)
        (prove-all (current-remaining-goals) (current-bindings))
        (make-failure)))

  (define-predicate (number term)
    (let ((value (substitute-bindings (current-bindings) term)))
      (if (number? value)
          (prove-all (current-remaining-goals) (current-bindings))
          (make-failure))))

  (define-predicate (dynamic-put variable-symbol value-expression)
    (let* ((parameter (get-dynamic-parameter variable-symbol))
           (substituted-expression (substitute-bindings (current-bindings) value-expression))
           (evaluated-value (eval substituted-expression (current-lisp-environment))))
      (parameter evaluated-value)
      (prove-all (current-remaining-goals) (current-bindings))))

  (define-predicate (dynamic-get variable-symbol prolog-variable)
    (let* ((parameter (get-dynamic-parameter variable-symbol))
           (new-bindings (unify prolog-variable (parameter) (current-bindings))))
      (prove-all (current-remaining-goals) new-bindings)))

  (define-predicate (--add-solution-with-vars-and-fail template . vars)
    (let* ((substituted-template (substitute-bindings (current-bindings) template))
           (substituted-vars (map (lambda (v) (substitute-bindings (current-bindings) v)) vars))
           (entry (cons substituted-vars substituted-template))
           (new-solutions (cons entry (current-solution-accumulator))))
      (current-solution-accumulator new-solutions))
    (make-failure))

  (define-predicate (bagof template goal result-bag)
    (define (group-solutions pairs)
      (let loop ((remaining pairs) (acc '()))
        (if (null? remaining)
            (reverse acc)
            (let* ((current-pair (car remaining))
                   (key (car current-pair))
                   (template (cdr current-pair))
                   (existing-group (assoc key acc)))
              (if existing-group
                  (let* ((updated-group (cons key (append (cdr existing-group) (list template))))
                         (acc-without-old (alist-delete key acc equal?)))
                    (loop (cdr remaining) (cons updated-group acc-without-old)))
                  (loop (cdr remaining) (alist-cons key (list template) acc)))))))
    (define (enumerate-groups grouped-solutions free-vars result-bag)
      (if (null? grouped-solutions)
          (make-failure)
          (let* ((current-group (car grouped-solutions))
                 (free-var-values (car current-group))
                 (bag-of-templates (cdr current-group))
                 (next-group-thunk
                  (lambda ()
                    (enumerate-groups (cdr grouped-solutions) free-vars result-bag)))
                 (b1 (unify free-vars free-var-values (current-bindings)))
                 (b2 (unify result-bag bag-of-templates b1)))
             (if (failure? b2)
                 (next-group-thunk)
                 (let* ((proof-stream (prove-all (current-remaining-goals) b2))
                        (continuation (success-continuation proof-stream))
                        (new-continuation (combine continuation next-group-thunk)))
                   (make-success (success-bindings proof-stream) new-continuation))))))
    (let* ((substituted-goal (substitute-bindings (current-bindings) goal))
           (goal-vars (variables-in substituted-goal))
           (template-vars (variables-in (substitute-bindings (current-bindings) template)))
           (free-vars (filter (lambda (v) (not (memq v template-vars))) goal-vars))
           (collector-goal `(--add-solution-with-vars-and-fail ,template ,@free-vars))
           (all-solutions
             (parameterize ((current-solution-accumulator '()))
               (prove-all `(,substituted-goal ,collector-goal fail) (current-bindings))
               (reverse (current-solution-accumulator))))
           (grouped-solutions (group-solutions all-solutions)))
      (enumerate-groups grouped-solutions free-vars result-bag)))

  (define-predicate (sort unsorted-list result-list)
    (define (object<? a b)
      (let ((sa (object->string a))
            (sb (object->string b)))
        (string<? sa sb)))
    (let* ((actual-list (substitute-bindings (current-bindings) unsorted-list))
           (unique-list (delete-duplicates actual-list equal?))
           (sorted-list (list-sort object<? unique-list))
           (new-bindings (unify result-list sorted-list (current-bindings)))
           (goals (current-remaining-goals)))
      (prove-all goals new-bindings)))

  (define-predicate (--add-solution-and-fail template)
    (let* ((substituted-template (substitute-bindings (current-bindings) template))
           (new-solutions (cons substituted-template (current-solution-accumulator))))
      (current-solution-accumulator new-solutions))
    (make-failure))

  (define-predicate (findall template goal result-list)
    (parameterize ((current-solution-accumulator '()))
      (let ((new-goals (list goal (list '--add-solution-and-fail template) 'fail)))
        (prove-all new-goals (current-bindings)))
      (let* ((reversed-solutions (reverse (current-solution-accumulator)))
             (new-bindings (unify result-list reversed-solutions (current-bindings)))
             (goals (current-remaining-goals)))
        (prove-all goals new-bindings))))

  (define-predicate (fail)
    (make-failure))

  (primitive-clause-database (current-clause-database))

  (<-- true)

  (<- and true)
  (<- (and ?g . ?gs) (call ?g) (call (and . ?gs)))

  (<- or fail)
  (<- (or ?g . ?gs) (call ?g))
  (<- (or ?g . ?gs) (call (or . ?gs)))

  (<- (not ?goal) (call ?goal) ! (fail))
  (<- (not ?goal))

  (<- (if ?cond ?then ?else) (call ?cond) ! (call ?then))
  (<- (if ?cond ?then ?else) (call ?else))
  (<- (if ?cond ?then) (call ?cond) (call ?then))

  (<- (lisp ?result ?expression) (--lisp-eval-internal ?result ?expression))
  (<- (lisp ?expression) (--lisp-eval-internal ? ?expression))

  (<- (is ?result ?expression) (--lisp-eval-internal ?result ?expression))

  (<- (repeat))
  (<- (repeat) (repeat))

  (<- (member ?item (?item . ?)))
  (<- (member ?item (? . ?rest)) (member ?item ?rest))

  (<- (append () ?list ?list))
  (<- (append (?head . ?tail) ?list (?head . ?result))
      (append ?tail ?list ?result))

  (<- (--all-null ()))
  (<- (--all-null (() . ?rest)) (--all-null ?rest))

  (<- (--get-heads () ()))
  (<- (--get-heads ((?h . ?t) . ?rest-lists) (?h . ?rest-heads))
      (--get-heads ?rest-lists ?rest-heads))

  (<- (--get-tails () ()))
  (<- (--get-tails ((?h . ?t) . ?rest-lists) (?t . ?rest-tails))
      (--get-tails ?rest-lists ?rest-tails))

  (<- (maplist ?pred . ?lists)
      (if (--all-null ?lists)
          true
          (and
           (--get-heads ?lists ?heads)
           (--get-tails ?lists ?tails)
           (call (?pred . ?heads))
           (call (maplist ?pred . ?tails)))))

  (<-- (setof ?template ?goal ?result-set)
       (bagof ?template ?goal ?result-bag)
       (sort ?result-bag ?result-set))

  (standard-clause-database (current-clause-database)))

