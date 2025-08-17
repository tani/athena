;; prolog-primitive.scm — Prolog built-in predicates
;; Copyright © 2025 Masaya Taniguchi
;; Released under the GNU General Public License v3.0
;;
;; This file contains the core built-in predicates for the Prolog engine:
;; - Basic unification and comparison predicates
;; - Type checking predicates
;; - Evaluation predicates
;; - Meta-predicates (call, !)
;; - Dynamic parameter predicates

(begin
  (define current-lisp-environment (make-parameter (interaction-environment)))
  (define current-dynamic-parameters (make-parameter '()))

  (define (object->string object)
    (let ((p (open-output-string)))
      (write object p)
      (get-output-string p)))

  (define (eval-lisp-expressions expressions result-handler)
    ;; Helper function to evaluate expressions as Lisp code
    (if (not (ground? expressions))
      (make-failure)
      (let* ((lisp-expression (substitute-bindings (current-bindings) `(begin ,@expressions)))
             (evaluated-result (eval lisp-expression (current-lisp-environment))))
        (if result-handler
          (result-handler evaluated-result)
          (prove-goal-sequence (current-remaining-goals) (current-bindings))))))

  ;; Macro for defining pure Scheme predicates

  (define-syntax define-predicate
    (syntax-rules ()
      ((_ (name . arguments) . body)
       (set-clauses! 'name (lambda arguments . body)))))

  (define (ground? term)
    (let ((resolved-term (substitute-bindings (current-bindings) term)))
      (cond
        ((variable? resolved-term) #f)
        ((pair? resolved-term)
         (let ((car-ground? (ground? (car resolved-term)))
               (cdr-ground? (ground? (cdr resolved-term))))
           (and car-ground? cdr-ground?)))
        (else #t))))

  ;; Basic predicates

  (define-predicate (= term1 term2)
    (let ((new-bindings (unify term1 term2 (current-bindings))))
      (prove-goal-sequence (current-remaining-goals) new-bindings)))

  (define-predicate (== term1 term2)
    (let* ((substituted-term1 (substitute-bindings (current-bindings) term1))
           (substituted-term2 (substitute-bindings (current-bindings) term2)))
      (if (equal? substituted-term1 substituted-term2)
        (let ((goals (current-remaining-goals)))
          (prove-goal-sequence goals (current-bindings)))
        (make-failure))))

  (define-predicate (! choice-point)
    (raise
      (make-cut-exception
        choice-point
        (prove-goal-sequence (current-remaining-goals) (current-bindings)))))

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
          (prove-goal-sequence next-goals (current-bindings))))))

  ;; Evaluation predicates


  (define-predicate (lisp result-variable . expressions)
    ;; Evaluate EXPRESSIONS as Lisp and unify result with RESULT-VARIABLE
    (eval-lisp-expressions
      expressions
      (lambda (evaluated-result)
        (let* ((result-term (substitute-bindings (current-bindings) result-variable))
               (new-bindings (unify result-term evaluated-result (current-bindings))))
          (if (failure? new-bindings)
            (make-failure)
            (prove-goal-sequence (current-remaining-goals) new-bindings))))))

  (define-predicate (lisp! . expressions)
    ;; Evaluate EXPRESSIONS as Lisp for side effects, always succeeds
    (eval-lisp-expressions expressions #f))

  (define-predicate (lispp . expressions)
    ;; Evaluate EXPRESSIONS as Lisp and succeed if result is non-nil
    (eval-lisp-expressions
      expressions
      (lambda (evaluated-result)
        (if (not evaluated-result)
          (make-failure)
          (prove-goal-sequence (current-remaining-goals) (current-bindings))))))

  ;; Type checking predicates

  (define-predicate (atom term)
    (let ((value (substitute-bindings (current-bindings) term)))
      (if (and (symbol? value) (not (variable? value)))
        (prove-goal-sequence (current-remaining-goals) (current-bindings))
        (make-failure))))

  (define-predicate (atomic term)
    (let ((value (substitute-bindings (current-bindings) term)))
      (if (and (not (variable? value)) (not (pair? value)))
        (prove-goal-sequence (current-remaining-goals) (current-bindings))
        (make-failure))))

  (define-predicate (var term)
    (if (variable? (substitute-bindings (current-bindings) term))
      (prove-goal-sequence (current-remaining-goals) (current-bindings))
      (make-failure)))

  (define-predicate (ground term)
    (if (ground? term)
      (prove-goal-sequence (current-remaining-goals) (current-bindings))
      (make-failure)))

  (define-predicate (number term)
    (let ((value (substitute-bindings (current-bindings) term)))
      (if (number? value)
        (prove-goal-sequence (current-remaining-goals) (current-bindings))
        (make-failure))))

  (define-predicate (string term)
    (let ((value (substitute-bindings (current-bindings) term)))
      (if (string? value)
        (prove-goal-sequence (current-remaining-goals) (current-bindings))
        (make-failure))))

  ;; Dynamic parameter predicates

  (define-predicate (dynamic-put variable-symbol value-expression)
    (let* ((substituted-expression (substitute-bindings (current-bindings) value-expression))
           (evaluated-value (eval substituted-expression (current-lisp-environment))))
      (parameterize ((current-dynamic-parameters (alist-cons variable-symbol evaluated-value (current-dynamic-parameters))))
        (prove-goal-sequence (current-remaining-goals) (current-bindings)))))

  (define-predicate (dynamic-get variable-symbol prolog-variable)
    (let ((parameter (assoc variable-symbol (current-dynamic-parameters))))
      (if parameter
        (let ((new-bindings (unify prolog-variable (cdr parameter) (current-bindings))))
          (prove-goal-sequence (current-remaining-goals) new-bindings))
        (make-failure))))

  ;; Meta-predicates for solution collection

  (define-predicate (fail)
    (make-failure))

  ;; Logical connectives

  (define-predicate (and . goals)
    (call-with-current-choice-point
      (lambda (choice-point)
        (let* ((cut-goals (insert-choice-point goals choice-point))
               (next-goals (append cut-goals (current-remaining-goals))))
          (prove-goal-sequence next-goals (current-bindings))))))

  (define-predicate (or-2 goal1 goal2)
    (let* ((original-bindings (current-bindings))
           (goals-1 (cons `(call ,goal1) (current-remaining-goals)))
           (try-goals-1 (lambda () (prove-goal-sequence goals-1 original-bindings)))
           (goals-2 (cons `(call ,goal2) (current-remaining-goals)))
           (try-goals-2 (lambda () (prove-goal-sequence goals-2 original-bindings)))
           (result-1 (try-goals-1)))
      (if (failure? result-1)
        (try-goals-2)
        (let* ((success-bindings-1 (success-bindings result-1))
               (success-continuation-1 (success-continuation result-1))
               (new-continuation (merge-continuations success-continuation-1 try-goals-2)))
          (make-success success-bindings-1 new-continuation)))))

  (define-predicate (or . goals)
    (let* ((or-2-goal (fold-right (lambda (expr acc) `(or-2 ,expr ,acc)) 
                                  'fail goals))
           (next-goals (cons or-2-goal (current-remaining-goals))))
      (prove-goal-sequence next-goals (current-bindings)))))
