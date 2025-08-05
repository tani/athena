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
  (define current-solution-accumulator (make-parameter '()))
  (define current-lisp-environment (make-parameter (interaction-environment)))
  (define current-dynamic-parameters (make-parameter '()))

  (define (object->string object)
    (let ((p (open-output-string)))
      (write object p)
      (get-output-string p)))

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

  ;; Evaluation predicates

  (define-predicate (--lisp-eval-internal result-variable expression)
    (let* ((scheme-expression (substitute-bindings (current-bindings) expression))
           (evaluated-result (eval scheme-expression (current-lisp-environment)))
           (result-term (substitute-bindings (current-bindings) result-variable))
           (new-bindings (unify result-term evaluated-result (current-bindings))))
      (prove-all (current-remaining-goals) new-bindings)))

  ;; Type checking predicates

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

  (define-predicate (string term)
    (let ((value (substitute-bindings (current-bindings) term)))
      (if (string? value)
        (prove-all (current-remaining-goals) (current-bindings))
        (make-failure))))

  ;; Dynamic parameter predicates

  (define-predicate (dynamic-put variable-symbol value-expression)
    (let* ((substituted-expression (substitute-bindings (current-bindings) value-expression))
           (evaluated-value (eval substituted-expression (current-lisp-environment))))
      (parameterize ((current-dynamic-parameters (alist-cons variable-symbol evaluated-value (current-dynamic-parameters))))
        (prove-all (current-remaining-goals) (current-bindings)))))

  (define-predicate (dynamic-get variable-symbol prolog-variable)
    (let ((parameter (assoc variable-symbol (current-dynamic-parameters))))
      (if parameter
        (let ((new-bindings (unify prolog-variable (cdr parameter) (current-bindings))))
          (prove-all (current-remaining-goals) new-bindings))
        (make-failure))))

  ;; Meta-predicates for solution collection

  (define-predicate (fail)
    (make-failure)))
