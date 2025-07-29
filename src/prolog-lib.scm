;; prolog-lib.scm — Prolog standard library and built-in predicates
;; Copyright © 2025 Masaya Taniguchi
;; Released under the GNU General Public License v3.0
;;
;; This file contains the standard Prolog predicates and library functions:
;; - Basic unification and comparison predicates
;; - Arithmetic and type checking predicates
;; - Meta-predicates (bagof, findall, sort)
;; - Control flow predicates (and, or, not, if)
;; - List manipulation predicates (member, append, maplist)

(begin
  (define current-solution-accumulator (make-parameter '()))
  (define current-lisp-environment (make-parameter (interaction-environment)))
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

  ;; Dynamic parameter predicates

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

  ;; Meta-predicates for solution collection

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

  ;; Standard clause database initialization and basic predicates

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
       (sort ?result-bag ?result-set)))

