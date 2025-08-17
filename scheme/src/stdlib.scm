;; prolog-stdlib.scm — Prolog standard library clauses
;; Copyright © 2025 Masaya Taniguchi
;; Released under the GNU General Public License v3.0
;;
;; This file contains the standard library clauses for Prolog:
;; - Control flow predicates (and, or, not, if)
;; - Basic predicates (true, repeat)
;; - List manipulation predicates (member, append, maplist)
;; - Evaluation wrappers (lisp, is)

(begin
  ;; Standard clause database initialization and basic predicates

  (<-- true)
  (<-- false (fail))

  ;; Type checking predicates (implemented as Prolog clauses)
  ;; Note: For atom/1, we need to check that it's a symbol but not a variable
  (<-- (atom ?term) (lispp (and (symbol? '?term) (not (variable? '?term)))))
  (<-- (atomic ?term) (lispp (and (not (variable? '?term)) (not (pair? '?term)))))
  (<-- (number ?term) (lispp (number? '?term)))
  (<-- (string ?term) (lispp (string? '?term)))
  ;; ground/1 checks if the term contains no unbound variables
  (<-- (ground ?term) (lispp '?term))

  ;; Basic logical predicates - these use the built-in predicates from primitive.scm
  ;; but also provide clause-based alternatives for compatibility

  (<- (not ?goal) (call ?goal) ! (fail))
  (<- (not ?goal))

  (<- (if ?cond ?then ?else) (call ?cond) ! (call ?then))
  (<- (if ?cond ?then ?else) (call ?else))
  (<- (if ?cond ?then) (call ?cond) (call ?then))

  ;; Arithmetic evaluation - delegate to built-in lisp predicate
  (<- (is ?result ?expression) (lisp ?result ?expression))

  (<- (repeat))
  (<- (repeat) (repeat))

  (<- (member ?item (?item . ?)))
  (<- (member ?item (? . ?rest)) (member ?item ?rest))

  ;; List operations
  (<- (append () ?list ?list))
  (<- (append (?head . ?tail) ?list (?head . ?result))
    (append ?tail ?list ?result))

  ;; Two-argument append (append/2)
  (<- (append () ()))
  (<- (append (?head . ?tail) ?result)
    (append ?tail ?tail-result)
    (append (?head) ?tail-result ?result))

  ;; Higher-order predicates (maplist variants)
  (<-- (maplist ?goal ()))
  (<- (maplist ?goal (?head . ?tail))
    (call ?goal ?head)
    (maplist ?goal ?tail))

  (<-- (maplist ?goal () ()))
  (<- (maplist ?goal (?head1 . ?tail1) (?head2 . ?tail2))
    (call ?goal ?head1 ?head2)
    (maplist ?goal ?tail1 ?tail2))

  (<-- (maplist ?goal () () ()))
  (<- (maplist ?goal (?head1 . ?tail1) (?head2 . ?tail2) (?head3 . ?tail3))
    (call ?goal ?head1 ?head2 ?head3)
    (maplist ?goal ?tail1 ?tail2 ?tail3))

  (<-- (maplist ?goal () () () ()))
  (<- (maplist ?goal (?head1 . ?tail1) (?head2 . ?tail2) (?head3 . ?tail3) (?head4 . ?tail4))
    (call ?goal ?head1 ?head2 ?head3 ?head4)
    (maplist ?goal ?tail1 ?tail2 ?tail3 ?tail4)))
