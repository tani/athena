(defpackage :prolog/stdlib
  (:use :common-lisp :prolog/core :prolog/primitive))

(in-package :prolog/stdlib)

;; Basic logical predicates
(<-- true)
(<-- false (fail))

(<-- (not ?goal) (call ?goal) ! (fail))
(<- (not ?goal))

;; Logical operators
(<-- (if ?cond ?then) (call ?cond) (call ?then))
(<-- (if ?cond ?then ?else) (call ?cond) ! (call ?then))
(<- (if ?cond ?then ?else) (call ?else))

;; Arithmetic
(<-- (is ?result ?expression) (lisp ?result ?expression))

;; Control flow
(<-- repeat)
(<- (repeat) (repeat))

;; List operations
(<-- (member ?item (?item . ?)))
(<- (member ?item (? . ?rest)) (member ?item ?rest))

(<-- (append () ?list ?list))
(<- (append (?head . ?tail) ?list (?head . ?result))
  (append ?tail ?list ?result))

(<-- (append () ()))
(<- (append (?head . ?tail) ?result)
  (append ?tail ?tail-result)
  (append ?head ?tail-result ?result))

;; Higher-order predicates
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
  (maplist ?goal ?tail1 ?tail2 ?tail3 ?tail4))