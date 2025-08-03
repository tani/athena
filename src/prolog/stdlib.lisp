(defpackage :prolog/stdlib
  (:use :common-lisp :prolog/core :prolog/primitive))

(in-package :prolog/stdlib)

  
;; Control flow clauses
(<- (and) true)
(<- (and ?goal) (call ?goal))
(<- (and ?goal . ?goals) (call ?goal) (call (and . ?goals)))

(<- (or ?goal . ?goals) (call ?goal))
(<- (or ?goal . ?goals) (call (or . ?goals)))

(<- (not ?goal) (call ?goal) ! (fail))
(<- (not ?goal))

(<- (if ?cond ?then ?else) (call ?cond) ! (call ?then))
(<- (if ?cond ?then ?else) (call ?else))
(<- (if ?cond ?then) (call ?cond) (call ?then))

;; Repeat predicate
(<- (repeat))
(<- (repeat) (repeat))

;; List manipulation clauses
(<- (member ?item (?item . ?)))
(<- (member ?item (? . ?rest)) (member ?item ?rest))

(<- (append () ?list ?list))
(<- (append (?head . ?tail) ?list (?head . ?result))
    (append ?tail ?list ?result))

;; Maplist helper clauses
(<- (--all-null ()))
(<- (--all-null (() . ?rest)) (--all-null ?rest))

(<- (--get-heads () ()))
(<- (--get-heads ((?head . ?) . ?rest) (?head . ?heads))
    (--get-heads ?rest ?heads))

(<- (--get-tails () ()))
(<- (--get-tails ((? . ?tail) . ?rest) (?tail . ?tails))
    (--get-tails ?rest ?tails))

(<- (maplist ?pred . ?lists)
    (if (--all-null ?lists)
        true
        (and (--get-heads ?lists ?heads)
             (--get-tails ?lists ?tails)
             (call (?pred . ?heads))
             (call (maplist ?pred . ?tails)))))

