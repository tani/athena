(defpackage :prolog/stdlib
  (:use :common-lisp :prolog/core :prolog/primitive))

(in-package :prolog/stdlib)

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
