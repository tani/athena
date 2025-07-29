(test-begin "prolog-lib")

;; -----------------------------------------------------------
;; Library predicates (or/member/append/repeat/true)
;; -----------------------------------------------------------
(test-group "library"
  (test-equal "or variadic first succeeds"
              'ok
              (solve-first '((or (= ?r ok) (= ?r ng) (= ?r nope))) '?r))
  (test-equal "or variadic middle succeeds"
              'ok
              (solve-first '((or (= 1 2) (= ?r ok) (= ?r ng))) '?r))
  (test-equal "or variadic all solutions"
              '(ok also-ok stop)
              (solve-all '((or (= ?r ok) (= ?r also-ok) (= ?r stop))) '?r))

  (test-equal "and variadic success"
              '(ok yes)
              (solve-first '((and (= ?a ok) (= ?b yes))) '(?a ?b)))
  (test-assert "and variadic failure"
               (null? (solve-all '((and (= 1 2) (= ?x 3))) '?x)))

  (test-assert "member/2 success" (not (null? (solve-all '((member b (a b c))) 'dummy))))
  (test-assert "member/2 failure" (null? (solve-all '((member x (a b c))) 'dummy)))
  (test-equal "member/2 generate" '(a b c) (solve-all '((member ?x (a b c))) '?x))

  (test-equal "append/3 forward" '(a b c d) (solve-first '((append (a b) (c d) ?x)) '?x))
  (test-equal "append/3 backward" '(() (a b c)) (solve-first '((append ?x ?y (a b c))) '(?x ?y)))
  (test-equal "append/3 all splits" 4 (length (solve-all '((append ?x ?y (a b c))) '(?x ?y))))

  (test-assert "repeat generates multiple solutions"
               (let loop ((n 0))
                 (if (>= n 5)
                     #t
                     (and (solve-first '((repeat)) 'dummy)
                          (loop (+ n 1))))))
  (test-assert "true/0 always succeeds" (not (null? (solve-all '((true)) 'dummy))))
  (test-assert "and/0 behaves as true"
               (not (null? (solve-all '((and)) 'dummy))))
  (test-assert "or/0 behaves as fail"
               (null? (solve-all '((or)) 'dummy)))

  ;; maplist
  (test-assert "maplist atom success"
               (not (null? (solve-all '((maplist atom (a b c))) 'dummy))))
  (test-assert "maplist atom failure"
               (null? (solve-all '((maplist atom (a 1 c))) 'dummy)))
  (test-equal "maplist = unify lists"
              '(a b c)
              (solve-first '((maplist = (a b c) (?x ?y ?z))) '(?x ?y ?z))))

(test-end "prolog-lib")