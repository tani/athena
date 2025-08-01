(define (solve-first goals term)
  (call/cc
    (lambda (exit)
      (solve
        goals
        (lambda (solution)
          (let ((result (substitute-bindings solution term)))
            (exit result)))
        (lambda () (exit '()))))))

(define (solve-all goals term)
  (let ((results '()))
    (solve
      goals
      (lambda (solution)
        (let ((result (substitute-bindings solution term)))
          (set! results (cons result results))))
      (lambda () #f))
    (reverse results)))
