(js-eval "
  BiwaScheme.define_libfunc('eval', 1, 2, (ar, intp) => {
    const expr = ar[0];
    const intp2 = new BiwaScheme.Interpreter(intp);
    return intp2.evaluate(BiwaScheme.to_write(expr));
  });
")

(js-eval "
  BiwaScheme.define_libfunc('read-char', 0, 1, (ar) => {
    const port = ar[0] || BiwaScheme.Port.current_input;
    BiwaScheme.assert_port(port);
    return port.get_string((str) => {
      if (!str || str.length === 0) {
        return eof;
      }
      const ch = str.charAt(0);
      if (port instanceof BiwaScheme.Port.StringInput) {
        port.str = port.str.slice(1);
      }
      return Char.get(ch);
    });
  });
")

(define (flush-output-port _) (newline))

(define (environment _) (list))

(define (min . args)
  (define (min-1 a b)
    (if (< a b) a b))
  (fold-left min-1 0 args))

(define (alist-cons key val alist)
  (cons (cons key val) alist))

(define (alist-delete key alist . maybe=?)
  (let ((=? (if (null? maybe=?) equal? (car maybe=?))))
    (let loop ((xs alist))
      (cond ((null? xs) '())
            ((=? (caar xs) key) (loop (cdr xs)))
            (else (cons (car xs) (loop (cdr xs))))))))

(define (delete-duplicates lst . maybe=?)
  (let ((=? (if (null? maybe=?) equal? (car maybe=?))))
    (let loop ((seen '()) (rest lst))
      (cond ((null? rest) (reverse seen))
            ((exists (lambda (x) (=? x (car rest))) seen)
             (loop seen (cdr rest)))
            (else
             (loop (cons (car rest) seen) (cdr rest)))))))

(define-macro (<- . clause)
  (let ((head (car clause))
        (body (cdr clause)))
    (if (pair? head)
        `(add-clause! (replace-anonymous-variables ',clause))
        `(add-clause! (replace-anonymous-variables '((,head) . ,body))))))

(define-macro (<-- . clause)
  (let ((head (car clause))
        (body (cdr clause)))
    (if (pair? head)
        (let* ((name (car head))
               (args (cdr head))
               (arity (length args)))
          `(let ((arity ,arity))
             (remove-clauses-with-arity! ',name arity)
             (add-clause! (replace-anonymous-variables ',clause))))
        (let ((name head))
          `(let ((arity 0))
             (remove-clauses-with-arity! ',name arity)
             (add-clause! (replace-anonymous-variables '((,name) . ,body))))))))

(define-macro (?- . goals)
  `(run-query (replace-anonymous-variables ',goals)))

(define-macro (define-predicate name-args . body)
  `(set-clauses! ',(car name-args) (lambda ,(cdr name-args) . ,body)))

(define-macro (prolog . goals)
  `(%prolog ',goals))

(define-macro (define-syntax . _) `',_)

(define-record-type (<failure> make-failure failure?)
  (fields))

(define-record-type (<success> make-success success?)
  (fields
    (immutable bindings success-bindings)
    (immutable continuation success-continuation)))

(define (object->string object)
  (call-with-string-output-port
    (lambda (p) (write object p))))

(load "prolog.scm")

(define (flush-input-port _) (list))
