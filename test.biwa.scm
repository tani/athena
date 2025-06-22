(define current-test-suite-name (make-parameter "Default Test Suite"))
(define current-test-group-name (make-parameter "Default Test Group"))
(define current-test-failures (make-parameter 0))
(define current-test-total (make-parameter 0))

(define (test-begin name)
  (current-test-suite-name name) ; parameter の値を設定
  (current-test-failures 0)     ; parameter の値を設定
  (current-test-total 0)        ; parameter の値を設定
  (display (string-append "--- Starting Test Suite: " (current-test-suite-name) " ---\n")))

(define (test-end name)
  (display (string-append "--- Test Suite: " (current-test-suite-name) " Finished ---\n"))
  (display (string-append "Total tests: " (number->string (current-test-total)) "\n"))
  (display (string-append "Failed tests: " (number->string (current-test-failures)) "\n"))
  (if (= (current-test-failures) 0)
      (display "All tests passed!\n")
      (display "Some tests failed.\n"))
  (newline))

(define-macro (test-group name . body)
  `(parameterize ((current-test-group-name ,name))
     (display (string-append "  Entering Test Group: " (current-test-group-name) "\n"))
     (begin ,@body)))

(define (test-report-failure assertion-name expected actual)
  (current-test-failures (+ (current-test-failures) 1)) ; parameter の値を更新
  (current-test-total (+ (current-test-total) 1))       ; parameter の値を更新
  (display (string-append "    FAIL [" (current-test-group-name) "] " assertion-name ": Expected "
                          (object->string expected) ", got " (object->string actual) "\n")))

(define (test-report-success assertion-name)
  (current-test-total (+ (current-test-total) 1))       ; parameter の値を更新
  (display (string-append "    PASS [" (current-test-group-name) "] " assertion-name "\n")))

(define-macro (test-assert assertion-name test-expr)
  `(if ,test-expr
       (test-report-success ,assertion-name)
       (test-report-failure ,assertion-name "#t" ,test-expr)))

(define-macro (test-equal assertion-name expected-expr actual-expr)
  `(let ((expected ,expected-expr)
         (actual ,actual-expr))
     (if (equal? expected actual)
         (test-report-success ,assertion-name)
         (test-report-failure ,assertion-name expected actual))))

(load "prolog.biwa.scm")
(load "test.scm")
