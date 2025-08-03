;;; test/shared/test-patterns.scm — Shared test patterns for Scheme implementations
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This module provides unified testing patterns that work across
;;; different Scheme implementations for the Athena Prolog engine.

(define-library (athena test patterns)
  (export
    ;; Core test utilities
    with-test-database
    reset-test-environment
    measure-execution-time

    ;; Prolog test helpers
    solve-first
    solve-all
    solve-count
    assert-solutions
    assert-no-solutions
    assert-solution-count

    ;; Performance testing
    benchmark-predicate
    performance-test

    ;; Database testing utilities
    with-fresh-database
    assert-predicate-exists

    ;; Test data generators
    generate-test-facts
    generate-test-rules)

  (import (scheme base)
    (scheme write)
    (scheme time)
    (srfi 64))

  (begin

    ;;; Global test state
    (define *test-performance-data* '())
    (define *original-clause-database* '())

    ;;; Core Test Infrastructure
    ;;; ========================

    (define-syntax with-test-database
      (syntax-rules ()
        ((_ body ...)
          (let ((original-db (current-clause-database)))
            (dynamic-wind
              (lambda ()
                (current-clause-database '()))
              (lambda ()
                body
                ...)
              (lambda ()
                (current-clause-database original-db)))))))

    (define (reset-test-environment)
      "Reset the Prolog environment to a clean state for testing."
      (current-clause-database '())
      (set! *test-performance-data* '()))

    (define-syntax measure-execution-time
      (syntax-rules ()
        ((_ body ...)
          (let ((start-time (current-jiffy)))
            (let ((result (begin body ...)))
              (let ((end-time (current-jiffy)))
                (values result
                  (/ (* 1000.0 (- end-time start-time)) (jiffies-per-second)))))))))

    ;;; Prolog Query Utilities
    ;;; ======================

    (define (solve-first goals term)
      "Solve GOALS and return the first solution's substitution of TERM, or #f if no solution."
      (call/cc
        (lambda (return)
          (solve goals
            (lambda (solution)
              (return (substitute-bindings solution term)))
            (lambda () #f)))))

    (define (solve-all goals term)
      "Solve GOALS and return a list of all solutions' substitutions of TERM."
      (let ((results '()))
        (solve goals
          (lambda (solution)
            (set! results (cons (substitute-bindings solution term) results)))
          (lambda () #f))
        (reverse results)))

    (define (solve-count goals)
      "Count the number of solutions for GOALS."
      (let ((count 0))
        (solve goals
          (lambda (solution)
            (set! count (+ count 1)))
          (lambda () #f))
        count))

    ;;; Test Assertion Utilities
    ;;; ========================

    (define-syntax assert-solutions
      (syntax-rules ()
        ((_ goals term expected-solutions)
          (assert-solutions goals term expected-solutions ""))
        ((_ goals term expected-solutions description)
          (let ((actual-solutions (solve-all goals term)))
            (test-assert
              (string-append "Solutions match"
                (if (string=? description "") "" (string-append " - " description)))
              (equal? actual-solutions expected-solutions))))))

    (define-syntax assert-no-solutions
      (syntax-rules ()
        ((_ goals)
          (assert-no-solutions goals ""))
        ((_ goals description)
          (let ((count (solve-count goals)))
            (test-assert
              (string-append "No solutions"
                (if (string=? description "") "" (string-append " - " description)))
              (= count 0))))))

    (define-syntax assert-solution-count
      (syntax-rules ()
        ((_ goals expected-count)
          (assert-solution-count goals expected-count ""))
        ((_ goals expected-count description)
          (let ((actual-count (solve-count goals)))
            (test-assert
              (string-append "Solution count: " (number->string expected-count)
                (if (string=? description "") "" (string-append " - " description)))
              (= actual-count expected-count))))))

    ;;; Performance Testing
    ;;; ===================

    (define-syntax benchmark-predicate
      (syntax-rules ()
        ((_ name goals)
          (benchmark-predicate name goals 100 10))
        ((_ name goals iterations)
          (benchmark-predicate name goals iterations 10))
        ((_ name goals iterations warmup-iterations)
          (begin
            ;; Warmup
            (do ((i 0 (+ i 1)))
              ((>= i warmup-iterations))
              (solve-count goals))

            ;; Benchmark
            (let ((times '()))
              (do ((i 0 (+ i 1)))
                ((>= i iterations))
                (let-values (((result time) (measure-execution-time (solve-count goals))))
                  (set! times (cons time times))))

              (let* ((total-time (fold + 0 times))
                     (avg-time (/ total-time iterations))
                     (min-time (fold min (car times) times))
                     (max-time (fold max (car times) times)))

                (set! *test-performance-data*
                  (cons (list name iterations total-time avg-time min-time max-time times)
                    *test-performance-data*))

                (values avg-time min-time max-time)))))))

    (define-syntax performance-test
      (syntax-rules ()
        ((_ description body ...)
          (begin
            (display (string-append "\n=== Performance Test: " description " ===\n"))
            body
            ...
            (display "\nPerformance test completed.\n")))))

    ;;; Database Testing Utilities
    ;;; ===========================

    (define-syntax with-fresh-database
      (syntax-rules ()
        ((_ body ...)
          (with-test-database body ...))))

    (define (assert-predicate-exists predicate-name . description)
      "Assert that a predicate with PREDICATE-NAME exists in the database."
      (let ((clauses (get-clauses predicate-name))
            (desc (if (null? description) "" (car description))))
        (test-assert
          (string-append "Predicate " (symbol->string predicate-name) " exists"
            (if (string=? desc "") "" (string-append " - " desc)))
          (not (null? clauses)))))

    ;;; Test Data Generators
    ;;; ====================

    (define (generate-test-facts predicate-name facts)
      "Generate test facts for PREDICATE-NAME using the list of FACTS."
      (for-each (lambda (fact)
                 (<- (cons predicate-name fact)))
        facts))

    (define (generate-test-rules rules)
      "Generate test rules from a list of RULES in the format ((head . body) ...)."
      (for-each (lambda (rule)
                 (let ((head (car rule))
                       (body (cdr rule)))
                   (if (null? body)
                     (<- head)
                     (apply <- head body))))
        rules))

    ;;; Test Report Generation
    ;;; ======================

    (define (generate-performance-report)
      "Generate a performance test report from collected data."
      (display "\n=== Performance Test Report ===\n")
      (for-each (lambda (data)
                 (let ((name (car data))
                       (iterations (cadr data))
                       (avg-time (cadddr data))
                       (min-time (car (cddddr data)))
                       (max-time (cadr (cddddr data))))
                   (display (string-append (symbol->string name) ": avg="
                             (number->string avg-time)
                             "ms, min="
                             (number->string min-time)
                             "ms, max="
                             (number->string max-time)
                             "ms ("
                             (number->string iterations)
                             " iterations)\n"))))
        (reverse *test-performance-data*))
      (display "\n"))

    ;; Helper function for fold (if not available)
    (define (fold proc init lst)
      (if (null? lst)
        init
        (fold proc (proc (car lst) init) (cdr lst))))))
