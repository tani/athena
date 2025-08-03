;;; test/shared/test-utilities.lisp — Shared test utilities for Common Lisp tests
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This module provides unified testing utilities that work across
;;; different Common Lisp test frameworks and provide consistent
;;; test patterns for the Athena Prolog engine.

(defpackage :athena/test/utilities
  (:use :common-lisp)
  (:export
   ;; Core test utilities
   :with-test-database
   :reset-test-environment
   :measure-execution-time
   
   ;; Prolog test helpers
   :solve-first
   :solve-all
   :solve-count
   :assert-solutions
   :assert-no-solutions
   :assert-solution-count
   
   ;; Performance testing
   :benchmark-predicate
   :performance-test
   
   ;; Database testing utilities
   :with-fresh-database
   :assert-predicate-exists
   :assert-predicate-arity
   
   ;; Error testing
   :assert-prolog-error
   :assert-unification-failure))

(in-package :athena/test/utilities)

;;; Core Test Infrastructure
;;; ========================

(defvar *test-performance-data* '()
  "Storage for performance test results")

(defvar *original-clause-database* nil
  "Backup of original clause database for restoration")

(defmacro with-test-database (&body body)
  "Execute BODY with a fresh, isolated Prolog database that gets restored afterwards."
  `(let ((*original-clause-database* (copy-tree prolog:*current-clause-database*)))
     (unwind-protect
         (progn
           (setf prolog:*current-clause-database* '())
           ,@body)
       (setf prolog:*current-clause-database* *original-clause-database*))))

(defun reset-test-environment ()
  "Reset the Prolog environment to a clean state for testing."
  (setf prolog:*current-clause-database* '())
  (setf *test-performance-data* '()))

(defmacro measure-execution-time (&body body)
  "Measure and return the execution time of BODY in milliseconds."
  (let ((start-time (gensym "START"))
        (end-time (gensym "END")))
    `(let ((,start-time (get-internal-real-time)))
       (prog1
           (progn ,@body)
         (let ((,end-time (get-internal-real-time)))
           (/ (* 1000 (- ,end-time ,start-time)) internal-time-units-per-second))))))

;;; Prolog Query Utilities
;;; ======================

(defun solve-first (goals term)
  "Solve GOALS and return the first solution's substitution of TERM, or NIL if no solution."
  (block solve-first
    (prolog:solve goals
                  (lambda (solution)
                    (let ((result (prolog:substitute-bindings solution term)))
                      (return-from solve-first result)))
                  (lambda () nil))
    nil))

(defun solve-all (goals term)
  "Solve GOALS and return a list of all solutions' substitutions of TERM."
  (let ((results '()))
    (prolog:solve goals
                  (lambda (solution)
                    (let ((result (prolog:substitute-bindings solution term)))
                      (push result results)))
                  (lambda () nil))
    (nreverse results)))

(defun solve-count (goals)
  "Count the number of solutions for GOALS."
  (let ((count 0))
    (prolog:solve goals
                  (lambda (solution)
                    (declare (ignore solution))
                    (incf count))
                  (lambda () nil))
    count))

;;; Test Assertion Utilities
;;; ========================

(defmacro assert-solutions (goals term expected-solutions &optional description)
  "Assert that GOALS produces the EXPECTED-SOLUTIONS for TERM."
  `(let ((actual-solutions (solve-all ,goals ,term)))
     (unless (equal actual-solutions ,expected-solutions)
       (error "Solution mismatch~@[ in ~A~]:~%  Expected: ~S~%  Actual: ~S"
              ,description ,expected-solutions actual-solutions))))

(defmacro assert-no-solutions (goals &optional description)
  "Assert that GOALS produces no solutions."
  `(let ((count (solve-count ,goals)))
     (unless (zerop count)
       (error "Expected no solutions~@[ in ~A~], but got ~D"
              ,description count))))

(defmacro assert-solution-count (goals expected-count &optional description)
  "Assert that GOALS produces exactly EXPECTED-COUNT solutions."
  `(let ((actual-count (solve-count ,goals)))
     (unless (= actual-count ,expected-count)
       (error "Solution count mismatch~@[ in ~A~]:~%  Expected: ~D~%  Actual: ~D"
              ,description ,expected-count actual-count))))

;;; Performance Testing
;;; ===================

(defmacro benchmark-predicate (name goals &key (iterations 100) (warmup-iterations 10))
  "Benchmark a predicate and store results for analysis."
  (let ((times (gensym "TIMES"))
        (warmup (gensym "WARMUP"))
        (i (gensym "I")))
    `(progn
       ;; Warmup iterations
       (dotimes (,warmup ,warmup-iterations)
         (solve-count ,goals))
       
       ;; Actual benchmark
       (let ((,times '()))
         (dotimes (,i ,iterations)
           (let ((time (measure-execution-time (solve-count ,goals))))
             (push time ,times)))
         
         (let* ((total-time (reduce #'+ ,times))
                (avg-time (/ total-time ,iterations))
                (min-time (reduce #'min ,times))
                (max-time (reduce #'max ,times)))
           
           (push (list :name ,name
                       :iterations ,iterations
                       :total-time total-time
                       :avg-time avg-time
                       :min-time min-time
                       :max-time max-time
                       :times ,times)
                 *test-performance-data*)
           
           (values avg-time min-time max-time))))))

(defmacro performance-test (description &body test-cases)
  "Run performance tests and report results."
  `(progn
     (format t "~%=== Performance Test: ~A ===~%" ,description)
     ,@test-cases
     (format t "~%Performance test completed.~%")))

;;; Database Testing Utilities
;;; ===========================

(defmacro with-fresh-database (&body body)
  "Execute BODY with a completely fresh database."
  `(with-test-database
     ,@body))

(defun assert-predicate-exists (predicate-name &optional description)
  "Assert that a predicate with PREDICATE-NAME exists in the database."
  (let ((clauses (prolog:get-clauses predicate-name)))
    (unless clauses
      (error "Predicate ~S does not exist~@[ in ~A~]" predicate-name description))))

(defun assert-predicate-arity (predicate-name expected-arity &optional description)
  "Assert that PREDICATE-NAME has the EXPECTED-ARITY."
  (let ((clauses (prolog:get-clauses predicate-name)))
    (unless clauses
      (error "Predicate ~S does not exist~@[ in ~A~]" predicate-name description))
    
    (let ((actual-arity (length (cdar clauses))))
      (unless (= actual-arity expected-arity)
        (error "Predicate ~S arity mismatch~@[ in ~A~]:~%  Expected: ~D~%  Actual: ~D"
               predicate-name description expected-arity actual-arity)))))

;;; Error Testing
;;; =============

(defmacro assert-prolog-error (condition-type goals &optional description)
  "Assert that executing GOALS signals an error of CONDITION-TYPE."
  (let ((error-caught (gensym "ERROR-CAUGHT")))
    `(let ((,error-caught nil))
       (handler-case
           (solve-count ,goals)
         (,condition-type (condition)
           (declare (ignore condition))
           (setf ,error-caught t)))
       (unless ,error-caught
         (error "Expected ~S error~@[ in ~A~] but none was signaled"
                ',condition-type ,description)))))

(defmacro assert-unification-failure (term1 term2 &optional description)
  "Assert that unifying TERM1 and TERM2 fails."
  `(let ((result (prolog:unify ,term1 ,term2 '())))
     (unless (prolog/core:failure-p result)
       (error "Expected unification failure~@[ in ~A~]:~%  Term1: ~S~%  Term2: ~S~%  Result: ~S"
              ,description ,term1 ,term2 result))))

;;; Test Report Generation
;;; ======================

(defun generate-performance-report (&optional (stream t))
  "Generate a performance test report from collected data."
  (format stream "~%=== Performance Test Report ===~%")
  (format stream "~{~{~A: avg=~,3Fms, min=~,3Fms, max=~,3Fms (~D iterations)~%~}~}"
          (mapcar (lambda (data)
                    (list (getf data :name)
                          (getf data :avg-time)
                          (getf data :min-time) 
                          (getf data :max-time)
                          (getf data :iterations)))
                  (reverse *test-performance-data*)))
  (format stream "~%"))

;;; Module Initialization
;;; =====================

(eval-when (:load-toplevel :execute)
  (format t "~%Athena Test Utilities loaded successfully.~%"))