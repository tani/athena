;;; test/benchmarks/performance-suite.lisp — Comprehensive performance benchmark suite
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file provides comprehensive performance benchmarks for the Athena Prolog engine,
;;; measuring key operations and identifying potential performance bottlenecks.

(require :asdf)
(asdf:load-system :prolog)

;; Load our test utilities
(load (merge-pathnames "../shared/test-utilities.lisp" 
                       (make-pathname :directory (pathname-directory *load-truename*))))

(defpackage :athena/benchmarks/performance
  (:use :common-lisp :prolog :athena/test/utilities)
  (:export :run-performance-suite :benchmark-summary :performance-report))

(in-package :athena/benchmarks/performance)

;;; ============================================================================
;;; BENCHMARK INFRASTRUCTURE
;;; ============================================================================

(defvar *benchmark-results* '()
  "Storage for all benchmark results")

(defstruct benchmark-result
  "Structure to hold benchmark results"
  name                    ; Benchmark name
  category               ; Category (unification, database, etc.)
  iterations             ; Number of iterations
  total-time            ; Total execution time (seconds)
  average-time          ; Average time per iteration
  min-time              ; Minimum time
  max-time              ; Maximum time
  operations-per-second ; Operations per second
  memory-usage          ; Memory usage (if available)
  notes)                ; Additional notes

(defmacro benchmark (name category iterations &body body)
  "Execute benchmark and record results."
  (let ((times (gensym "TIMES"))
        (start-mem (gensym "START-MEM"))
        (end-mem (gensym "END-MEM"))
        (i (gensym "I")))
    `(let ((,times '())
           (,start-mem #+sbcl (sb-kernel:dynamic-usage) #-sbcl 0))
       
       ;; Warmup (10% of iterations)
       (dotimes (,i (max 1 (floor ,iterations 10)))
         ,@body)
       
       ;; Actual benchmark
       (dotimes (,i ,iterations)
         (let ((start-time (get-internal-real-time)))
           ,@body
           (let ((end-time (get-internal-real-time)))
             (push (/ (- end-time start-time) internal-time-units-per-second) ,times))))
       
       (let* ((,end-mem #+sbcl (sb-kernel:dynamic-usage) #-sbcl 0)
              (total-time (reduce #'+ ,times))
              (avg-time (/ total-time ,iterations))
              (min-time (reduce #'min ,times))
              (max-time (reduce #'max ,times))
              (ops-per-sec (if (> avg-time 0) (/ 1.0 avg-time) 0))
              (mem-used (- ,end-mem ,start-mem)))
         
         (push (make-benchmark-result
                 :name ,name
                 :category ,category
                 :iterations ,iterations
                 :total-time total-time
                 :average-time avg-time
                 :min-time min-time
                 :max-time max-time
                 :operations-per-second ops-per-sec
                 :memory-usage mem-used)
               *benchmark-results*)
         
         (format t "~A: ~,6F ms avg (~,0F ops/sec)~%" 
                 ,name (* avg-time 1000) ops-per-sec)
         
         avg-time))))

;;; ============================================================================
;;; UNIFICATION BENCHMARKS
;;; ============================================================================

(defun benchmark-unification ()
  "Benchmark unification operations."
  (format t "~%=== Unification Benchmarks ===~%")
  
  ;; Simple atom unification
  (benchmark "Simple Atom Unification" :unification 100000
    (unify 'foo 'foo '()))
  
  ;; Variable binding
  (benchmark "Variable Binding" :unification 100000
    (unify '?x 'foo '()))
  
  ;; Complex term unification
  (benchmark "Complex Term Unification" :unification 50000
    (unify '(parent john ?child) '(parent john mary) '()))
  
  ;; Deep structure unification
  (benchmark "Deep Structure Unification" :unification 10000
    (unify '(deeply (nested (structure (with many levels))))
           '(deeply (nested (structure (with many levels))))
           '()))
  
  ;; Unification failure (early detection)
  (benchmark "Unification Failure (Early)" :unification 100000
    (unify 'foo 'bar '()))
  
  ;; Unification failure (late detection)
  (benchmark "Unification Failure (Late)" :unification 10000
    (unify '(complex (structure (with (many (levels foo)))))
           '(complex (structure (with (many (levels bar)))))
           '()))
  
  ;; Variable chain resolution
  (let ((chain-bindings '((?a . ?b) (?b . ?c) (?c . ?d) (?d . foo))))
    (benchmark "Variable Chain Resolution" :unification 50000
      (substitute-bindings chain-bindings '?a))))

;;; ============================================================================
;;; DATABASE OPERATION BENCHMARKS
;;; ============================================================================

(defun benchmark-database-operations ()
  "Benchmark clause database operations."
  (format t "~%=== Database Operation Benchmarks ===~%")
  
  (with-fresh-database
    ;; Setup: Add many clauses
    (dotimes (i 1000)
      (add-clause! 'test-predicate `((test-predicate ,(intern (format nil "ARG~D" i))))))
    
    ;; Clause lookup
    (benchmark "Clause Lookup" :database 10000
      (get-clauses 'test-predicate))
    
    ;; Clause addition
    (benchmark "Clause Addition" :database 5000
      (add-clause! 'new-predicate '((new-predicate foo))))
    
    ;; Database scan
    (benchmark "Database Scan" :database 1000
      (length *current-clause-database*))
    
    ;; Setup: Create predicates with different arities
    (dotimes (arity 10)
      (dotimes (i 100)
        (let ((args (loop for j from 1 to arity collect (intern (format nil "A~D" j)))))
          (add-clause! (intern (format nil "PRED~D" arity)) 
                       `((,(intern (format nil "PRED~D" arity)) ,@args))))))
    
    ;; Arity-based filtering
    (benchmark "Arity-based Filtering" :database 5000
      (remove-clauses-with-arity! 'test-predicate 1))))

;;; ============================================================================
;;; QUERY EXECUTION BENCHMARKS
;;; ============================================================================

(defun benchmark-query-execution ()
  "Benchmark query execution performance."
  (format t "~%=== Query Execution Benchmarks ===~%")
  
  (with-fresh-database
    ;; Setup family tree
    (dotimes (i 100)
      (<- (parent ,(intern (format nil "PERSON~D" i)) 
                  ,(intern (format nil "PERSON~D" (+ i 100))))))
    
    (<- (grandparent ?gp ?gc) (parent ?gp ?p) (parent ?p ?gc))
    
    ;; Simple fact lookup
    (benchmark "Simple Fact Lookup" :query 10000
      (solve-count '((parent person0 person100))))
    
    ;; Variable binding query
    (benchmark "Variable Binding Query" :query 5000
      (solve-count '((parent person0 ?child))))
    
    ;; Join query (grandparent)
    (benchmark "Join Query" :query 1000
      (solve-count '((grandparent person0 ?grandchild))))
    
    ;; Backtracking query
    (benchmark "Backtracking Query" :query 1000
      (solve-count '((parent ?parent ?child))))
    
    ;; Recursive query setup
    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))
    
    ;; Recursive query
    (benchmark "Recursive Query" :query 100
      (solve-count '((ancestor person0 ?descendant))))))

;;; ============================================================================
;;; ARITHMETIC BENCHMARKS
;;; ============================================================================

(defun benchmark-arithmetic ()
  "Benchmark arithmetic operations."
  (format t "~%=== Arithmetic Benchmarks ===~%")
  
  ;; Simple arithmetic
  (benchmark "Simple Addition" :arithmetic 50000
    (solve-count '((is ?result (+ 2 3)))))
  
  ;; Complex arithmetic
  (benchmark "Complex Arithmetic" :arithmetic 10000
    (solve-count '((is ?result (+ (* 2 3) (/ 10 2))))))
  
  ;; Arithmetic comparison
  (benchmark "Arithmetic Comparison" :arithmetic 50000
    (solve-count '((> 5 3))))
  
  ;; Variable arithmetic
  (with-fresh-database
    (<- (number-fact 42))
    (benchmark "Variable Arithmetic" :arithmetic 10000
      (solve-count '((number-fact ?x) (is ?result (+ ?x 10)))))))

;;; ============================================================================
;;; META-PREDICATE BENCHMARKS
;;; ============================================================================

(defun benchmark-meta-predicates ()
  "Benchmark meta-predicates performance."
  (format t "~%=== Meta-predicate Benchmarks ===~%")
  
  (with-fresh-database
    ;; Setup data for meta-predicates
    (dotimes (i 50)
      (<- (item ,(intern (format nil "ITEM~D" i)))))
    
    ;; findall performance
    (benchmark "findall/3" :meta-predicate 1000
      (solve-count '((findall ?x (item ?x) ?items))))
    
    ;; call/1 performance
    (benchmark "call/1" :meta-predicate 10000
      (solve-count '((call (item item0)))))
    
    ;; Nested meta-calls
    (benchmark "Nested Meta-calls" :meta-predicate 5000
      (solve-count '((call (call (item item0))))))))

;;; ============================================================================
;;; MEMORY AND STRESS BENCHMARKS
;;; ============================================================================

(defun benchmark-memory-stress ()
  "Benchmark memory usage and stress scenarios."
  (format t "~%=== Memory and Stress Benchmarks ===~%")
  
  ;; Large term creation
  (benchmark "Large Term Creation" :memory 1000
    (let ((large-term (make-list 1000 :initial-element 'atom)))
      (unify large-term large-term '())))
  
  ;; Deep recursion
  (with-fresh-database
    (<- (countdown 0))
    (<- (countdown ?n) (> ?n 0) (is ?n1 (- ?n 1)) (countdown ?n1))
    
    (benchmark "Deep Recursion (100 levels)" :memory 100
      (solve-count '((countdown 100)))))
  
  ;; Backtracking stress
  (with-fresh-database
    (dotimes (i 20)
      (<- (choice ,(intern (format nil "OPT~D" i)))))
    
    (benchmark "Backtracking Stress" :memory 100
      (solve-count '((choice ?x) (choice ?y) (choice ?z)))))
  
  ;; Variable binding chains
  (benchmark "Variable Chain (10 deep)" :memory 10000
    (let ((bindings '()))
      (dotimes (i 10)
        (setf bindings (acons (intern (format nil "?VAR~D" i))
                              (intern (format nil "?VAR~D" (+ i 1)))
                              bindings)))
      (substitute-bindings bindings '?var0))))

;;; ============================================================================
;;; COMPARISON BENCHMARKS
;;; ============================================================================

(defun benchmark-implementations ()
  "Compare different implementation strategies."
  (format t "~%=== Implementation Comparison Benchmarks ===~%")
  
  ;; Compare different unification strategies if available
  (with-fresh-database
    ;; Pattern 1: First argument indexing friendly
    (dotimes (i 100)
      (<- (indexed ,(intern (format nil "KEY~D" (mod i 10))) ?value)))
    
    ;; Pattern 2: No first argument indexing
    (dotimes (i 100)
      (<- (unindexed data ,(intern (format nil "KEY~D" (mod i 10))) ?value)))
    
    (benchmark "Indexed Lookup" :comparison 5000
      (solve-count '((indexed key0 ?value))))
    
    (benchmark "Unindexed Lookup" :comparison 5000
      (solve-count '((unindexed data key0 ?value))))))

;;; ============================================================================
;;; REGRESSION BENCHMARKS
;;; ============================================================================

(defun benchmark-regression ()
  "Regression benchmarks to detect performance changes."
  (format t "~%=== Regression Benchmarks ===~%")
  
  ;; Standard operations that should maintain consistent performance
  (benchmark "Regression: Basic Unification" :regression 100000
    (unify '(f ?x) '(f a) '()))
  
  (benchmark "Regression: Simple Query" :regression 10000
    (with-fresh-database
      (<- (test-fact a))
      (solve-count '((test-fact a)))))
  
  (benchmark "Regression: Arithmetic" :regression 20000
    (solve-count '((is ?x (+ 1 2 3 4 5)))))
  
  (benchmark "Regression: Findall" :regression 1000
    (with-fresh-database
      (dotimes (i 10)
        (<- (reg-fact ,(intern (format nil "ITEM~D" i)))))
      (solve-count '((findall ?x (reg-fact ?x) ?items))))))

;;; ============================================================================
;;; BENCHMARK SUITE RUNNER
;;; ============================================================================

(defun run-performance-suite (&key (categories :all) (verbose t))
  "Run the complete performance benchmark suite."
  (when verbose
    (format t "~%╔════════════════════════════════════════════════════════════════╗~%")
    (format t "║                    Athena Prolog Performance Suite            ║~%") 
    (format t "╚════════════════════════════════════════════════════════════════╝~%"))
  
  ;; Reset results
  (setf *benchmark-results* '())
  
  ;; Run benchmark categories
  (when (or (eq categories :all) (member :unification categories))
    (benchmark-unification))
  
  (when (or (eq categories :all) (member :database categories))
    (benchmark-database-operations))
  
  (when (or (eq categories :all) (member :query categories))
    (benchmark-query-execution))
  
  (when (or (eq categories :all) (member :arithmetic categories))
    (benchmark-arithmetic))
  
  (when (or (eq categories :all) (member :meta-predicate categories))
    (benchmark-meta-predicates))
  
  (when (or (eq categories :all) (member :memory categories))
    (benchmark-memory-stress))
  
  (when (or (eq categories :all) (member :comparison categories))
    (benchmark-implementations))
  
  (when (or (eq categories :all) (member :regression categories))
    (benchmark-regression))
  
  (when verbose
    (performance-report)
    (format t "~%Performance suite completed.~%"))
  
  *benchmark-results*)

(defun benchmark-summary ()
  "Print a summary of benchmark results."
  (format t "~%=== Benchmark Summary ===~%")
  (format t "Total benchmarks: ~D~%" (length *benchmark-results*))
  
  (let ((categories (remove-duplicates 
                     (mapcar #'benchmark-result-category *benchmark-results*))))
    (dolist (category categories)
      (let ((category-results (remove-if-not 
                               (lambda (r) (eq (benchmark-result-category r) category))
                               *benchmark-results*)))
        (format t "~%~A (~D benchmarks):~%" category (length category-results))
        (dolist (result category-results)
          (format t "  ~A: ~,3F ms~%" 
                  (benchmark-result-name result)
                  (* (benchmark-result-average-time result) 1000)))))))

(defun performance-report (&optional (stream t))
  "Generate a detailed performance report."
  (format stream "~%╔════════════════════════════════════════════════════════════════╗~%")
  (format stream "║                    Performance Report                         ║~%")
  (format stream "╚════════════════════════════════════════════════════════════════╝~%")
  
  (let ((sorted-results (sort (copy-list *benchmark-results*)
                              (lambda (a b)
                                (string< (symbol-name (benchmark-result-category a))
                                        (symbol-name (benchmark-result-category b)))))))
    
    (format stream "~%~A~A~A~A~A~%"
            (format nil "~30A" "Benchmark")
            (format nil "~12A" "Category") 
            (format nil "~12A" "Avg (ms)")
            (format nil "~12A" "Ops/sec")
            (format nil "~12A" "Iterations"))
    
    (format stream "~80,'-A~%" "")
    
    (dolist (result sorted-results)
      (format stream "~30A~12A~12,3F~12,0F~12D~%"
              (benchmark-result-name result)
              (benchmark-result-category result)
              (* (benchmark-result-average-time result) 1000)
              (benchmark-result-operations-per-second result)
              (benchmark-result-iterations result))))
  
  ;; Performance analysis
  (format stream "~%=== Performance Analysis ===~%")
  
  ;; Find slowest operations
  (let ((slowest (first (sort (copy-list *benchmark-results*)
                              (lambda (a b)
                                (> (benchmark-result-average-time a)
                                   (benchmark-result-average-time b)))))))
    (when slowest
      (format stream "Slowest operation: ~A (~,3F ms)~%"
              (benchmark-result-name slowest)
              (* (benchmark-result-average-time slowest) 1000))))
  
  ;; Find fastest operations
  (let ((fastest (first (sort (copy-list *benchmark-results*)
                              (lambda (a b)
                                (< (benchmark-result-average-time a)
                                   (benchmark-result-average-time b)))))))
    (when fastest
      (format stream "Fastest operation: ~A (~,3F ms)~%"
              (benchmark-result-name fastest)
              (* (benchmark-result-average-time fastest) 1000))))
  
  ;; Category averages
  (format stream "~%Category averages:~%")
  (let ((categories (remove-duplicates 
                     (mapcar #'benchmark-result-category *benchmark-results*))))
    (dolist (category categories)
      (let* ((category-results (remove-if-not 
                                (lambda (r) (eq (benchmark-result-category r) category))
                                *benchmark-results*))
             (avg-time (/ (reduce #'+ category-results 
                                  :key #'benchmark-result-average-time)
                          (length category-results))))
        (format stream "  ~A: ~,3F ms average~%" category (* avg-time 1000))))))

;; Export main function for script usage
(defun main ()
  "Main entry point for command-line benchmark execution."
  (handler-case
      (progn
        (run-performance-suite)
        #+sbcl (sb-ext:exit :code 0))
    (error (e)
      (format t "~%Error running benchmarks: ~A~%" e)
      #+sbcl (sb-ext:exit :code 1))))

;; Module completion
(eval-when (:load-toplevel :execute)
  (format t "~%Athena Performance Benchmark Suite loaded.~%")
  (format t "Use (run-performance-suite) to run all benchmarks.~%"))