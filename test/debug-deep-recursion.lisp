;;; Debug script for deep recursion test failure

(load "src/prolog/core.lisp")
(load "src/prolog/primitive.lisp")
(load "src/prolog/stdlib.lisp")
(load "test/helpers.lisp")

(defpackage :prolog/test-core
  (:use :common-lisp :prolog/core :prolog/primitive :prolog/stdlib :prolog/test-helpers))

(in-package :prolog/test-core)

(format t "~%=== Debugging deep recursion test failure ===~%")

(test-group "performance"
  (let ((*current-clause-database* (copy-list *current-clause-database*)))
    
    (format t "~%1. Setting up countdown predicates...~%")
    
    ;; Define countdown predicates exactly as in test
    (<- (countdown 0))
    (<- (countdown ?n) 
        (number ?n)
        (> ?n 0)
        (is ?n1 (- ?n 1))
        (countdown ?n1))
    
    (format t "Predicates defined.~%")
    
    ;; Test basic arithmetic predicates first
    (format t "~%2. Testing arithmetic predicates...~%")
    
    (format t "Testing (number 50): ")
    (let ((result (solve-all '((number 50)) 'dummy)))
      (format t "~S~%" (not (null result))))
    
    (format t "Testing (> 50 0): ")
    (let ((result (solve-all '((> 50 0)) 'dummy)))
      (format t "~S~%" (not (null result))))
    
    (format t "Testing (is ?x (- 50 1)): ")
    (let ((result (solve-all '((is ?x (- 50 1))) '?x)))
      (format t "~S~%" result))
    
    ;; Test small recursion first
    (format t "~%3. Testing small countdown values...~%")
    
    (format t "Testing (countdown 0): ")
    (let ((result (solve-all '((countdown 0)) 'dummy)))
      (format t "~S (length: ~D)~%" (not (null result)) (length result)))
    
    (format t "Testing (countdown 1): ")
    (let ((result (solve-all '((countdown 1)) 'dummy)))
      (format t "~S (length: ~D)~%" (not (null result)) (length result)))
    
    (format t "Testing (countdown 2): ")
    (let ((result (solve-all '((countdown 2)) 'dummy)))
      (format t "~S (length: ~D)~%" (not (null result)) (length result)))
    
    (format t "Testing (countdown 3): ")
    (let ((result (solve-all '((countdown 3)) 'dummy)))
      (format t "~S (length: ~D)~%" (not (null result)) (length result)))
    
    ;; Test medium recursion
    (format t "~%4. Testing medium countdown values...~%")
    
    (format t "Testing (countdown 10): ")
    (handler-case
      (let ((result (solve-all '((countdown 10)) 'dummy)))
        (format t "~S (length: ~D)~%" (not (null result)) (length result)))
      (error (e)
        (format t "ERROR: ~A~%" e)))
    
    ;; Test the failing case with timeout
    (format t "~%5. Testing the failing case (countdown 50)...~%")
    (format t "Testing (countdown 50): ")
    (handler-case
      (let ((result (solve-all '((countdown 50)) 'dummy)))
        (format t "~S (length: ~D)~%" (not (null result)) (length result)))
      (error (e)
        (format t "ERROR: ~A~%" e)))
    
    ;; Manual step-by-step for countdown 50
    (format t "~%6. Manual tracing of countdown execution...~%")
    (format t "Tracing first few steps of (countdown 50):~%")
    (let ((step 0))
      (handler-case
        (solve '((countdown 50))
               (lambda (bindings)
                 (format t "  Step ~D: SUCCESS with bindings ~S~%" (incf step) bindings)
                 (when (> step 3)
                   (error "Stopping trace after 3 steps")))
               (lambda ()
                 (format t "  No solutions found~%")))
        (error (e)
          (unless (string= (format nil "~A" e) "Stopping trace after 3 steps")
            (format t "  ERROR during trace: ~A~%" e)))))
    
    ;; Check if there are issues with specific arithmetic operations
    (format t "~%7. Checking arithmetic operations in sequence...~%")
    (format t "Simulating countdown steps manually:~%")
    (format t "  50 -> ")
    (let ((result (solve-first '((is ?x (- 50 1))) '?x)))
      (format t "~S~%" result))
    (format t "  49 -> ")
    (let ((result (solve-first '((is ?x (- 49 1))) '?x)))
      (format t "~S~%" result))
    
    ;; Test if the issue is stack overflow
    (format t "~%8. Testing for stack overflow issues...~%")
    (format t "The issue might be Common Lisp stack limits with deep recursion.~%")))

(format t "~%=== Debug complete ===~%")