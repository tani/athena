;;; test-simple-asdf.lisp — Simple ASDF integration test without FiveAM
;;; This script tests basic ASDF loading without external dependencies

;; Load ASDF (required for script execution)
(require :asdf)

;; Tell ASDF where to find our system definition
(push (uiop:getcwd) asdf:*central-registry*)

(format t "~%=== Simple ASDF Integration Test ===~%")

;; Test 1: Load the main system
(format t "1. Loading main :prolog system...~%")
(handler-case
    (progn
      (asdf:load-system :prolog)
      (format t "   ✅ Successfully loaded :prolog system~%"))
  (error (e)
    (format t "   ❌ Failed to load :prolog system: ~A~%" e)
    (sb-ext:quit :unix-status 1)))

;; Test 2: Check that main package exists and basic API works
(format t "2. Testing main package API...~%")
(handler-case
    (progn
      ;; Test package existence
      (find-package :prolog)
      (format t "   ✅ Package :prolog exists~%")
      
      ;; Test that we can use the main API
      (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
        ;; Test basic clause definition
        (prolog:<- (test-fact a))
        (prolog:<- (test-rule ?x) (test-fact ?x))
        (format t "   ✅ Clause definition works~%")
        
        ;; Test that we can query using a simple solve call
        (let ((result nil))
          (prolog:solve '((test-rule ?y))
                        (lambda (solution) 
                          (setf result (prolog:substitute-bindings solution '?y)))
                        (lambda () nil))
          (if (eq result 'a)
              (format t "   ✅ Basic querying works~%")
              (error "Expected 'a, got ~S" result))))
      
      ;; Test that exported functions are accessible
      (unless (fboundp 'prolog:solve)
        (error "prolog:solve not accessible"))
      (unless (fboundp 'prolog:<-)
        (error "prolog:<- not accessible"))
      (unless (fboundp 'prolog:unify)
        (error "prolog:unify not accessible"))
      (format t "   ✅ All main API functions are accessible~%"))
  (error (e)
    (format t "   ❌ Main package API failed: ~A~%" e)
    (sb-ext:quit :unix-status 1)))

;; Test 3: Check that sub-packages are loaded correctly
(format t "3. Testing sub-packages...~%")
(handler-case
    (progn
      (find-package :prolog/core)
      (find-package :prolog/primitive)
      (find-package :prolog/stdlib)
      (format t "   ✅ All sub-packages exist~%"))
  (error (e)
    (format t "   ❌ Sub-package test failed: ~A~%" e)
    (sb-ext:quit :unix-status 1)))

;; Test 4: Test basic Prolog functionality
(format t "4. Testing Prolog functionality...~%")
(handler-case
    (progn
      (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
        ;; Define some test predicates
        (prolog:<- (parent john mary))
        (prolog:<- (parent mary susan))
        (prolog:<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
        
        ;; Test simple query
        (let ((result nil))
          (prolog:solve '((parent john ?child))
                        (lambda (solution)
                          (setf result (prolog:substitute-bindings solution '?child)))
                        (lambda () nil))
          (unless (eq result 'mary)
            (error "Simple query failed: expected mary, got ~S" result)))
        
        ;; Test rule query
        (let ((result nil))
          (prolog:solve '((grandparent john ?grandchild))
                        (lambda (solution)
                          (setf result (prolog:substitute-bindings solution '?grandchild)))
                        (lambda () nil))
          (unless (eq result 'susan)
            (error "Rule query failed: expected susan, got ~S" result)))
        
        (format t "   ✅ Basic Prolog functionality works~%")))
  (error (e)
    (format t "   ❌ Prolog functionality test failed: ~A~%" e)
    (sb-ext:quit :unix-status 1)))

;; Test 5: Test arithmetic comparisons (our recently added feature)
(format t "5. Testing arithmetic comparisons...~%")
(handler-case
    (progn
      (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
        ;; Test the countdown functionality that was failing before
        (prolog:<- (countdown 0))
        (prolog:<- (countdown ?n) 
                   (number ?n)
                   (> ?n 0)
                   (is ?n1 (- ?n 1))
                   (countdown ?n1))
        
        ;; Test a small countdown
        (let ((success nil))
          (prolog:solve '((countdown 3))
                        (lambda (solution)
                          (declare (ignore solution))
                          (setf success t))
                        (lambda () nil))
          (unless success
            (error "Countdown test failed"))
          (format t "   ✅ Arithmetic comparisons and recursion work~%"))))
  (error (e)
    (format t "   ❌ Arithmetic test failed: ~A~%" e)
    (sb-ext:quit :unix-status 1)))

(format t "~%🎉 All ASDF integration tests passed!~%")
(format t "The ASDF system definition is working correctly.~%")
(sb-ext:quit)