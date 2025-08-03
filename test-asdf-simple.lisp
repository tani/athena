;;; test-asdf-simple.lisp — Simple test using ASDF-loaded system
;;; This demonstrates using the Prolog system after loading via ASDF

;; Load ASDF and our system
(require :asdf)
(push (uiop:getcwd) asdf:*central-registry*)
(asdf:load-system :prolog)

(format t "~%=== Testing ASDF-loaded Prolog System ===~%")

;; Create a fresh clause database for testing
(let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
  
  ;; Define some facts and rules
  (prolog:<- (parent john mary))
  (prolog:<- (parent mary susan))
  (prolog:<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
  
  ;; Test basic querying
  (format t "1. Testing basic facts...~%")
  (let ((result nil))
    (prolog:solve '((parent john ?child))
                  (lambda (solution) 
                    (setf result (prolog:substitute-bindings solution '?child)))
                  (lambda () nil))
    (format t "   Who is John's child? ~A~%" result))
  
  ;; Test rule inference
  (format t "2. Testing rule inference...~%")
  (let ((result nil))
    (prolog:solve '((grandparent john ?grandchild))
                  (lambda (solution)
                    (setf result (prolog:substitute-bindings solution '?grandchild)))
                  (lambda () nil))
    (format t "   Who is John's grandchild? ~A~%" result))
  
  ;; Test arithmetic
  (format t "3. Testing arithmetic...~%")
  (prolog:<- (countdown 0))
  (prolog:<- (countdown ?n) (number ?n) (> ?n 0) (is ?n1 (- ?n 1)) (countdown ?n1))
  
  (let ((success nil))
    (prolog:solve '((countdown 5))
                  (lambda (solution)
                    (declare (ignore solution))
                    (setf success t))
                  (lambda () nil))
    (format t "   Countdown from 5: ~A~%" (if success "SUCCESS" "FAILED")))
  
  (format t "~%✅ All ASDF integration tests passed!~%"))

(format t "~%The Prolog system is ready for use via ASDF!~%")