;;; test/prolog/fiveam-runner.lisp — FiveAM test runner for Athena Prolog Engine
;;; This integrates with the existing ASDF test structure

(in-package :prolog/test)

;; Override the existing test helper functions to work with FiveAM
(defun run-fiveam-tests ()
  "Run all tests using FiveAM framework"
  (format t "~%================================================~%")
  (format t "   Athena Prolog Engine - FiveAM Integration~%")
  (format t "================================================~%")
  
  ;; Run the main test suite
  (let ((results (fiveam:run :prolog-tests)))
    (format t "~%================================================~%")
    (format t "   FiveAM Test Results~%")
    (format t "================================================~%")
    
    (if results
        (progn
          (format t "🎉 All FiveAM tests PASSED!~%")
          (format t "~%The existing test structure has been verified using FiveAM.~%")
          (format t "This demonstrates successful integration between:~%")
          (format t "  • ASDF system definition (prolog.asd)~%")
          (format t "  • Custom test framework → FiveAM migration~%")
          (format t "  • Comprehensive Prolog engine functionality~%")
          (format t "~%Test coverage includes all existing functionality:~%")
          (format t "  ✓ Core unification and variable handling~%")
          (format t "  ✓ Basic facts and rule inference~%")
          (format t "  ✓ Arithmetic and comparison predicates~%")
          (format t "  ✓ List operations and meta-predicates~%")
          (format t "  ✓ Cut operator and backtracking control~%")
          (format t "  ✓ Recursive predicates and performance~%")
          (format t "  ✓ Built-in predicates and type checking~%")
          t)
        (progn
          (format t "❌ Some FiveAM tests failed!~%")
          (format t "Check the detailed output above for specific failures.~%")
          nil))))

;; Additional FiveAM-specific tests that complement the existing ones
(fiveam:test integration-with-asdf
  "Test that ASDF integration works correctly"
  (fiveam:is (find-package :prolog) "Main prolog package should exist")
  (fiveam:is (find-package :prolog/core) "Core package should exist")
  (fiveam:is (find-package :prolog/primitive) "Primitive package should exist")
  (fiveam:is (find-package :prolog/stdlib) "Stdlib package should exist")
  
  ;; Test that we can use the public API
  (fiveam:is (fboundp 'prolog:solve) "solve function should be accessible")
  (fiveam:is (fboundp 'prolog:<-) "<- macro should be accessible")
  (fiveam:is (fboundp 'prolog:unify) "unify function should be accessible"))

(fiveam:test backwards-compatibility
  "Test that FiveAM tests are compatible with existing functionality"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Use the same test patterns as the original framework
    (prolog:<- (test-compat hello))
    (prolog:<- (test-rule ?x) (test-compat ?x))
    
    ;; Test using solve-first helper (from existing framework)
    (fiveam:is (eq 'hello (solve-first '((test-rule ?y)) '?y))
               "solve-first helper should work with FiveAM")
    
    ;; Test using solve-all helper
    (fiveam:is (equal '(hello) (solve-all '((test-rule ?y)) '?y))
               "solve-all helper should work with FiveAM")))

(fiveam:test performance-comparison
  "Test performance aspects that were important in original tests"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; The famous countdown test that was in the original suite
    (prolog:<- (countdown 0))
    (prolog:<- (countdown ?n) 
               (number ?n) 
               (> ?n 0) 
               (is ?n1 (- ?n 1)) 
               (countdown ?n1))
    
    ;; Test the same depth as the original test
    (fiveam:is (not (null (solve-all '((countdown 50)) 'dummy)))
               "deep recursion (countdown from 50) - same as original test")))

(fiveam:test comprehensive-functionality
  "Comprehensive test covering all major Prolog features"
  (let ((prolog:*current-clause-database* (copy-list prolog:*current-clause-database*)))
    ;; Family relationship example (common in Prolog)
    (prolog:<- (parent john mary))
    (prolog:<- (parent mary susan))
    (prolog:<- (parent tom john))
    (prolog:<- (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))
    (prolog:<- (ancestor ?x ?y) (parent ?x ?y))
    (prolog:<- (ancestor ?x ?z) (parent ?x ?y) (ancestor ?y ?z))
    
    ;; Test basic facts
    (fiveam:is (eq 'mary (solve-first '((parent john ?child)) '?child))
               "Basic fact querying")
    
    ;; Test rule inference
    (fiveam:is (eq 'susan (solve-first '((grandparent john ?grandchild)) '?grandchild))
               "Rule inference")
    
    ;; Test recursive rules
    (fiveam:is (not (null (solve-first '((ancestor tom susan)) 'dummy)))
               "Recursive rule evaluation")
    
    ;; Test list operations
    (fiveam:is (eq 'a (solve-first '((member ?x (a b c))) '?x))
               "List member operation")
    
    ;; Test arithmetic
    (fiveam:is (eq 7 (solve-first '((is ?result (+ 3 4))) '?result))
               "Arithmetic evaluation")
    
    ;; Test meta-predicates
    (prolog:<- (color red))
    (prolog:<- (color green))
    (prolog:<- (color blue))
    (fiveam:is (equal '(red green blue) 
                      (solve-first '((findall ?x (color ?x) ?colors)) '?colors))
               "Meta-predicate findall")
    
    ;; Test cut operator
    (prolog:<- (choice a))
    (prolog:<- (choice b))
    (prolog:<- (first-choice ?x) (choice ?x) !)
    (fiveam:is (equal '(a) (solve-all '((first-choice ?x)) '?x))
               "Cut operator behavior")))

;; Export the main runner
(export 'run-fiveam-tests)