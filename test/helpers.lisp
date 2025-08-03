;;; test/helpers.lisp - Test helper functions for Prolog tests

(defpackage :prolog/test-helpers
  (:use :common-lisp :prolog/core)
  (:export :solve-first :solve-all :test-group :test-equal :test-assert))

(in-package :prolog/test-helpers)

(defun solve-first (goals term)
  "Solve GOALS and return the first solution's substitution of TERM, or NIL if no solution."
  (block solve-first
    (solve goals
           (lambda (solution)
             (let ((result (substitute-bindings solution term)))
               (return-from solve-first result)))
           (lambda () nil))
    nil))

(defun solve-all (goals term)
  "Solve GOALS and return a list of all solutions' substitutions of TERM."
  (let ((results '()))
    (solve goals
           (lambda (solution)
             (let ((result (substitute-bindings solution term)))
               (push result results)))
           (lambda () nil))
    (nreverse results)))

(defvar *current-test-group* nil)
(defvar *test-count* 0)
(defvar *pass-count* 0)

(defmacro test-group (name &body body)
  "Execute BODY with test group NAME for reporting."
  `(let ((*current-test-group* ,name))
     (format t "~%Testing ~A:~%" ,name)
     ,@body))

(defmacro test-equal (description expected actual)
  "Assert that ACTUAL equals EXPECTED."
  `(progn
     (incf *test-count*)
     (let ((expected-val ,expected)
           (actual-val ,actual))
       (if (equal expected-val actual-val)
           (progn
             (incf *pass-count*)
             (format t "  ✓ ~A~%" ,description))
           (progn
             (format t "  ✗ ~A~%" ,description)
             (format t "    Expected: ~S~%" expected-val)
             (format t "    Actual:   ~S~%" actual-val)
             (assert nil () "Test failed: ~A" ,description))))))

(defmacro test-assert (description condition)
  "Assert that CONDITION is true."
  `(progn
     (incf *test-count*)
     (if ,condition
         (progn
           (incf *pass-count*)
           (format t "  ✓ ~A~%" ,description))
         (progn
           (format t "  ✗ ~A~%" ,description)
           (assert nil () "Test failed: ~A" ,description)))))