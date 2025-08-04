;;; utilities.lisp — Simple test utilities for Athena Prolog Engine
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(defpackage :prolog-test/utilities
  (:use :common-lisp)
  (:import-from :prolog/all
                #:solve
                #:substitute-bindings)
  (:export
   ;; Simple Prolog query helpers
   :solve-first
   :solve-all
   :solve-count))

(in-package :prolog-test/utilities)

;;; Simple Prolog Query Helpers
;;; ============================

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

(defun solve-count (goals)
  "Count the number of solutions for GOALS."
  (let ((count 0))
    (solve goals
                  (lambda (solution)
                    (declare (ignore solution))
                    (incf count))
                  (lambda () nil))
    count))
