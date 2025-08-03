;;; test/prolog/package.lisp — Test package definitions for FiveAM
;;; Copyright © 2025 Masaya Taniguchi 
;;; Released under the GNU General Public License v3.0

(defpackage :prolog/test
  (:use :common-lisp :fiveam :prolog)
  (:export :prolog-tests :run-all-tests
           ;; Test helper functions (preserve from old framework)
           :solve-first :solve-all))

(defpackage :prolog/test/core
  (:use :common-lisp :fiveam :prolog :prolog/test)
  (:import-from :prolog/core
                ;; Import additional symbols needed for core tests
                :named-variable-p :atom-p :variables-in :replace-anonymous-variables
                :set-clauses! :call-with-current-choice-point
                :make-failure :failure-p :make-success :success-p  
                :success-bindings :success-continuation
                :*current-spy-predicates* :*current-spy-mode* :*current-occurs-check*))

(defpackage :prolog/test/primitive  
  (:use :common-lisp :fiveam :prolog :prolog/test)
  (:import-from :prolog/primitive
                ;; Import symbols needed for primitive tests
                :object->string :define-predicate
                := :== :atom :atomic :var :ground :number :string
                :findall :bagof :setof :sort :is :lisp))

(defpackage :prolog/test/stdlib
  (:use :common-lisp :fiveam :prolog :prolog/test))

(in-package :prolog/test)

;; Main test suite that contains all other suites
(def-suite :prolog-tests
  :description "All tests for the Athena Prolog Engine")

;; Test helper functions preserved from original framework
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

