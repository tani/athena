;;; prolog.lisp — Main Prolog API wrapper for Common Lisp
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file provides the main entry point and API for the Prolog engine

(defpackage :prolog
  (:use :cl)
  (:import-from :prolog-core
                ;; Variable handling
                #:variable-p #:named-variable-p #:atom-p
                #:variables-in #:replace-anonymous-variables
                ;; Bindings and unification
                #:lookup-variable #:substitute-bindings #:unify
                #:*current-occurs-check*
                ;; Failure/success structures
                #:failure #:make-failure #:failure-p
                #:success #:make-success #:success-p
                #:success-bindings #:success-continuation
                ;; Cut exception
                #:cut-exception #:make-cut-exception #:cut-exception-p
                #:cut-exception-tag #:cut-exception-value
                ;; Clause database
                #:*current-clause-database* #:get-clauses #:set-clauses
                #:add-clause #:remove-clauses-with-arity #:min-arity
                ;; Proof engine
                #:*current-bindings* #:*current-remaining-goals*
                #:prove #:prove-all #:call-with-current-choice-point
                ;; Spy/debug support
                #:*current-spy-mode* #:*current-spy-predicates*
                #:*current-spy-depth* #:*current-spy-indent-p*
                ;; Solution streams
                #:make-solution-stream #:run-query #:display-solution
                ;; Macros
                #:<- #:<-- #:?- #:define-predicate
                ;; Dynamic parameters
                #:*current-dynamic-parameters* #:get-dynamic-parameter
                ;; Utilities
                #:ground-p #:*current-lisp-environment*
                #:object->string)
  (:export
   ;; Re-export all public symbols
   #:variable-p #:named-variable-p #:atom-p
   #:variables-in #:replace-anonymous-variables
   #:substitute-bindings #:unify
   #:*current-occurs-check*
   #:failure-p #:success-p
   #:success-bindings #:success-continuation
   #:*current-clause-database* #:get-clauses #:set-clauses
   #:add-clause #:remove-clauses-with-arity #:min-arity
   #:prove-all #:call-with-current-choice-point
   #:*current-spy-mode* #:*current-spy-predicates*
   #:*current-spy-depth* #:*current-spy-indent-p*
   #:make-solution-stream #:run-query
   #:<- #:<-- #:?- #:define-predicate
   #:*current-lisp-environment*
   ;; Additional utilities
   #:object->string
   #:solve-first
   #:solve-all))

(in-package :prolog)

;;; Additional utility functions

(defun solve-first (goals term)
  "Find the first solution to goals and substitute bindings in term"
  (let ((ss (make-solution-stream goals)))
    (let ((solution (funcall ss)))
      (if solution
          (substitute-bindings solution term)
          '()))))

(defun solve-all (goals term &optional (max-solutions 100))
  "Find all solutions to goals and substitute bindings in term for each"
  (let ((ss (make-solution-stream goals))
        (results '())
        (count 0))
    (loop for solution = (funcall ss)
          while (and solution (< count max-solutions))
          do (push (substitute-bindings solution term) results)
             (incf count))
    (nreverse results)))

;;; Initialize the Prolog environment
(defun initialize-prolog ()
  "Initialize the Prolog environment with standard library loaded"
  ;; The library is loaded when this file is loaded
  (format t "Prolog engine initialized.~%"))

;;; ASDF system definition (can be moved to a separate .asd file)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :asdf)
    (require :asdf)))

(asdf:defsystem :athena-prolog
  :description "Athena - A comprehensive Prolog engine implemented in Common Lisp"
  :version "1.0.0"
  :author "Masaya Taniguchi"
  :license "GPL-3.0"
  :depends-on ()
  :components ((:file "prolog-core")
               (:file "prolog-lib" :depends-on ("prolog-core"))
               (:file "prolog" :depends-on ("prolog-lib"))))