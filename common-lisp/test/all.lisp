;;; test.lisp — Main test package and runner for Athena Prolog Engine
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(defpackage :prolog/test/all
  (:use :cl)
  (:import-from :prolog/all
    #:<-
    #:<--
    #:?-
    #:run-query
    #:solve
    #:variable-p
    #:unify
    #:substitute-bindings
    #:add-clause!
    #:get-clauses
    #:remove-clauses-with-arity!
    #:prove-all
    #:*current-clause-database*
    #:ground-p
    #:named-variable-p
    #:atom-p
    #:variables-in
    #:replace-anonymous-variables
    #:call-with-current-choice-point
    #:failure-p
    #:success-p
    #:success-bindings
    #:success-continuation
    #:object->string)
  (:import-from :prolog/test/utilities
    #:solve-first
    #:solve-all
    #:solve-count)
  (:import-from :fiveam
    #:def-suite
    #:in-suite
    #:is
    #:is-true
    #:is-false
    #:signals
    #:finishes
    #:test
    #:run!
    #:run
    #:pass))

(in-package :prolog/test/all)

;;; Test Suite Definition
;;; =====================

(def-suite :prolog-test-suite
  :description
  "Main test suite for Athena Prolog Engine")
