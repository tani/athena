;;; package.lisp — Test package definition for Athena Prolog Engine
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(defpackage :prolog-test
  (:use :cl)
  (:import-from :prolog
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
                #:call
                #:true
                #:fail
                #:ground-p)
  (:import-from :prolog/core
                #:named-variable-p
                #:atom-p
                #:variables-in
                #:replace-anonymous-variables
                #:call-with-current-choice-point
                #:failure-p
                #:success-p
                #:success-bindings
                #:success-continuation)
  (:import-from :prolog/primitive
                #:object->string
                #:=
                #:number
                #:>
                #:member
                #:append
                #:maplist
                #:findall
                #:bagof)
  (:import-from :prolog/stdlib
                #:not
                #:and
                #:or
                #:if)
  (:import-from :athena/test/utilities
                #:solve-first
                #:solve-all)
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
                #:pass)
  (:export #:run-all-tests))

(in-package :prolog-test)

(def-suite :prolog-tests
  :description "Main test suite for Athena Prolog Engine")