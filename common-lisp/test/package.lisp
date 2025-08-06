;;; test.lisp — Main test package and runner for Athena Prolog Engine
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(defpackage :prolog/test/package
  (:use :cl :prolog/package :prolog/test/utilities)
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

(in-package :prolog/test/package)

;;; Test Suite Definition
;;; =====================

(def-suite :prolog-test-suite
  :description
  "Main test suite for Athena Prolog Engine")
