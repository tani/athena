;;; prolog.asd — ASDF system definition for Athena Prolog Engine
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0

(defsystem "prolog"
  :class :package-inferred-system
  :description "Athena - A comprehensive Prolog engine implemented in Common Lisp"
  :long-description "Athena provides a complete Prolog implementation with unification,
backtracking, cut operator, built-in predicates, and seamless Lisp integration."
  :author "Masaya Taniguchi"
  :license "GPL-3.0"
  :version "1.0.0"
  :homepage "https://github.com/tani/athena"
  :bug-tracker "https://github.com/tani/athena/issues"
  :source-control (:git "https://github.com/tani/athena.git")
  :pathname "src"
  :depends-on ("prolog/package")
  :in-order-to ((test-op (test-op "prolog/test"))))

(defsystem "prolog/test"
  :description "Test suite for Athena Prolog Engine"
  :author "Masaya Taniguchi"
  :license "GPL-3.0"
  :pathname "test"
  :depends-on ("prolog" "fiveam")
  :serial t
  :components ((:file "utilities")
               (:file "package")
               (:file "core")
               (:file "primitive")
               (:file "stdlib"))
  :perform (test-op (operation system)
            (declare (ignore operation system))
            (symbol-call :fiveam :run! :prolog-test-suite)))
