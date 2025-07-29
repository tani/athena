;; prolog.scm — Prolog engine main entry point
;; Copyright © 2025 Masaya Taniguchi
;; Released under the GNU General Public License v3.0
;;
;; This file combines the Prolog core engine and standard library.
;; It is intended to be included by a version-specific (R6RS/R7RS) wrapper.

(begin
  ;; Include the core Prolog engine
  (include "./prolog-core.scm")
  
  ;; Include the standard library and built-in predicates
  (include "./prolog-lib.scm"))

