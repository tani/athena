;;; src/prolog.lisp — Main Prolog package interface for package-inferred-system
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file defines the main :prolog package that provides a clean
;;; public API by re-exporting selected symbols from the implementation
;;; packages.

(uiop:define-package :prolog/package
  (:nicknames prolog)
  (:use-reexport
    :prolog/core
    :prolog/primitive
    :prolog/stdlib))
