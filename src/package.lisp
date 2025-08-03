;;; package.lisp — Main Prolog package interface  
;;; Copyright © 2025 Masaya Taniguchi
;;; Released under the GNU General Public License v3.0
;;;
;;; This file defines the main :prolog package that provides a clean
;;; public API by re-exporting selected symbols from the implementation
;;; packages.

(defpackage :prolog
  (:use :common-lisp)
  
  ;; Import core API symbols
  (:import-from :prolog/core
                ;; Main user API
                :<-                      ; Define clause
                :<--                     ; Define clause, removing existing ones
                :?-                      ; Interactive query macro
                :run-query               ; Programmatic query execution  
                :solve                   ; Execute queries with callbacks
                
                ;; Variables and unification
                :variable-p              ; Check if symbol is a Prolog variable
                :unify                   ; Unify two terms
                :substitute-bindings     ; Apply variable bindings to term
                
                ;; Database operations
                :add-clause!             ; Add clause to database
                :get-clauses             ; Get clauses for predicate
                :remove-clauses-with-arity! ; Remove clauses with specific arity
                
                ;; Advanced API (for extending the system)
                :prove-all               ; Low-level proof function
                :*current-clause-database*) ; Clause database parameter
  
  ;; Import built-in predicates from primitive
  (:import-from :prolog/primitive
                ;; Core predicates
                :call                    ; Meta-call predicate
                :true                    ; Always succeeds
                :fail                    ; Always fails
                
                ;; Utility functions
                :ground-p)               ; Check if term is fully ground
  
  ;; Re-export all imported symbols as the public API
  (:export
   ;; Main user API - clause definition and querying
   :<- :<-- :?- :run-query :solve
   
   ;; Unification and variables
   :variable-p :unify :substitute-bindings
   
   ;; Database manipulation  
   :add-clause! :get-clauses :remove-clauses-with-arity!
   
   ;; Built-in predicates
   :call :true :fail
   
   ;; Utilities
   :ground-p
   
   ;; Advanced API
   :prove-all :*current-clause-database*))

(in-package :prolog)

;; Package is ready - all functionality is imported from implementation packages