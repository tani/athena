#lang racket/base
(require srfi/64
         "../src/prolog.rkt"
         racket/include)
(define-namespace-anchor anchor)
(current-lisp-environment (namespace-anchor->namespace anchor))
(include "test.scm")
