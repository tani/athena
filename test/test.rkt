#lang racket/base

(require
  srfi/64
  srfi/41
  "../src/prolog.rkt"
  racket/include)

(test-begin "prolog")
(include "helpers.scm")
(include "core.scm")
(include "lib.scm")
(test-end "prolog")
