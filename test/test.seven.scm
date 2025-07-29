(import (scheme base)
        (srfi 64)
        (srfi 41)
        (scheme r5rs)
        (prolog))

(cond-expand
  (chibi 
    (include "test/helpers.scm")
    (include "test/core.scm")
    (include "test/lib.scm"))
  (else  
    (include "helpers.scm")
    (include "core.scm")
    (include "lib.scm")))
