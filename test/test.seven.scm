(import (scheme base)
        (srfi 64)
        (srfi 41)
        (scheme r5rs)
        (prolog))

(cond-expand
  (chibi (include "test/test.scm"))
  (else  (include "test.scm")))
