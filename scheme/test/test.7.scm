(import
  (scheme base)
  (srfi 64)
  (scheme r5rs)
  (prolog))

(test-begin "prolog")
(cond-expand
  (chibi
    (include "test/helpers.scm")
    (include "test/core.scm")
    (include "test/lib.scm"))
  (else
    (include "helpers.scm")
    (include "core.scm")
    (include "lib.scm")))
(test-end "prolog")
