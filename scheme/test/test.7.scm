(import
  (scheme base)
  (srfi 64)
  (scheme r5rs)
  (prolog))

(test-begin "prolog")
(cond-expand
  (chibi
    (include "scheme/test/helpers.scm")
    (include "scheme/test/core.scm")
    (include "scheme/test/lib.scm"))
  (else
    (include "helpers.scm")
    (include "core.scm")
    (include "lib.scm")))
(test-end "prolog")
