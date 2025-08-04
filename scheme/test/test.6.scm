(import
  (rnrs base)
  (srfi :64)
  (srfi :41)
  (prolog)
  (only (chezscheme) include interaction-environment))

(test-begin "prolog")
(include "scheme/test/helpers.scm")
(include "scheme/test/core.scm")
(include "scheme/test/lib.scm")
(test-end "prolog")
