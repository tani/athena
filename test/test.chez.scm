(import (rnrs base)
        (srfi :64)
        (srfi :41)
        (prolog)
        (only (chezscheme) include interaction-environment))

(include "test/helpers.scm")
(include "test/core.scm")
(include "test/lib.scm")
