(import (scheme base)
        (srfi 64)
        (scheme r5rs)
        (prolog))
(cond-expand
  (gauche
   (import (only (gauche base) current-module))
   (current-lisp-environment (current-module)))
  (else
   (current-lisp-environment (interaction-environment))))

(cond-expand
  (chibi (include "test/test.scm"))
  (else  (include "test.scm")))
