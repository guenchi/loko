;; Loko Scheme sample
;; Copyright © 2019 Göran Weinholt
;; SPDX-License-Identifier: MIT

(library (hello-lib)
  (export hello)
  (import (rnrs))

(define (hello who)
  (display "Hello, ")
  (display who)
  (display "!\n")))
