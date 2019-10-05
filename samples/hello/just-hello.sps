#!/usr/bin/env scheme-script
;; Loko Scheme sample
;; Copyright © 2019 Göran Weinholt
;; SPDX-License-Identifier: MIT

;; Free-standing "Hello world" program

(import
  (except (rnrs base) list cons)
  (loko system unsafe)
  ;; XXX: The primitives are not guaranteed to be stable between releases
  (loko system $primitives))

(define (display x) ($debug-display x))
(define (exit x) (syscall 60 x))

(display 'hello)
(display #\space)
(display 'world)
(display #\newline)
(exit 0)
