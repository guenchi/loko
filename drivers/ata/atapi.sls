;; -*- mode: scheme; coding: utf-8 -*-;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; ATAPI device driver

(library (loko drivers ata atapi)
  (export
    driver·ata·atapi)
  (import
    (rnrs (6))
    (loko system fibers))

(define (driver·ata·atapi dev identify)
  (let lp ()
    (sleep 1)
    (lp))))
