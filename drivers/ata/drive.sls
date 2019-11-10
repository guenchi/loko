;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; ATA drive driver

(library (loko drivers ata drive)
  (export
    driver·ata·drive)
  (import
    (rnrs (6))
    (loko system fibers))

(define (driver·ata·drive dev identify)
  (let lp ()
    (sleep 1)
    (lp))))
