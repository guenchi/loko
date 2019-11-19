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
    (loko match)
    (loko system fibers)
    (loko drivers ata core)
    (loko drivers storage))

(define (!? atadev msg)
  (let ((resp-ch (make-channel)))
    (put-message (ata-device-channel atadev) (cons resp-ch msg))
    (get-message resp-ch)))

(define (driver·ata·drive atadev storage)
  (define sector-size (storage-device-logical-sector-size storage))
  (let lp ()
    (match (get-message (storage-device-request-channel storage))
      [(resp-ch 'read lba sectors)
       (match (!? atadev (if (fx<? lba (expt 2 28))
                             (ata-READ-DMA atadev lba sectors)
                             (ata-READ-DMA-EXT atadev lba sectors)))
         [('ok resp data)
          (put-message resp-ch (list 'ok data))]
         [((or 'ata-error 'error) . resp)
          (put-message resp-ch (list 'error #f))])]

      [(resp-ch 'write lba data)
       (let ((sectors (fxdiv (bytevector-length data) sector-size)))
         (match (!? atadev (if (fx<? lba (expt 2 28))
                               (ata-WRITE-DMA atadev lba sectors data)
                               (ata-WRITE-DMA-EXT atadev lba sectors data)))
           [('ok resp)
            (put-message resp-ch (list 'ok))]
           [((or 'ata-error 'error) . resp)
            (put-message resp-ch (list 'error #f))]))]

      [(resp-ch . _)
       (put-message resp-ch (list 'error 'unknown-request))])
    (lp))))
