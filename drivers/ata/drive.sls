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
  ;; (write (!? atadev (ata-READ-DMA atadev 0 1)))
  ;; (newline)
  ;;(write (!? atadev (ata-READ-SECTORS dev 0 1)))

  (let lp ()
    (match (get-message (storage-device-request-channel storage))
      [('read resp-ch lba sectors)
       ;; FIXME: the device might not support DMA?
       (write (list lba sectors))
       (newline)
       (match (!? atadev (ata-READ-DMA atadev lba sectors))
         [('ok resp data)
          (put-message resp-ch (list 'ok data))]
         [((or 'ata-error 'error) resp)
          (put-message resp-ch (list 'error #f))])])
    (lp))))
