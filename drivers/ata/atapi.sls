;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; ATAPI device driver

(library (loko drivers ata atapi)
  (export
    driver·ata·atapi)
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system fibers)
    (loko drivers ata core)
    (loko drivers ata identify))

(define (!? atadev msg)
  (let ((resp-ch (make-channel)))
    (put-message (ata-device-channel atadev) (cons resp-ch msg))
    (get-message resp-ch)))

;; SCSI stuff to move out of here
(define SCSI-INQUIRE #x12)
(define SCSI-READ-CAPACITY-10 #x25)
(define SCSI-READ-10 #x28)

(define (driver·ata·atapi atadev scsi-req-ch)
  (define packet-length (ata-identify:atapi-packet-length
                         (ata-device-identify-block atadev)))
  (let lp ()
    (match (get-message scsi-req-ch)

      [(resp-ch 'in (? bytevector? cdb) data-len)
       (let ((buf (make-bytevector packet-length 0)))
         (bytevector-copy! cdb 0 buf 0 (fxmin (bytevector-length cdb)
                                              packet-length))
         ;; FIXME: Get the status properly
         (match (!? atadev (ata-PACKET/in atadev buf data-len))
           [('ok resp data)
            (put-message resp-ch (list 'ok #f data))]

           [('ata-error resp . _)
            (match resp
              [#(error count lba status)
               (let ((sense-key (fxbit-field error 4 8)))
                 (put-message resp-ch (list 'error sense-key (vector error count lba status))))])]

           [('error . x)
            (put-message resp-ch (list 'error 'TODO))]))]

      [(resp-ch 'non-data (? bytevector? cdb))
       (let ((buf (make-bytevector packet-length 0)))
         (bytevector-copy! cdb 0 buf 0 (fxmin (bytevector-length cdb)
                                              packet-length))
         ;; FIXME: Get the status properly
         (match (!? atadev (ata-PACKET/non-data atadev buf))
           [('ok resp)
            (put-message resp-ch (list 'ok #f))]

           [('ata-error resp . _)
            (match resp
              [#(error count lba status)
               (let ((sense-key (fxbit-field error 4 8)))
                 (put-message resp-ch (list 'error sense-key (vector error count lba status))))])]

           [('error . x)
            (put-message resp-ch (list 'error 'TODO))]))]

      [(resp-ch . x)
       (put-message resp-ch (list 'error 'bad-request))])
    (lp))))
