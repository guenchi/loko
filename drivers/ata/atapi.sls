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
  (define inq
    (pack "!uCxxSx" SCSI-INQUIRE 36))
  (define read-sector16
    (let ((flags 0)
          (blocks 1)
          (LBA 16))
      (pack "!uCCLxS" SCSI-READ-10 flags LBA blocks)))
  (define packet-length (ata-identify:atapi-packet-length
                         (ata-device-identify-block atadev)))

  ;; FIXME: This driver should convert to 12/16 byte packets; not the
  ;; transport driver

  (match (!? atadev (ata-PACKET/in atadev inq 36))
    [('ok resp inq-data)
     (write (list 'ATAPI inq-data))
     (newline)
     (unless (eqv? (bytevector-u8-ref inq-data 0) 5)
       (error 'driver·ata·atapi "Not a DVD/CD-ROM drive"))]
    [((or 'ata-error 'error) . x)
     (write (list 'ATAPI-error x))
     (newline)])

  (sleep 3)

  (match (!? atadev (ata-PACKET/in atadev read-sector16 2048))
    [('ok resp sector-data)
     (write (list 'ATAPI-sector-16: (utf8->string sector-data)))
     (newline)]
    [('ata-error resp . _)
     (write (list 'ATAPI-error resp))
     (newline)
     (match resp
       [#(error count lba status)

        (let ((sense-key (fxbit-field error 4 8)))
          (write (list 'sense-key sense-key))
          (newline))]

       [_ #f])]
    [('error . x)
     (write (list 'ATAPI-error-x x))
     (newline)])

  (let lp ()
    (sleep 10)
    (lp))))
