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
    (loko drivers ata core))

(define (!? atadev msg)
  (let ((resp-ch (make-channel)))
    (put-message (ata-device-channel atadev) (cons resp-ch msg))
    (get-message resp-ch)))

;; SCSI stuff to move out of here
(define SCSI-INQUIRE #x12)
(define SCSI-READ-CAPACITY-10 #x25)
(define SCSI-READ-10 #x28)


(define (driver·ata·atapi atadev)
  (define inq
    (pack "!uCxxSx" SCSI-INQUIRE 36))
  (define read0
    (let ((flags 0)
          (blocks 1)
          (LBA 16))
      (pack "!uCCLxS" SCSI-READ-10 flags LBA blocks)))
  (match (!? atadev (ata-PACKET/in atadev inq 36))
    [('ok resp inq-data)
     (write (list 'ATAPI inq-data))
     (newline)
     (unless (eqv? (bytevector-u8-ref inq-data 0) 5)
       (error 'driver·ata·atapi "No a DVD/CD-ROM drive"))]
    [((or 'ata-error 'error) . x)
     (write (list 'ATAPI-error x))
     (newline)])

  (sleep 3)
  (display "reading sector...\n")
  (match (!? atadev (ata-PACKET/in atadev read0 2048))
    [('ok resp sector-data)
     (write (list 'ATAPI-data sector-data))
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
