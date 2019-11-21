;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; SCSI direct-access block devices

;; Reference: SCSI Block Commands - 3 (SBC-3)

(library (loko drivers scsi block)
  (export
    scsi·block·read-capacity
    driver·scsi·block)
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system fibers)
    (loko drivers storage)
    (loko drivers scsi core))

(define SCSI-READ-CAPACITY_10 #x25)
(define SCSI-READ-CAPACITY_16 #x9E)

(define SCSI-READ_10 #x28)
(define SCSI-READ_12 #xA8)
(define SCSI-READ_16 #x88)

(define SCSI-WRITE_10 #x2A)
(define SCSI-WRITE_12 #xAA)
(define SCSI-WRITE_16 #x8A)

(define (!? scsidev msg)
  (let ((resp-ch (make-channel)))
    (put-message (scsi-device-channel scsidev) (cons resp-ch msg))
    (get-message resp-ch)))

(define (scsi·block·read-capacity scsidev)
  (match (!? scsidev (scsi-READ_CAPACITY_10))
    [('ok resp data)
     (let-values ([(last-lba block-length) (unpack "!LL" data)])
       (if (eqv? last-lba #xffffffff)
           (match (!? scsidev (scsi-READ_CAPACITY_16))
             [('ok resp data)
              (let-values ([(last-lba block-length) (unpack "!LL" data)])
                (values last-lba block-length))]
             [('error . _)
              (values last-lba block-length)])
           (values last-lba block-length)))]
    [('error . _)
     (values #f #f)]))

(define (driver·scsi·block scsidev storage)
  (define sector-size (storage-device-logical-sector-size storage))
  (let lp ()
    (match (get-message (storage-device-request-channel storage))
      [(resp-ch 'read lba sectors)
       (let ((data-len (fx* sectors sector-size)))
         (match (!? scsidev (cond ((not (fx<? lba (expt 2 32)))
                                   (scsi-READ_16 lba sectors data-len))
                                  ((fx<? sectors (expt 2 16))
                                   (scsi-READ_10 lba sectors data-len))
                                  (else
                                   (scsi-READ_12 lba sectors data-len))))
           [('ok resp data)
            (put-message resp-ch (list 'ok data))]
           [('error . resp)
            (put-message resp-ch (list 'error #f))]))]

      [(resp-ch . _)
       (put-message resp-ch (list 'error 'unknown-request))])
    (lp)))

;;; "SCSI Block Commands - 2 (SBC-2)"

(define (scsi-READ_CAPACITY_10)
  ;; Returns last-lba, block-length = !LL
  (list 'in (pack "!uCxLxxxx" SCSI-READ-CAPACITY_10 0) 8))

(define (scsi-READ_CAPACITY_16)
  ;; Returns last-lba, block-length, flags = !QLC
  (list 'in (pack "!uCxLxxxx" SCSI-READ-CAPACITY_16 0) 32))

(define (scsi-READ_10 lba blocks data-len)
  (list 'in (pack "!uCxLxSx" SCSI-READ_10 lba blocks) data-len))

(define (scsi-READ_12 lba blocks data-len)
  (list 'in (pack "!uCxLLx" SCSI-READ_12 lba blocks) data-len))

(define (scsi-READ_16 lba blocks data-len)
  (list 'in (pack "!uCxQLx" SCSI-READ_16 lba blocks) data-len)))
