;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; SCSI common stuff

;; Good references are:

;; SCSI Architecture Model - 2 (SAM-2)
;; SCSI Primary Commands - 3 (SPC-3)

(library (loko drivers scsi core)
  (export
    probe·scsi

    ;; Accessors for the inquiry data returned from a SCSI device
    inquiry:peripheral-device-type
    inquiry:peripheral-device-qualifier
    inquiry:rmb?

    make-scsi-device scsi-device?
    scsi-device-channel
    scsi-device-inquiry-data
    )
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system fibers))

(define-record-type scsi-device
  (sealed #t)
  (fields channel
          inquiry-data)
  (protocol
   (lambda (p)
     (lambda (channel inquiry-data)
       (p channel inquiry-data)))))

;; Status codes
(define-condition-type &scsi-status &condition
  make-scsi-status scsi-status?
  (code scsi-status-code))

(define status-codes
  '((#x00 . GOOD)
    (#x02 . CHECK-CONDITION)
    (#x04 . CONDITION-MET)
    (#x08 . BUSY)
    (#x10 . INTERMEDIATE)
    (#x14 . INTERMEDIATE-CONDITION-MET)
    (#x18 . RESERVATION-CONFLICT)
    (#x28 . TASK-SET-FULL)
    (#x30 . ACA-ACTIVE)
    (#x40 . TASK-ABORTED)))

;; Sense key, additional code and additional code qualifier. Quite
;; descriptive if you have a table of codes.
(define-condition-type &scsi-sense &condition
  make-scsi-sense scsi-sense?
  (key scsi-sense-key)
  (key-meaning scsi-sense-key-meaning)
  (asc scsi-sense-asc)
  (ascq scsi-sense-ascq)
  (bytes scsi-sense-bytes))

;; http://www.t10.org/lists/2sensekey.htm
(define sense-key-descriptions
  '#(NO-SENSE
     RECOVERED-ERROR
     NOT-READY
     MEDIUM-ERROR
     HARDWARE-ERROR
     ILLEGAL-REQUEST
     UNIT-ATTENTION
     DATA-PROTECT
     BLANK-CHECK
     VENDOR-SPECIFIC
     COPY-ABORTED
     ABORTED-COMMAND
     #f
     VOLUME-OVERFLOW
     MISCOMPARE
     COMPLETED))

(define-condition-type &scsi-additional-sense &error
  make-scsi-additional-sense scsi-additional-sense?
  (code scsi-additional-sense-code)
  ;; Additional sense code qualifier
  (qualifier scsi-additional-sense-code-qualifier)
  (description scsi-sense-additional-description))

(define (!? ch msg)
  (let ((resp-ch (make-channel)))
    (put-message ch (cons resp-ch msg))
    (get-message resp-ch)))

(define (inquiry:peripheral-device-type x)
  (fxbit-field (bytevector-u8-ref x 0) 0 5))

(define (inquiry:peripheral-device-qualifier x)
  (fxbit-field (bytevector-u8-ref x 0) 5 8))

(define (inquiry:rmb? x)
  (fxbit-set? (bytevector-u8-ref x 1) 7))

;; Gather information about one logical unit. Enough to identify the
;; unit and what driver to use.
(define (probe·scsi request-ch)
  (match (!? request-ch (scsi-INQUIRY 5))
    [('ok _ resp)
     (let ((extra-len (bytevector-u8-ref resp 4)))
       (match (!? request-ch (scsi-INQUIRY (fx+ 5 extra-len)))
         [('ok _ resp)
          (let ((vpd-id (scsi-get-vpd request-ch VPD-DEVICE-IDENTIFICATION))
                (driver-type
                 (case (inquiry:peripheral-device-type resp)
                   ((#x00 #x04 #x07) 'SBC)
                   ((#x05) 'MMC)
                   (else #f))))
            (list driver-type resp vpd-id))]
         [('error . _)
          'unknown/no-device]))]
    [('error . x)
     'unknown/no-device]))

;; Get sense data. "Sense" is the extended status information.
(define (scsi-get-sense request-ch)
  (define normal-length (format-size "!uCxCLCLCCC CS"))
  (match (!? request-ch (scsi-REQUEST-SENSE normal-length))
    [('ok _ resp)
     (let ((extra-len (fx- (fx+ (bytevector-u8-ref resp 7) 8) normal-length)))
       (match (if (eqv? extra-len 0)
                  (list 'ok #f resp)
                  (!? request-ch (scsi-REQUEST-SENSE (fx+ normal-length extra-len))))
         [('ok _ resp)
          (case (fxbit-field (bytevector-u8-ref resp 0) 0 7)
            ((#x70 #x71)
             (let ((key (bytevector-u8-ref resp 2))
                   (asc (bytevector-u8-ref resp 12))
                   (ascq (bytevector-u8-ref resp 13)))
               (make-scsi-sense key (vector-ref sense-key-descriptions key)
                                asc ascq resp)))
            ((#x72 #x73)
             (let ((key (bytevector-u8-ref resp 1))
                   (asc (bytevector-u8-ref resp 2))
                   (ascq (bytevector-u8-ref resp 3)))
               (make-scsi-sense key (vector-ref sense-key-descriptions key)
                                asc ascq resp)))
            (else
             (make-scsi-sense #f #f #f resp)))]))]))

;; Get vital product data.
(define (scsi-get-vpd request-ch page)
  (match (!? request-ch (scsi-INQUIRY/vital-product-data 254 page))
    [('ok _ resp)
     (let* ([len (fx+ (unpack "!uxxS" resp) (format-size "!uCCS"))]
            [ret (make-bytevector len)])
       (bytevector-copy! resp 0 ret 0 (fxmin (bytevector-length resp)
                                             (bytevector-length ret)))
       ret)]
    [('error . x)
     ;; (write (cons 'error0 x))
     ;; (newline)
     #f]))

;;; Commands usable with all SCSI devices

(define SCSI-INQUIRY #x12)
(define SCSI-REQUEST-SENSE #x03)
(define SCSI-TEST-UNIT-READY #x00)

(define (scsi-INQUIRY len)
  (list 'in (pack "!uCxxxCx" SCSI-INQUIRY len) len))

(define VPD-SUPPORTED-VPD-PAGES    #x00)
(define VPD-DEVICE-IDENTIFICATION  #x83)

(define (scsi-INQUIRY/vital-product-data len page)
  ;; XXX: QEMU's ATAPI emulation truncates the length and returns an
  ;; error if too few bytes are requested
  (list 'in (pack "!uCCCxC" SCSI-INQUIRY #b01 page len) len))

(define (scsi-REQUEST-SENSE len)
  (list 'in (pack "!uCxxxCx" SCSI-REQUEST-SENSE len) len))

(define (scsi-TEST-UNIT-READY)
  (list 'non-data (pack "!uC4x" SCSI-TEST-UNIT-READY)))

)
