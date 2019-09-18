;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; USB mass storage driver

;; TODO: It would be good if the SCSI parts of this were separated
;; out, but that can wait until another SCSI driver shows up

(library (loko drivers usb mass-storage)
  (export
    probe·usb·mass-storage?
    driver·usb·mass-storage)
  (import
    (rnrs (6))
    (struct pack)
    (loko system fibers)
    (loko drivers storage)
    (loko drivers usb core))

(define (print . x) (for-each display x) (newline))

(define (intdesc? desc)
  (eqv? (desc-bDescriptorType desc) desctype-INTERFACE))

(define (conf-scsi-mass-storage? conf)
  (exists (lambda (desc)
            (and (intdesc? desc)
                 (eqv? #x08 (intdesc-bInterfaceClass desc))
                 (eqv? #x06 (intdesc-bInterfaceSubClass desc))
                 (eqv? #x50 (intdesc-bInterfaceProtocol desc))))
          conf))

(define (probe·usb·mass-storage? dev)
  (find conf-scsi-mass-storage? (usb-device-$configurations dev)))

(define (driver·usb·mass-storage dev storage-device)
  (cond
    ((find conf-scsi-mass-storage? (usb-device-$configurations dev)) =>
     (lambda (conf)
       (usb-set-configuration dev (cfgdesc-bConfigurationValue (car conf)))
       (usb-mass-storage-driver dev conf storage-device)))
    (else #f)))

(define (usb-mass-storage-driver dev conf storage-device)
  (define interface (find intdesc? conf)) ;XXX: NIT
  (define ep-in #x81)                    ;FIXME: should be found through conf
  (define ep-out #x02)
  (define timeout 100)
  (define req-Get-Max-LUN #xFE)
  (define req-Reset       #xFF)
  (define (get-max-lun dev interface)
    (let ((bv (make-bytevector 1)))
      (usb-control-transfer dev #xA1 req-Get-Max-LUN 0
                            (intdesc-bInterfaceNumber interface) bv timeout)
      (let ((max-lun (bytevector-u8-ref bv 0)))
        (if (eqv? max-lun #xFF) 0 max-lun))))

  (define CBW-SIGNATURE #x43425355)
  (define CSW-SIGNATURE #x53425355)
  (define SCSI-INQUIRE #x12)
  (define SCSI-READ-CAPACITY-10 #x25)
  (define SCSI-READ-10 #x28)
  (define %tag 0)
  (define (scsi-INQUIRY LUN)
    (set! %tag (fxand #x3fffff (fx+ %tag 1)))
    (let ((data (make-bytevector 31))
          (resp (make-bytevector 36)))
      ;; page 17-19
      (pack! "<LLLCCC" data 0 CBW-SIGNATURE %tag (bytevector-length resp)
             #x80 LUN (format-size "!uCxxSx"))
      ;; command block
      (pack! "!uCxxSx" data 15 SCSI-INQUIRE (bytevector-length resp))
      ;; (print "CBW: " data)
      ;; TODO: Check amount transferred, check for errors
      (usb-bulk-transfer dev ep-out data timeout)
      (usb-bulk-transfer dev ep-in resp timeout)
      (let ((CSW (make-bytevector 13)))
        (usb-bulk-transfer dev ep-in CSW timeout)
        (unless (eqv? CSW-SIGNATURE (bytevector-u32-ref CSW 0 (endianness little)))
          (error 'usb-msc-command "Bad signature on CSW"))
        (unless (eqv? %tag (bytevector-u32-native-ref CSW 4))
          (error 'usb-msc-command "Wrong tag on CSW"))
        (unless (eqv? 0 (bytevector-u8-ref CSW 12))
          (error 'usb-msc-command "Bad status" (bytevector-u8-ref CSW 12))))
      resp))

  (define (scsi-READ-CAPACITY_10 LUN)
    (set! %tag (fxand #x3fffff (fx+ %tag 1)))
    (let ((data (make-bytevector 31))
          (resp (make-bytevector 8)))
      (pack! "<LLLCCC" data 0 CBW-SIGNATURE %tag (bytevector-length resp)
             #x80 LUN (format-size "!uCCLxxCC"))
      (pack! "!uCCLxxCC" data 15 SCSI-READ-CAPACITY-10 0 0 0 0)
      ;; (print "CBW: " data)
      ;; TODO: Check amount transferred, check for errors
      (usb-bulk-transfer dev ep-out data timeout)
      (usb-bulk-transfer dev ep-in resp timeout)
      (let ((CSW (make-bytevector 13)))
        (usb-bulk-transfer dev ep-in CSW timeout)
        ;; XXX: check residue?
        (unless (eqv? CSW-SIGNATURE (bytevector-u32-ref CSW 0 (endianness little)))
          (error 'usb-msc-command "Bad signature on CSW"))
        (unless (eqv? %tag (bytevector-u32-native-ref CSW 4))
          (error 'usb-msc-command "Wrong tag on CSW"))
        (unless (eqv? 0 (bytevector-u8-ref CSW 12))
          (error 'usb-msc-command "Bad status" (bytevector-u8-ref CSW 12))))
      resp))

  (define (scsi-READ_10 LUN LBA #;bytevector #;start blocks block-length)
    ;; (print (list 'LUN LUN 'LBA LBA 'blocks blocks))
    (set! %tag (fxand #x3fffff (fx+ %tag 1)))
    (let ((data (make-bytevector 31))
          (resp (make-bytevector (fx* block-length blocks))))
      (pack! "<LLLCCC" data 0 CBW-SIGNATURE %tag (fx* block-length blocks)
             #x80 LUN (format-size "!uCCLxS"))
      (let ((flags 0))
        (pack! "!uCCLxS" data 15 SCSI-READ-10 flags LBA blocks))
      ;; (print "CBW: " data)
      ;; FIXME: Check amount transferred, check for errors
      (usb-bulk-transfer dev ep-out data timeout)
      (usb-bulk-transfer dev ep-in resp timeout)
      (let ((CSW (make-bytevector 13)))
        (usb-bulk-transfer dev ep-in CSW timeout)
        ;; FIXME: check residue?
        (unless (eqv? CSW-SIGNATURE (bytevector-u32-ref CSW 0 (endianness little)))
          (error 'usb-msc-command "Bad signature on CSW"))
        (unless (eqv? %tag (bytevector-u32-native-ref CSW 4))
          (error 'usb-msc-command "Wrong tag on CSW"))
        (unless (eqv? 0 (bytevector-u8-ref CSW 12))
          (error 'usb-msc-command "Bad status" (bytevector-u8-ref CSW 12))))
      resp))

  (print "Welcome to USB Mass Storage Class, Bulk-Only Transport (with SCSI)")
  (print "Max LUN: " (get-max-lun dev interface))
  (print "Sending INQUIRY")
  ;; FIXME: Make every LUN available
  (let* ((LUN 0)
         (inq (scsi-INQUIRY LUN))
         (PDT (fxbit-field (bytevector-u8-ref inq 0) 0 4)))
    (print "INQUIRY resp: " inq)
    (when (eqv? 0 PDT) ;disk
      (let ((cap (scsi-READ-CAPACITY_10 LUN)))
        (let-values ([(lba block-length) (unpack "!LL" cap)])
          (unless (eqv? lba #xFFFFFFFF)
            (print "READ_CAPACITY_10: " lba "·" block-length " = "
                   (* (+ lba 1) block-length) " bytes = "
                   (div (* (+ lba 1) block-length) (* 1024 1024))
                   " MiB")

            ;; (print "READ_10(0,0): ")
            ;; (write (utf8->string (scsi-READ_10 0 0 1 block-length)))
            ;; (newline)

            (let lp ()
              (let ((x (perform-operation
                        (get-operation
                         (storage-device-request-channel storage-device)))))
                (case (car x)
                  ((read)
                   (apply
                    (case-lambda
                      ((ch lba blocks)
                       (let ((resp (scsi-READ_10 LUN lba blocks block-length)))
                         (put-message ch (cons 'ok resp)))))
                    (cdr x))))
                (lp))))))))))
