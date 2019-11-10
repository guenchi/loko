;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; ATA/ATAPI common definitions

(library (loko drivers ata core)
  (export
    probe·ata

    make-ata-controller ata-controller?
    ata-controller-notify-channel

    make-ata-device ata-device?
    ata-device-controller
    ata-device-channel
    ata-device-identify-block

    ;; Flags in responses
    ata-error-ICRC
    ata-error-UNC
    ata-error-IDNF
    ata-error-ABRT
    ata-error-MED
    ata-device-obs
    ata-device-LBA
    ata-device-DRV
    ata-status-BSY
    ata-status-DRDY
    ata-status-DF
    ata-status-SERV
    ata-status-DRQ
    ata-status-CORR
    ata-status-IDX
    ata-status-ERR
    ata-count-C/D
    ata-count-I/O
    ata-count-REL)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko drivers ata identify))

(define-record-type ata-controller
  (sealed #t)
  (fields
   ;; Notifications from the controller driver:
   ;; ('new-channel . channel)
   notify-channel)
  (protocol
   (lambda (p)
     (lambda ()
       (p (make-channel))))))

(define-record-type ata-device
  (sealed #t)
  (fields class                         ;ata or atapi
          controller
          channel
          identify-block
          logical-sector-size)          ;512 (but can be 520, 528)
  (protocol
   (lambda (p)
     (lambda (controller channel identify-block)
       (let-values ([(logical _physical) (ata:identify:sector-size identify-block)])
         (p controller channel identify-block logical))))))

;; Probe an ATA/ATAPI device to see what lurks behinds the surface.
;; Either an ATA disk or some SCSI device behind ATAPI.
(define (probe·ata channel)
  (let ((resp-ch (make-channel)))
    (put-message channel (cons resp-ch (ata-IDENTIFY-DEVICE)))
    (match (get-message resp-ch)
      [('ok resp data)
       (cons 'ata data)]
      [('error #(_error count lba status))
       ;; Check for the PACKET feature set signature
       (cond ((and (eqv? (fxbit-field count 0 8) #x01)
                   (eqv? (fxbit-field lba 0 24) #xEB1401))
              (put-message channel (cons resp-ch (ata-IDENTIFY-PACKET-DEVICE)))
              (match (get-message resp-ch)
                [('ok resp data)
                 (cons 'atapi data)]
                [('error err)
                 (list 'unknown-device count lba)]))
             (else
              (if (eqv? status 0)
                  'no-device
                  (list 'unknown-device count lba))))])))

(define (!? dev msg)
  (let ((resp-ch (make-channel)))
    (put-message (ata-device-channel dev) (cons resp-ch msg))
    (get-message resp-ch)))

;; Flag definitions

(define-syntax define-inlined
  (syntax-rules ()
    ((_ name v)
     (define-syntax name (identifier-syntax v)))))

(define-inlined ata-error-ICRC   #b10000000) ;Interface CRC
(define-inlined ata-error-UNC    #b01000000) ;Uncorrectable Error
(define-inlined ata-error-IDNF   #b00010000) ;ID Not Found
(define-inlined ata-error-ABRT   #b00000100) ;Abort
(define-inlined ata-error-MED    #b00000001) ;Media Error

(define-inlined ata-device-obs   #b10100000) ;obsolete, should be set
(define-inlined ata-device-LBA   #b01000000) ;use LBA
(define-inlined ata-device-DRV   #b00010000) ;drive select

(define-inlined ata-status-BSY   #b10000000) ;busy
(define-inlined ata-status-DRDY  #b01000000) ;drive ready
(define-inlined ata-status-DF    #b00100000) ;device fault
(define-inlined ata-status-SERV  #b00010000) ;service
(define-inlined ata-status-DRQ   #b00001000) ;data request
(define-inlined ata-status-CORR  #b00000100) ;corrected data
(define-inlined ata-status-IDX   #b00000010) ;index bit
(define-inlined ata-status-ERR   #b00000001) ;error

;; Interrupt reasons
(define-inlined ata-count-C/D #b001)    ;Command/data
(define-inlined ata-count-I/O #b010)    ;Input/Output (0 = to device)
(define-inlined ata-count-REL #b100)    ;Release

;;; ATA commands

(define (make-inputs feature sector-count lba device command)
  (vector feature sector-count lba device command))

(define (make-cmd-non-data inputs)
  (list '(non-data) inputs))

(define (make-cmd-pio-data-in data-len inputs)
  (list (list 'pio-data-in data-len) inputs))

(define (make-cmd-pio-data-out bytevector inputs)
  (list (list 'pio-data-out bytevector) inputs))

(define (make-cmd-dma-data-in data-len inputs)
  (list (list 'dma-data-in data-len) inputs))

(define (make-cmd-dma-data-out bytevector inputs)
  (list (list 'dma-data-out bytevector) inputs))

(define ata-cmd-flush-cache             #xE7)
(define ata-cmd-identify-device         #xEC)
(define ata-cmd-identify-packet-device  #xA1)
(define ata-cmd-read-sectors            #x20)
(define ata-cmd-read-dma                #xC8)
(define ata-cmd-read-dma-ext            #x25)
(define ata-cmd-set-features            #xEF)
(define ata-cmd-set-multiple            #xC6)
(define ata-cmd-write-dma               #xCA)
(define ata-cmd-write-multiple          #xC3)
(define ata-cmd-write-sectors           #x30)

;;; The general feature set

;; ata-EXECUTE-DEVICE-DIAGNOSTICS

(define (ata-FLUSH-CACHE _dev)
  (make-cmd-non-data (make-inputs 0 0 0 0 ata-cmd-flush-cache)))

(define (ata-IDENTIFY-DEVICE)
  (make-cmd-pio-data-in 512 (make-inputs 0 0 0 0 ata-cmd-identify-device)))

(define (ata-READ-DMA dev lba sectors)
  (assert (fx<=? 1 sectors 256))
  (assert (eqv? lba (fxbit-field lba 0 28)))
  (let ((sectors (if (eqv? sectors 256) 0 sectors)))
    (make-cmd-dma-data-in (fx* sectors (ata-device-logical-sector-size dev))
                          (make-inputs 0 sectors (fxbit-field lba 0 28)
                                       (fxarithmetic-shift-left ata-device-LBA 8)
                                       ata-cmd-read-dma))))

;; ata-READ-MULTIPLE

(define (ata-READ-SECTORS dev lba sectors)
  (assert (fx<=? 1 sectors 256))
  (assert (eqv? lba (fxbit-field lba 0 28)))
  (let ((sectors (if (eqv? sectors 256) 0 sectors)))
    (make-cmd-pio-data-in (fx* sectors (ata-device-logical-sector-size dev))
                          (make-inputs 0 sectors (fxbit-field lba 0 28)
                                       (fxarithmetic-shift-left ata-device-LBA 8)
                                       ata-cmd-read-sectors))))

;; ata-READ-VERIFY-SECTORS
;; ata-SET-FEATURES
;; ata-SET-MULTIPLE-MODE
;; ata-WRITE-DMA
;; ata-WRITE-MULTIPLE
;; ata-WRITE-SECTORS

;; ata-DOWNLOAD-MICROCODE
;; ata-NOP
;; ata-READ-BUFFER
;; ata-WRITE-BUFFER
;; ata-WRITE-UNCORRECTABLE

;;; Extra commans for the PACKET feature set

;; ata-PACKET
;; ata-DEVICE-RESET

(define (ata-IDENTIFY-PACKET-DEVICE)
  (make-cmd-pio-data-in 512 (make-inputs 0 0 0 0 ata-cmd-identify-packet-device)))

;;; 48-bit feature set (feature:LBA48)

;; FLUSH CACHE EXT

(define (ata-READ-DMA-EXT dev bufs lba sector-count)
  (assert (fx<=? 1 sector-count 65536))
  (assert (eqv? lba (fxbit-field lba 0 48)))
  (assert (for-all pair? bufs))
  (let ((sector-count (if (eqv? sector-count 65536) 0 sector-count)))
    (list 'dma-in bufs
          (make-inputs 0 sector-count lba
                       (fxarithmetic-shift-left ata-device-LBA 8)
                       ata-cmd-read-dma-ext))))

;; READ DMA QUEUED EXT
;; READ MULTIPLE EXT
;; READ NATIVE MAX ADDRESS EXT
;; READ SECTOR(S) EXT
;; READ VERIFY SECTOR(S) EXT
;; SET MAX ADDRESS EXT
;; WRITE DMA EXT
;; WRITE DMA FUA EXT
;; WRITE DMA QUEUED EXT
;; WRITE DMA QUEUED FUA EXT
;; WRITE MULTIPLE EXT
;; WRITE MULTIPLE FUA EXT
;; WRITE SECTOR(S) EXT

)
