;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; PCI expansion ROMs

(library (loko drivers pci roms)
  (export
    ;; Get the whole ROM from the PCI device
    pcidev-get-ROM

    ;; Parse a ROM extracted by pcidev-get-ROM. Returns pcirom
    ;; records.
    pci-parse-ROM

    pcirom-image                        ;bytevector with cod
    pcirom-vendor-id                    ;supported vendor id
    pcirom-device-ids                   ;supported device ids
    pcirom-header-revision
    pcirom-class-code
    pcirom-revision-level
    pcirom-code-type                    ;one of the types below
    pcirom-checksum-ok?

    ROM-CODE-TYPE-x86
    ROM-CODE-TYPE-OpenFirmware
    ROM-CODE-TYPE-PARISC
    ROM-CODE-TYPE-EFI)
  (import
    (rnrs (6))
    (struct pack)
    (loko drivers pci)
    (loko system unsafe))

;; Save the BARs for the dynamic extent of thunk
(define (with-BARs-saved dev thunk)
  ;; Get a PCI device's BARs
  (define (pcidev-get-raw-BARs dev)
    (case (pcidev-header-layout dev)
      ((#x00)
       (vector (pci-get-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-0)
               (pci-get-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-1)
               (pci-get-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-2)
               (pci-get-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-3)
               (pci-get-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-4)
               (pci-get-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-5)
               (pci-get-u32 dev PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS)))
      (else #f)))
  ;; Put back the saved BARs in reverse order, ROM first (which will
  ;; disable it first, which is good)
  (define (pcidev-restore-raw-BARs dev data)
    (case (pcidev-header-layout dev)
      ((#x00)
       (pci-put-u32 dev PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS (vector-ref data 6))
       (pci-put-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-5 (vector-ref data 5))
       (pci-put-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-4 (vector-ref data 4))
       (pci-put-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-3 (vector-ref data 3))
       (pci-put-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-2 (vector-ref data 2))
       (pci-put-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-1 (vector-ref data 1))
       (pci-put-u32 dev PCI-CFG-00-BASE-ADDRESS-REGISTER-0 (vector-ref data 0)))))
  (let ((old (pcidev-get-raw-BARs dev)))
    (define (swap!)
      (let ((temp old))
        (set! old (pcidev-get-raw-BARs dev))
        (pcidev-restore-raw-BARs dev temp)))
    (dynamic-wind swap! thunk swap!)))

;; Use a command word for the dynamic extended of thunk
(define (with-command-word dev command thunk)
  (define (swap!)
    (let ((temp (pci-get-u16 dev PCI-CFG-COMMAND)))
      (pci-put-u16 dev PCI-CFG-COMMAND command)
      (set! command temp)))
  (dynamic-wind swap! thunk swap!))

;; Disable the BAR, hopefully preventing the device from decoding bus
;; accesses in this range.
(define (pcibar-disable! dev BAR)
  (let ((reg (pcibar-reg BAR)))
    (case (and (pcibar-mem? BAR) (pcibar-mem-type BAR))
      ((64)
       ;; Most machines are likely to have a difficult time sending
       ;; out an address like this on the bus.
       (pci-put-u32 dev reg #xFFFFFFFF)
       (pci-put-u32 dev (fx+ reg 4) #xFFFFFFFF))
      (else
       ;; 20/32-bit memory BARs and I/O. Hopefully this stops most
       ;; devices from decoding anything.
       (pci-put-u32 dev reg #xFFFFFFFF)))))

;; Map the expansion ROM into memory and return its address. This
;; should only run inside with-BARs-saved. Other BARs may stop working
;; during this procedure (both because we remap it and because the
;; hardware can reuse their decoding logic).
(define (pcidev-map-ROM dev)
  (let* ((rom-bar (pci-get-u32 dev PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS))
         (addr (fxand rom-bar (fxnot #b1111111111))))
    (cond ((not (eqv? 0 addr))
           (pci-put-u32 dev PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS (fxior 1 rom-bar))
           addr)
          (else
           ;; This is tricky. We're allowed to put the expansion ROM
           ;; anywhere in memory, but the field is only 32 bits wide
           ;; (and the lower 10 bits reserved). That memory is already
           ;; taken by RAM or other PCI devices. So this looks for
           ;; another BAR that can be temporarily disabled.

           ;; Some devices will not provide a BAR for this purposes,
           ;; so then we'd need to check for another usable address,
           ;; but that is not done here (it is complicated and
           ;; involves checking the bridges).
           (let ((BARs (pcidev-BARs dev)))
             (let lp ((i 0))
               (cond ((fx=? i (vector-length BARs))
                      (raise-continuable
                        (condition
                         (make-warning)
                         (make-who-condition 'pcidev-map-ROM)
                         (make-message-condition
                          "Could not find a BAR to steal for the Expansion ROM")
                         (make-irritants-condition (list dev))))
                      #f)
                     (else
                      (cond
                        ((vector-ref BARs i) =>
                         (lambda (BAR)
                           (cond ((and (pcibar-mem? BAR)
                                       (eqv? (bitwise-arithmetic-shift-right (pcibar-base BAR) 32)
                                             0)
                                       (>= (pcibar-size BAR) (pcidev-ROM-size dev)))
                                  (pcibar-disable! dev BAR)
                                  (pci-put-u32 dev PCI-CFG-00-EXPANSION-ROM-BASE-ADDDRESS
                                               (fxior 1 (pcibar-base BAR)))
                                  (pcibar-base BAR))
                                 (else
                                  (lp (+ i 1))))))
                        (else
                         (lp (+ i 1))))))))))))

;; Get the whole ROM. Note that this may contain duplicates of the
;; images in the ROM, but it also lets us read any trailing data.
;; Should be fun for people who collect ROMs.
(define (pcidev-get-ROM dev)
  (define cmd (pci-get-u16 dev PCI-CFG-COMMAND))
  (with-command-word dev (fxior (fxand cmd (fxnot (fxior PCI-CMD-I/O-SPACE
                                                         PCI-CMD-BUS-MASTER)))
                                (fxior PCI-CMD-INTERRUPT-DISABLE
                                       PCI-CMD-MEM-SPACE))
    (lambda ()
      (with-BARs-saved dev
        (lambda ()
          (let ((size (pcidev-ROM-size dev)))
            (if (not size)
                #vu8()
                ;; FIXME: Since the stolen BAR could be pre-fetchable,
                ;; does this need to flush the cache before reading?
                (let ((addr (pcidev-map-ROM dev)))
                  (if (not addr)
                      #vu8()
                      (do ((addr addr (fx+ addr 4))
                           (bv (make-bytevector size))
                           (i 0 (fx+ i 4)))
                          ((fx=? i (bytevector-length bv))
                           bv)
                        (bytevector-u32-native-set! bv i (get-mem-u32 addr))))))))))))

(define ROM-CODE-TYPE-x86 0)
(define ROM-CODE-TYPE-OpenFirmware 1)
(define ROM-CODE-TYPE-PARISC 2)
(define ROM-CODE-TYPE-EFI 3)

;; A PCI Expansion ROM image. If code-type is ROM-TYPE-x86 then you
;; can place the image at C000:0 and do a far call to C000:3.
(define-record-type pcirom
  (sealed #f)
  (fields image
          ;; Part of the PCI Data Structure
          vendor-id
          device-ids
          header-revision
          class-code
          revision-level
          code-type))

(define (pcirom-checksum-ok? rom)
  (let ((bv (pcirom-image rom)))
    (do ((i 0 (fx+ i 1))
         (c 0 (fxand (fx+ c (bytevector-u8-ref bv i)) #xff)))
        ((fx=? i (bytevector-length bv))
         (fxzero? c)))))

;; Parse a ROM extracted by pcidev-get-ROM. Returns a list of pcirom records.
(define (pci-parse-ROM bv)
  (define PCIR #x52494350)              ;"PCIR"
  (define (get-pci-data p position)
    (set-port-position! p position)
    ;; TODO: PCI Firmware spec rev 3.0 adds some fields. The only one
    ;; supported from those is the device id list.
    (let-values ([(sig vendor-id device-id dev-list-offset
                       header-length header-revision
                       cls2 cls1 cls0   ;24-bit class code
                       image-length revision-level code-type indicator)
                  (get-unpack p "<L SSS SC CCC SSCC")])
      (and (eqv? sig PCIR)
           (let ((device-ids
                  (or (and (not (eqv? dev-list-offset 0))
                           (fx>=? header-revision 3)
                           (cons device-id (get-device-list p (fx+ position dev-list-offset))))
                      (list device-id))))
             (values
               (list vendor-id
                     device-ids
                     header-revision
                     (fxior (fxarithmetic-shift-left cls2 16)
                            (fxarithmetic-shift-left cls1 8)
                            cls0)
                     revision-level
                     code-type)
               (fx* image-length 512)
               (fxbit-set? indicator 7))))))
  (define (get-device-list p position)
    (set-port-position! p position)
    (let lp ((ids '()))
      (let ((device-id (get-unpack p "<S")))
        (if (eqv? device-id 0)
            (reverse ids)
            (lp (cons device-id ids))))))
  (call-with-port (open-bytevector-input-port bv)
    (lambda (p)
      (let lp ((images '()))
        (if (port-eof? p)
            (reverse images)
            (let ((offset (port-position p)))
              (cond ((not (eqv? #xAA55 (get-unpack p "<S")))
                     (reverse images))
                    (else
                     (get-bytevector-n p #x16)
                     (let-values ([(pci-data image-length last-image?)
                                   (get-pci-data p (fx+ offset (get-unpack p "<S")))])
                       (set-port-position! p offset)
                       (let* ((image (get-bytevector-n p image-length))
                              (images (cons (apply make-pcirom image pci-data)
                                            images)))
                         (if last-image?
                             (reverse images)
                             (lp images)))))))))))))
