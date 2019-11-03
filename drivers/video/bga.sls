;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; Bochs Graphics Adapter driver

;; XXX: The API makes it appear like there could be multiple instances
;; of this hardware. Maybe QEMU would support it in the future, but
;; for now there is just one graphics adapter.

(library (loko drivers video bga)
  (export
    probe·pci·bga?
    make·pci·bga

    bga-set-mode
    bga-framebuffer-address)
  (import
    (rnrs (6))
    (loko drivers pci)
    (loko system unsafe))

;; I/O registers for the BGA device
(define bga-index #x01ce)
(define bga-data #x01cf)

;; Registers selected by the bga-index port
(define VBE_DISPI_INDEX_ID 0)
(define VBE_DISPI_INDEX_XRES 1)
(define VBE_DISPI_INDEX_YRES 2)
(define VBE_DISPI_INDEX_BPP 3)
(define VBE_DISPI_INDEX_ENABLE 4)
(define VBE_DISPI_INDEX_BANK 5)
(define VBE_DISPI_INDEX_VIRT_WIDTH 6)
(define VBE_DISPI_INDEX_VIRT_HEIGHT 7)
(define VBE_DISPI_INDEX_X_OFFSET 8)
(define VBE_DISPI_INDEX_Y_OFFSET 9)

(define (probe·pci·bga? dev)
  (and (eqv? (pcidev-vendor-id dev) #x1234)
       (eqv? (pcidev-device-id dev) #x1111)
       (eqv? (pci-get-u16 dev PCI-CFG-00-SUBSYSTEM-VENDOR-ID) #x1af4)
       (eqv? (pci-get-u16 dev PCI-CFG-00-SUBSYSTEM-ID) #x1100)
       (fx<=? #xB0C0 (bga-read #f VBE_DISPI_INDEX_ID) #xB0CF)))

(define (make·pci·bga dev)
  ;; BAR 0 is the framebuffer
  (pcibar-base (vector-ref (pcidev-BARs dev) 0)))

(define (bga-framebuffer-address bga)
  bga)

;; Write a BGA register
(define (bga-write bga reg value)
  (put-i/o-u16 bga-index reg)
  (put-i/o-u16 bga-data value))

;; Read a BGA register
(define (bga-read bga reg)
  (put-i/o-u16 bga-index reg)
  (get-i/o-u16 bga-data))

;; Change the BGA graphics mode
(define (bga-set-mode bga width height bit-depth enable-lfb? clear-memory?)
  (bga-write bga VBE_DISPI_INDEX_ENABLE 0)
  (bga-write bga VBE_DISPI_INDEX_XRES width)
  (bga-write bga VBE_DISPI_INDEX_YRES height)
  (bga-write bga VBE_DISPI_INDEX_BPP bit-depth)
  (bga-write bga VBE_DISPI_INDEX_ENABLE
             (fxior 1
                    (if enable-lfb? #x40 0)
                    (if clear-memory? 0 #x80)))))
