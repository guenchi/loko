;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; VESA SuperVGA BIOS driver

(library (loko drivers video vbe)
  (export
    probe·pci·vbe?
    make·pci·vbe
    vbe-bios
    vbe-supervga-info

    supervga-info?
    supervga-info-VbeVersionMajor
    supervga-info-VbeVersionMinor
    supervga-info-OemString
    supervga-info-Capabilities
    supervga-info-VideoModes
    supervga-info-TotalMemory
    supervga-info-OemSoftwareRev
    supervga-info-OemVendorName
    supervga-info-OemProductName
    supervga-info-OemProductRev

    supervga-mode-Number
    supervga-mode-ModeAttributes
    supervga-mode-WinAAttributes
    supervga-mode-WinBAttributes
    supervga-mode-WinGranularity
    supervga-mode-WinSize
    supervga-mode-WinASegment
    supervga-mode-WinBSegment
    supervga-mode-WinFuncPtr
    supervga-mode-BytesPerScanLine
    supervga-mode-XResolution
    supervga-mode-YResolution
    supervga-mode-XCharSize
    supervga-mode-YCharSize
    supervga-mode-NumberOfPlanes
    supervga-mode-BitsPerPixel
    supervga-mode-NumberOfBanks
    supervga-mode-MemoryModel
    supervga-mode-BankSize
    supervga-mode-NumberOfImagePages
    supervga-mode-RedMaskSize
    supervga-mode-RedFieldPosition
    supervga-mode-GreenMaskSize
    supervga-mode-GreenFieldPosition
    supervga-mode-BlueMaskSize
    supervga-mode-BlueFieldPosition
    supervga-mode-RsvdMaskSize
    supervga-mode-RsvdFieldPosition
    supervga-mode-DirectColorModeInfo
    supervga-mode-PhysBasePtr
    supervga-mode-LinBytesPerScanLine
    supervga-mode-BnkNumberOfImagePages
    supervga-mode-LinNumberOfImagePages
    supervga-mode-LinRedMaskSize
    supervga-mode-LinRedFieldPosition
    supervga-mode-LinGreenMaskSize
    supervga-mode-LinGreenFieldPositiondb
    supervga-mode-LinBlueMaskSize
    supervga-mode-LinBlueFieldPosition
    supervga-mode-LinRsvdMaskSize
    supervga-mode-LinRsvdFieldPosition
    supervga-mode-MaxPixelClock

    ;; Helpers for ModeAttributes
    supervga-mode-supported?
    supervga-mode-tty-supported?
    supervga-mode-color-mode?
    supervga-mode-graphics-mode?
    supervga-mode-vga-compatible?
    supervga-mode-vga-windowing?
    supervga-mode-linear-framebuffer?
    supervga-mode-double-scan-available?
    supervga-mode-interlaced-available?
    supervga-mode-triple-buffering?
    supervga-mode-stereo-display?
    supervga-mode-dual-display-start?

    ;; Alist with values for MemoryModel
    supervga-memory-models

    make-crtc-info
    vesa-get-mode-info
    vesa-set-mode
    vesa-get-mode
    vesa-get-logical-scan-line-length
    vesa-get-maximum-scan-line-length
    vesa-set-logical-scan-line-length/pixels
    vesa-set-logical-scan-line-length/bytes
    vesa-set-display-start
    vesa-get-pixel-clock

    vga-set-video-mode
    vga-put-char)
  (import
    (rnrs (6))
    (loko drivers pci)
    (loko drivers pci roms)
    (loko system fibers)
    (loko system unsafe)
    (struct pack)
    (zabavno cpu x86))

(define (probe·pci·vbe? dev)
  (and (eqv? (pcidev-base-class dev) #x03)
       (eqv? (pcidev-sub-class dev) #x00)
       (eqv? (pcidev-interface dev) #x00)
       (pcidev-ROM-size dev)))

(define (make·pci·vbe dev)
  (let* ((img* (pci-parse-ROM (pcidev-get-ROM dev)))
         (img (find (lambda (img)
                       (and (pcirom-checksum-ok? img)
                            (eqv? ROM-CODE-TYPE-x86 (pcirom-code-type img))
                            (eqv? (pcidev-vendor-id dev) (pcirom-vendor-id img))
                            (memv (pcidev-device-id dev) (pcirom-device-ids img))))
                    img*)))
    (when (not img)
      (error 'driver·pci·vbe "Could not find a suitable extension ROM image" dev))
    ;; Enable I/O and memory
    (let ((cmd (pci-get-u16 dev PCI-CFG-COMMAND)))
      (pci-put-u16 dev PCI-CFG-COMMAND (fxior cmd PCI-CMD-MEM-SPACE
                                              PCI-CMD-I/O-SPACE)))
    (let* ((BIOS (video-bios-init dev img))
           (info (vesa-init BIOS)))
      (make-vbe BIOS info))))

(define-record-type vbe
  (sealed #t)
  (fields bios supervga-info))

;; Information about the BIOS and its supported modes
(define-record-type supervga-info
  (sealed #t)
  (fields VbeVersionMajor
          VbeVersionMinor
          OemString
          Capabilities
          VideoModes
          TotalMemory
          OemSoftwareRev
          OemVendorName
          OemProductName
          OemProductRev))

;;; SuperVGA mode information

(define-record-type supervga-mode
  (sealed #t)
  (fields Number
          ;; 1.0/1.1
          ModeAttributes
          WinAAttributes
          WinBAttributes
          WinGranularity
          WinSize
          WinASegment
          WinBSegment
          WinFuncPtr
          BytesPerScanLine
          ;; VBE 1.2
          XResolution
          YResolution
          XCharSize
          YCharSize
          NumberOfPlanes
          BitsPerPixel
          NumberOfBanks
          MemoryModel
          BankSize
          NumberOfImagePages
          ;; Direct color
          RedMaskSize
          RedFieldPosition
          GreenMaskSize
          GreenFieldPosition
          BlueMaskSize
          BlueFieldPosition
          RsvdMaskSize
          RsvdFieldPosition
          DirectColorModeInfo
          ;; VBE 2.0
          PhysBasePtr
          ;; VBE 3.0
          LinBytesPerScanLine
          BnkNumberOfImagePages
          LinNumberOfImagePages
          LinRedMaskSize
          LinRedFieldPosition
          LinGreenMaskSize
          LinGreenFieldPositiondb
          LinBlueMaskSize
          LinBlueFieldPosition
          LinRsvdMaskSize
          LinRsvdFieldPosition
          MaxPixelClock))

(define (parse-vbe-mode mode-number bv)
  (call-with-values
    (lambda ()
      (unpack "<SCCSSSSLS 2S8Cx 9C L4x2x S10CL" bv))
    (lambda args
      (apply make-supervga-mode mode-number args))))

(define (attribute bit)
  (lambda (mode) (fxbit-set? (supervga-mode-ModeAttributes mode) bit)))

(define (not-attribute bit)
  (lambda (mode) (not (fxbit-set? (supervga-mode-ModeAttributes mode) bit))))

(define supervga-mode-supported? (attribute 0))
(define supervga-mode-tty-supported? (attribute 2))
(define supervga-mode-color-mode? (attribute 3))
(define supervga-mode-graphics-mode? (attribute 4))
(define supervga-mode-vga-compatible? (not-attribute 5))
(define supervga-mode-vga-windowing? (not-attribute 6))
(define supervga-mode-linear-framebuffer? (attribute 7))
(define supervga-mode-double-scan-available? (attribute 8))
(define supervga-mode-interlaced-available? (attribute 9))
(define supervga-mode-triple-buffering? (attribute 10))
(define supervga-mode-stereo-display? (attribute 11))
(define supervga-mode-dual-display-start? (attribute 12))

(define supervga-memory-models
  '((#x00 . text-mode)
    (#x01 . cga-graphics)
    (#x02 . hercules-graphics)
    (#x03 . planar)
    (#x04 . packed-pixel)
    (#x05 . non-chain-4)
    (#x06 . direct-color)
    (#x07 . yuv)))

;;; BIOS emulation

(define DEBUG #f)

(define vga-memory-hook
  (case-lambda
    ((addr size value)
     (yield-current-task)
     (when DEBUG
       (display (list 'c-write-memory (number->string addr 16) size value))
       (newline))
     (case size
       ((8) (put-mem-u8 addr value))
       ((16) (and (fxzero? (fxand addr #b1)) (put-mem-u16 addr value)))
       ((32) (and (fxzero? (fxand addr #b11)) (put-mem-u32 addr value)))
       (else #f)))
    ((addr size)
     (yield-current-task)
     (when DEBUG
       (display (list 'c-read-memory addr size))
       (newline))
     (case size
       ((8) (get-mem-u8 addr))
       ((16) (and (fxzero? (fxand addr #b1)) (get-mem-u16 addr)))
       ((32) (and (fxzero? (fxand addr #b11)) (get-mem-u32 addr)))
       (else 0)))))

;; Relay I/O accesses to the hardware
(define vga-raw-i/o-hook
  (case-lambda
    ((port size)
     (yield-current-task)
     (case size
       ((8) (get-i/o-u8 port))
       ((16) (get-i/o-u16 port))
       (else (get-i/o-u32 port))))
    ((port size value)
     (yield-current-task)
     (case size
       ((8) (put-i/o-u8 port value))
       ((16) (put-i/o-u16 port value))
       (else (put-i/o-u32 port value))))))

(define vga-XXX-i/o-hook
  (case-lambda
    ((port size)
     (yield-current-task)
     (when DEBUG
       (display "I/O read ")
       (display (list (number->string port 16) size))
       (newline))
     (case size
       ((8) (get-i/o-u8 port))
       ((16) (get-i/o-u16 port))
       (else (get-i/o-u32 port))))
    ((port size value)
     (yield-current-task)
     (when DEBUG
       (display "I/O write ")
       (display (list (number->string port 16) size value))
       (newline))
     (case size
       ((8) (put-i/o-u8 port value))
       ((16) (put-i/o-u16 port value))
       (else (put-i/o-u32 port value))))))


;; XXX: PCI should be emulated. QEMU debug can be passed through or
;; printed. Not sure about #x1ce etc. Probably the VGA ports should be
;; routed to a single device at a time (whatever switcheroo is doing).

;; (callf (far #xc000 3))
;; (icebp)
(define boot-code
  #vu8(#x9A #x03 #x00 #x00 #xC0
            #xF1))

;; (int #x10)
;; (icebp)
(define int10-code
  #vu8(#xCD #x10 #xF1))

(define (pcidev-init-AX dev)
  (fxior (fxarithmetic-shift-left (pcidev-bus dev) 8)
         (fxarithmetic-shift-left (pcidev-dev dev) 3)
         (pcidev-func dev)))

(define (hook-ports M dev)
  (for-each
   (lambda (size)
     ;; VGA ports are passed through. TODO: If there are multiple
     ;; video cards then we can fiddle with the bridges and command
     ;; bytes to make sure these accesses go to the right card.
     (do ((port #x3c0 (fx+ port 1)))
         ((fx>? port #x3df))
       (machine-hook-i/o-port! M port size vga-XXX-i/o-hook))
     ;; PCI configuration space. TODO: Don't just pass these through.
     ;; They should be interpreted and sent to the PCI driver
     ;; properly.
     (for-each (lambda (port)
                 (machine-hook-i/o-port! M port size vga-XXX-i/o-hook))
               '(#xcf8 #xcfc #xcfd #xcfe #xcff))
     ;; Let the BIOS access the video card's I/O BARs
     (vector-for-each
      (lambda (bar)
        (when (pcibar-i/o? bar)
          (do ((i 0 (fx+ i 1)))
              ((fx=? i (pcibar-size bar)))
            (machine-hook-i/o-port! M (fx+ (pcibar-base bar) i) size vga-XXX-i/o-hook))))
      (pcidev-BARs dev)))
   '(8 16 32)))

(define (video-bios-init dev img)
  (let ((M (make-machine)))
    ;; (machine-debug-set! M #t)
    ;; (machine-trace-set! M #t)
    (do ((addr #xA0000 (fx+ addr 4096)))
        ((fx>=? addr #xBFFFF))
      (machine-hook-4k-page! M addr vga-memory-hook))
    ;; VBE DISPI
    (machine-hook-i/o-port! M #x1ce 16 vga-raw-i/o-hook)
    (machine-hook-i/o-port! M #x1cf 16 vga-raw-i/o-hook)
    ;; QEMU debug console
    (machine-hook-i/o-port! M #x402 16 vga-raw-i/o-hook)
    ;; (machine-A20-gate-control 'enable)
    (hook-ports M dev)
    (machine-SS-set! M 0)
    (machine-SP-set! M #x7000)
    (machine-CS-set! M 0)
    (machine-IP-set! M #x7C00)
    (machine-BX-set! M (fxdiv #xC0000 16))
    (machine-AX-set! M (pcidev-init-AX dev))
    (copy-to-memory M #xC0000 (pcirom-image img))
    (copy-to-memory M #x7C00 boot-code)
    ;; Trap icebp to exit
    (enable-interrupt-hooks M)
    (machine-hook-interrupt! M 1 (lambda (M int chain) 'stop))
    ;; FIXME: add a timeout
    (machine-run M)
    M))

(define (memory-far16-ref RAM x)
  (fx+ (fxarithmetic-shift-left (memory-u16-ref RAM (fx+ x 2)) 4)
       (memory-u16-ref RAM x)))

(define (get-asciiz RAM addr)
  (utf8->string
   (call-with-bytevector-output-port
     (lambda (p)
       (let lp ((i 0))
         (unless (> i 256)
           (let ((b (memory-u8-ref RAM (fx+ addr i))))
             (unless (eqv? b 0)
               (put-u8 p b)
               (lp (fx+ i 1))))))))))

;; Call interrupt 10h
(define (int10h M)
  (machine-CS-set! M 0)
  (machine-IP-set! M #x7C00)
  (copy-to-memory M #x7C00 int10-code)
  (machine-run M))

(define (vesa-init M)
  (define buf #x40000)
  (define RAM (machine-RAM M))
  ;; ES:DI is the buffer.
  (machine-AX-set! M #x4f00)        ;VESA init
  (machine-ES-set! M (fxarithmetic-shift-right buf 4))
  (machine-DI-set! M (fxand buf #xb1111))
  (copy-to-memory M buf (string->utf8 "VBE2"))
  (int10h M)
  (and (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
       (equal? (copy-from-memory M buf 4)
               (string->utf8 "VESA"))
       (make-supervga-info
        (memory-u8-ref RAM (fx+ buf #x5))
        (memory-u8-ref RAM (fx+ buf #x4))
        (get-asciiz RAM (memory-far16-ref RAM (fx+ buf #x6)))
        (memory-u32-ref RAM (fx+ buf #xa))
        (let ((modes (memory-far16-ref RAM #x4000e)))
          (let lp ((i 0))
            (if (> i 1024) '()
                (let ((number (memory-u16-ref RAM (fx+ modes i))))
                  (cond ((eqv? number #xffff) '())
                        (else
                         (cons number (lp (fx+ i 2)))))))))
        (* (memory-u16-ref RAM (fx+ buf #x12))
           (* 64 1024))
        (memory-u16-ref RAM (fx+ buf #x14))
        (get-asciiz RAM (memory-far16-ref RAM (fx+ buf #x16)))
        (get-asciiz RAM (memory-far16-ref RAM (fx+ buf #x1A)))
        (get-asciiz RAM (memory-far16-ref RAM (fx+ buf #x1E))))))

;;; Interrupt routines

(define (vesa-get-mode-info M mode)
  ;; ES:DI is the buffer.
  (machine-AX-set! M #x4f01)        ;VESA init
  (machine-CX-set! M mode)
  (machine-ES-set! M #x4000)
  (machine-DI-set! M #x0000)
  (int10h M)
  (and (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
       (parse-vbe-mode mode (copy-from-memory M #x40000 256))))

(define (vesa-set-mode M mode lfb? clear? crtc-info-block)
  ;; The mode #x81ff is special and provides access to all of Video
  ;; RAM.
  (machine-AX-set! M #x4f02)        ;VESA set mode
  (let* ((BX (fxand mode #x1ff))
         (BX (if lfb? (fxior BX (expt 2 14)) BX))
         (BX (if clear? BX (fxior BX (expt 2 15))))
         (BX (if crtc-info-block (fxior BX (expt 2 11)) BX)))
    ;; If bit 11 is set then ES:DI should point to the
    ;; crtc-info-block.
    (machine-BX-set! M BX))
  (when crtc-info-block
    (machine-ES-set! M #x4000)
    (machine-DI-set! M #x0000)
    (copy-to-memory M #x40000 crtc-info-block))
  (int10h M)
  (and (eqv? (fxand (machine-AX M) #xFFFF) #x004f)))

(define (make-crtc-info horizontal-total horizontal-sync-start horizontal-sync-end
                        vertical-total vertical-sync-start vertical-sync-end
                        flags pixel-clock)
  ;; Flags: bit 0 = double scan, 1 = interlaced, 2 = negative hsync, 3
  ;; negative vsync.
  (let ((refresh-rate
         (exact (round (* (/ pixel-clock (* horizontal-total vertical-total)) 100)))))
    (pack "<u6SCLS"
          horizontal-total horizontal-sync-start horizontal-sync-end
          vertical-total vertical-sync-start vertical-sync-end
          flags pixel-clock refresh-rate)))

(define (vesa-get-mode M)
  (machine-AX-set! M #x4f03)        ;VESA get mode
  (int10h M)
  (and (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
       (fxand (machine-BX M) #xffff)))

(define (vesa-get-logical-scan-line-length M)
  (define who 'vesa-get-logical-scan-line-length)
  ;; Arguments
  (machine-AX-set! M #x4f06) ;VESA set/get logical scan line length
  (machine-BX-set! M #x01)
  (int10h M)
  (unless (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
    (error who "VESA error" (machine-AX M)))
  (values (machine-BX M)            ;bytes per scan line
          (machine-CX M)            ;pixels per scan line
          (machine-DX M)))        ;maximum no. of scanlines

(define (vesa-get-maximum-scan-line-length M)
  (define who 'vesa-get-maximum-scan-line-length)
  (machine-AX-set! M #x4f06) ;VESA set/get logical scan line length
  (machine-BX-set! M #x03)
  (int10h M)
  (unless (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
    (error who "VESA error" (machine-AX M)))
  (values (machine-BX M)            ;bytes per scan line
          (machine-CX M)            ;pixels per scan line
          (machine-DX M)))        ;maximum no. of scanlines

(define (vesa-set-logical-scan-line-length/pixels M length)
  (define who 'vesa-set-logical-scan-line-length/pixels)
  ;; Arguments.
  (machine-AX-set! M #x4f06) ;VESA set/get logical scan line length
  (machine-BX-set! M #x00)
  (machine-CX-set! M length)
  (int10h M)
  (unless (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
    (error who "VESA error" (machine-AX M)))
  (values (machine-BX M)            ;bytes per scan line
          (machine-CX M)            ;pixels per scan line
          (machine-DX M)))        ;maximum no. of scanlines

(define (vesa-set-logical-scan-line-length/bytes M length)
  (define who 'vesa-set-logical-scan-line-length/bytes)
  ;; Arguments.
  (machine-AX-set! M #x4f06) ;VESA set/get logical scan line length
  (machine-BX-set! M #x02)
  (machine-CX-set! M length)
  (int10h M)
  (unless (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
    (error who "VESA error" (machine-AX M)))
  (values (machine-BX M)            ;bytes per scan line
          (machine-CX M)            ;pixels per scan line
          (machine-DX M)))        ;maximum no. of scanlines

;; First-pixel is actually in bytes.
(define (vesa-set-display-start M first-pixel first-scan-line vertical-retrace?)
  (define who 'vesa-set-display-start)
  ;; Arguments.
  (machine-AX-set! M #x4f07) ;VESA set/get display start
  (let* ((BX #x00)
         (BX (if vertical-retrace? (fxior BX #x80) BX)))
    ;; BX would be #x02/#x82 for VBE 1.2, but then the coordinates
    ;; are in pixels rather than bytes.
    (machine-BX-set! M BX))
  (machine-CX-set! M first-pixel)
  (machine-DX-set! M first-scan-line)
  (int10h M)
  (unless (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
    (error who "VESA error" (machine-AX M))))

;; Get the closest possible pixel clock (in Hz)
(define (vesa-get-pixel-clock M mode clock)
  (machine-AX-set! M #x4f0b)        ;VESA get pixel clock
  (machine-BX-set! M #x00)
  (machine-CX-set! M clock)
  (machine-DX-set! M mode)
  (int10h M)
  (and (eqv? (fxand (machine-AX M) #xFFFF) #x004f)
       (machine-CX M)))

;;; Plain old VGA

(define (vga-set-video-mode M mode clear?)
  (let* ((AX (fxand mode #x7f))
         (AX (if clear? AX (fxior AX #x80))))
    (machine-AX-set! M AX))
  (int10h M)
  (values))

(define (vga-put-char M char page color)
  (machine-AX-set! M (fxior #x0e00
                            (fxand (char->integer char) #xff)))
  (machine-BX-set! M (fxior (fxarithmetic-shift-left (fxand page #xff) 8)
                            (fxand color #xff)))
  (int10h M)
  (values)))
