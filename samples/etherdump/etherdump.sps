;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Dump ethernet traffic on an RTL8139

(import
  (rnrs (6))
  (loko drivers pci)
  (loko system unsafe)
  (loko system fibers)

  (text-mode console)
  (text-mode console events)
  (text-mode console model)

  ;; XXX: This API is up for question. It might be changed to use DMA
  ;; and IRQ objects instead.
  (only (loko system $host) dma-allocate dma-free
        enable-irq acknowledge-irq wait-irq-operation))

(define (fxalign i alignment)
  (fxand (fx+ i (fx- alignment 1))
         (fx- alignment)))

;;; Bare bones VGA text mode output

(define (vga-textmode-backend)
  (define mem-base #xb8000)
  (define reg-base #x3c0)
  (define rows 25)
  (define cols 80)
  ;; VGA regs
  (define crtc-addr 20)
  (define crtc-data 21)
  ;; Registers in the CRT Controller:
  (define cursor-start #x0a)
  (define cursor-end #x0b)
  (define cursor-location-high #x0e)
  (define cursor-location-low #x0f)
  (define (crtc-read addr)
    (put-i/o-u8 (fx+ reg-base crtc-addr) addr)
    (get-i/o-u8 (fx+ reg-base crtc-data)))
  (define (crtc-write addr byte)
    (put-i/o-u8 (fx+ reg-base crtc-addr) addr)
    (put-i/o-u8 (fx+ reg-base crtc-data) byte))
  (define (vga-cursor-move x y)
    (let ((offset (+ x (* y cols))))
      (crtc-write cursor-location-low (fxbit-field offset 0 8))
      (crtc-write cursor-location-high (fxbit-field offset 8 16))))
  (define vga-colors
    (vector Black Blue Green Cyan Red Magenta Brown Gray
            DarkGray LightBlue LightGreen LightCyan LightRed
            LightMagenta Yellow White))
  (define (closest-color col fallback)
    (if (eqv? col Default)
        fallback
        (let lp ((i 0))
          (cond ((fx=? i (vector-length vga-colors)) fallback)
                ((fx=? col (vector-ref vga-colors i)) i)
                (else (lp (fx+ i 1)))))))
  (lambda (cmd arg)
    (case cmd
      [(get-size)
       (values cols rows 0 0)]
      [(init)
       #f]
      [(update redraw)
       (let ((c arg))
         (assert (fx=? cols (console-full-cols c)))
         (assert (fx=? rows (console-full-rows c)))
         (do ((y 0 (fx+ y 1))) ((fx=? y rows))
           (let ((mem-row (fx* 2 (fx* y cols))))
             (when (console-row-dirty? c y 'absolute)
               (let-values ([(buf mbuf fgbuf bgbuf abuf idx) (%index/nowin c 0 y)])
                 (do ((x 0 (fx+ x 1))) ((fx=? x cols))
                   (let ((mem-offset (fx+ (fx* 2 x) mem-row)))
                     (let ((ch (text-ref buf (fx+ idx x))))
                       (unless (textcell-unused? ch)
                         (put-mem-u8 (fx+ mem-base mem-offset)
                                     (char->integer ch))
                         (let ((fg (closest-color (fg-ref fgbuf (fx+ idx x)) 7))
                               (bg (closest-color (bg-ref bgbuf (fx+ idx x)) 0)))
                           (put-mem-u8 (fx+ mem-base (fx+ mem-offset 1))
                                       (fxior (fxarithmetic-shift-left bg 4)
                                              fg)))))))))))
         (clear-console-dirty! c)
         (vga-cursor-move (fx+ (console-x c) (console-x1 c))
                          (fx+ (console-y c) (console-y1 c))))]
      [(read-event)
       #f]
      [else
       #f])))

(define (draw-ui)
  (reset-window)
  (text-color Default)
  (text-background Default)
  (clrscr)

  (gotoxy 0 0)
  (text-color Yellow)
  (text-background Blue)
  (print "etherdump.sps - rtl8139 ethernet frame printer demo")
  (clreol)

  (gotoxy 0 (- (window-maxy) 1))
  (clreol)

  (text-color Default)
  (text-background Default)
  (set-window 0 1 (window-maxx) (- (window-maxy) 1))
  (clrscr))

(current-console (make-console (vga-textmode-backend)))
(draw-ui)

;;; RTL8139

;; A few RTL8139 registers
(define reg-RBSTART   #x0030)
(define reg-CR        #x0037)
(define reg-CAPR      #x0038)
(define reg-IMR       #x003C)
(define reg-ISR       #x003E)
(define reg-RCR       #x0044)

;; Status register in the Rx packet header
(define rx-MAR  #b100000000000000)
(define rx-PAM   #b10000000000000)
(define rx-BAR    #b1000000000000)
(define rx-ISE           #b100000)
(define rx-RUNT           #b10000)
(define rx-LONG            #b1000)
(define rx-CRC              #b100)
(define rx-RAE               #b10)
(define rx-ROK                #b1)
(define rx-errors        #b111110)

;; CR, Command register
(define cr-RST  #b10000)
(define cr-RE    #b1000)
(define cr-TE     #b100)
(define cr-BUFE     #b1)

;; Bits for the ISR and IMR registers (interrupt status/mask)
(define int-SERR        #b1000000000000000)
(define int-TimeOut      #b100000000000000)
(define int-LenChg        #b10000000000000)
(define int-FOVW                 #b1000000)
(define int-PUN/LinkChg           #b100000)
(define int-RXOVW                  #b10000)
(define int-TER                     #b1000)
(define int-TOK                      #b100)
(define int-RER                       #b10)
(define int-ROK                        #b1)
(define int-all         #b1110000001111111)

;; RCR, u32, Receive Configuration Register (there are more bits)
(define rcr-WRAP #b10000000)
(define rcr-AER    #b100000)
(define rcr-AR      #b10000)
(define rcr-AB       #b1000)
(define rcr-AM        #b100)
(define rcr-APM        #b10)
(define rcr-AAP         #b1)

(define (rtl8139-reset base rxbuf rxbuflen)
  ;; Assert reset and wait for it to respond
  (put-i/o-u8 (fx+ base reg-CR) cr-RST)
  (do ((try 0 (fx+ try 1)))
      ((eqv? 0 (fxand (get-i/o-u8 (fx+ base reg-CR)) cr-RST)))
    (when (eqv? try 10)
      (error 'rtl8139-reset "timeout when resetting the device"))
    (sleep 0.001))

  ;; Set receive buffer address.
  (put-i/o-u32 (fx+ base reg-RBSTART) rxbuf)

  ;; Enable interrupts
  (put-i/o-u16 (fx+ base reg-IMR) (fxior int-TOK int-ROK))
  (get-i/o-u16 (fx+ base reg-ISR))

  ;; WRAP received packets (packets will be placed after the rx buffer
  ;; even if they don't fit); accept broadcast, multicast, our address
  ;; and set promiscuous mode (AAP). Rx buffer length is 8k + 16.
  (assert (eqv? rxbuflen 8192))
  (put-i/o-u32 (fx+ base reg-RCR) (fxior rcr-WRAP rcr-AB rcr-AM rcr-APM rcr-AAP))

  ;; Enable receiver and transmitter
  (put-i/o-u8 (fx+ base reg-CR) (fxior cr-RE cr-TE))
  (println "rtl8139 reset"))

(define (handle-packet buf len)
  (define (hexdump start end)
    (define (bytes start end)
      (text-color LightCyan)
      (do ((i start (fx+ i 1)))
          ((fx=? i (fx+ start 4)))
        (cond ((fx<? i end)
               (let ((b (get-mem-u8 i)))
                 (when (fx<? b #x10) (print "0"))
                 (print (number->string b 16))
                 (print #\space)))
              (else (print "   "))))
      (text-color DarkGray))
    (do ((i start (fx+ i 16)))
        ((fx>=? i end))
      (bytes i end)
      (print "\xB3; ")
      (bytes (fx+ i 4) end)
      (print "\xB3; ")
      (bytes (fx+ i 8) end)
      (print "\xB3; ")
      (bytes (fx+ i 12) end)
      (print "  ")
      (text-color Default)
      (do ((j i (fx+ j 1)))
          ((or (fx=? j (fx+ i 16)) (fx=? j end)))
        (print (integer->char (get-mem-u8 j))))
      (println)))
  (define (print-mac start)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i 6))
      (let ((b (get-mem-u8 (fx+ start 1))))
        (when (fx<? b #x10) (print "0"))
        (print (number->string b 16))
        (unless (fx=? i 5)
          (print ":")))))

  (when (fx>? len 12)
    (text-color LightGreen)
    (print-mac (fx+ buf 6))
    (text-color Default)
    (print " -> ")
    (text-color LightGreen)
    (print-mac buf)
    (print " ")
    (text-color Yellow)
    (let ((ethertype (fxior (fxarithmetic-shift-left (get-mem-u8 (fx+ buf 12)) 8)
                            (get-mem-u8 (fx+ buf 13)))))
      (print
       (case ethertype
         ((#x806) 'ARP)
         ((#x800) 'IPv4)
         ((#x86DD) 'IPv6)
         (else
          (number->string ethertype 16)))))
    (println)

    (hexdump (fx+ buf 14) (fx+ buf (fx- len 14)))))

(define (get-rx-header addr)
  (let ([status (fxior (get-mem-u8 addr)
                       (fxarithmetic-shift-left (get-mem-u8 (fx+ addr 1)) 8))]
        [size (fxior (get-mem-u8 (fx+ addr 2))
                     (fxarithmetic-shift-left (get-mem-u8 (fx+ addr 3)) 8))])
    (values status size)))

(define (rtl8139-main irq base rxbuf rxbuflen)
  (define offset 0)
  (enable-irq irq)
  (let lp ()
    (case (perform-operation
           (wrap-operation (wait-irq-operation irq)
                           (lambda _ 'int)))
      ((int)
       ;; Print the received packets
       (let loop ()
         (when (eqv? (fxand (get-i/o-u8 (fx+ base reg-CR)) #b1) 0)
           (let-values ([(status size) (get-rx-header (fx+ rxbuf offset))])
             (cond ((not (eqv? 0 (fxand status rx-errors)))
                    (print "Error status: ")
                    (println status))
                   (else
                    (handle-packet (fx+ (fx+ rxbuf offset) 4) (fx- size 4))))
             (set! offset (fxmod (fxalign (fx+ (fx+ offset size) 4) 4) rxbuflen))
             (put-i/o-u16 (fx+ base reg-CAPR) (fxand #xffff (fx- offset 16)))
             (loop))))
       ;; Acknowledge the interrupt. First in the rtl8139 and then in the PIC.
       (get-i/o-u16 (fx+ base reg-ISR))
       (put-i/o-u16 (fx+ base reg-ISR) int-all)
       (acknowledge-irq irq)))
    (lp)))

;; Driver entry point. Runs as a fiber.
(define (driver-pci-rtl8139 dev)
  ;; Enable I/O, bus mastering and unmask interrupts. Disable the
  ;; memory mapped registers.
  (pci-put-u8 dev
              PCI-CFG-COMMAND
              (fxand (fxior (fxior PCI-CMD-I/O-SPACE
                                   PCI-CMD-BUS-MASTER)
                            (pci-get-u8 dev PCI-CFG-COMMAND))
                     (fxnot (fxior PCI-CMD-INTERRUPT-DISABLE
                                   PCI-CMD-MEM-SPACE))))

  ;; BAR0 is in I/O space and BAR1 is in memory space.
  (let ([bar0 (vector-ref (pcidev-BARs dev) 0)]
        [irq (pcidev-irq dev)]
        ;; The manual says 8k + 16 + 1.5K. But that wrongly assumes a
        ;; 1500 byte packet limit.
        [rxbuf (dma-allocate (+ 8192 16 4096) #xfffffff0)]
        [rxbuflen 8192])
    (guard (exn
            (else
             (dma-free rxbuf)
             (raise exn)))
      (print "RTL8139 at I/O addr ")
      (print (number->string (pcibar-base bar0) 16))
      (print ", IRQ ")
      (println irq)
      (print "RX buffer: ")
      (println (number->string rxbuf 16))
      (rtl8139-reset (pcibar-base bar0) rxbuf rxbuflen)
      (rtl8139-main irq (pcibar-base bar0) rxbuf rxbuflen)
      (dma-free rxbuf))))

;; Check that this is a device the driver supports. Could check more.
(define (probe-pci-rtl8139 dev)
  (and (eqv? (pcidev-vendor-id dev) #x10EC)
       (eqv? (pcidev-device-id dev) #x8139)))

(define *devs* (pci-scan-bus #f))

(println "Scanning the PCI bus for rtl8139 devices...")
(update-screen)

(let ((devs *devs*))
  (for-each
   (lambda (dev)
     (when (probe-pci-rtl8139 dev)
       (print "Found an rtl8139 on ")
       (println (list (pcidev-bus dev)
                      (pcidev-dev dev)
                      (pcidev-func dev)))
       (spawn-fiber (lambda ()
                      (driver-pci-rtl8139 dev)))))
   devs))

;; Keep the screen updated. Not multiprocessing safe, but fibers are
;; currently cooperative anyway.
(let lp ()
  (sleep 0.1)
  (update-screen)
  (lp))
