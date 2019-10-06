;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; Universal Host Controller Interface (UHCI)

#|

Conventions in this driver:

 * &variable is a pointer to a virtual address for DMA:able (32-bit)
   memory
 * %variable is driver state;
 * devreq is device request
 * $variable is private
 * variable* is a list of zero or more objects
 * TD is transfer descriptor
 * QH is queue head

The driver is based on Lunt's book on USB. The queues are according to
the book, but oddly enough differ signficantly from how the design
guide says they should be set up wrt isochronous transfers and
reclamation.

|#

(library (loko drivers usb uhci)
  (export
    probe·pci·uhci?
    driver·pci·uhci
    driver·uhci)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko system unsafe)
    (only (loko system $host) dma-allocate dma-free
          enable-irq acknowledge-irq wait-irq-operation)
    (loko drivers pci)
    (loko drivers usb core)
    (loko drivers usb hub)
    (struct pack))

(define (driver·uhci reg-type^ reg-base reg-size irq controller)
  (define reg-type 'i/o)
  (define _dummy (assert (eq? reg-type^ reg-type)))
  (define (log/error . x)
    (for-each display x) (newline))
  (define log/debug (lambda _ #f))

;;; Access to the device registers (independent of i/o vs mem)

  (define (reg-u8-ref offset)
    (assert (fx<? -1 offset reg-size))
    (case reg-type
      ((i/o) (get-i/o-u8 (fx+ reg-base offset)))
      ((mem) (get-mem-u8 (fx+ reg-base offset)))
      (else (assert #f))))

  (define (reg-u16-ref offset)
    (assert (fx<? -1 offset (fx+ reg-size 1)))
    (case reg-type
      ((i/o) (get-i/o-u16 (fx+ reg-base offset)))
      ((mem) (get-mem-u16 (fx+ reg-base offset)))
      (else (assert #f))))

  (define (reg-u32-ref offset)
    (assert (fx<? -1 offset (fx+ reg-size 3)))
    (case reg-type
      ((i/o) (get-i/o-u32 (fx+ reg-base offset)))
      ((mem) (get-mem-u32 (fx+ reg-base offset)))
      (else (error 'reg-u32-ref "Unsupported register type" offset))))

  (define (reg-u8-set! offset v)
    (assert (fx<? -1 offset (fx+ reg-size 1)))
    (assert (fx<? -1 v (expt 2 8)))
    (case reg-type
      ((i/o) (put-i/o-u8 (fx+ reg-base offset) v))
      ((mem) (put-mem-u8 (fx+ reg-base offset) v))
      (else (assert #f))))

  (define (reg-u16-set! offset v)
    (assert (fx<? -1 offset (fx+ reg-size 1)))
    (assert (fx<? -1 v (expt 2 16)))
    (case reg-type
      ((i/o) (put-i/o-u16 (fx+ reg-base offset) v))
      ((mem) (put-mem-u16 (fx+ reg-base offset) v))
      (else (assert #f))))

  (define (reg-u32-set! offset v)
    (assert (fx<? -1 offset (fx+ reg-size 1)))
    (assert (fx<? -1 v (expt 2 32)))
    (case reg-type
      ((i/o) (put-i/o-u32 (fx+ reg-base offset) v))
      ((mem) (put-mem-u32 (fx+ reg-base offset) v))
      (else (assert #f))))

  (define (reg-flush-writes)
    ;; So... should read something. Only on some busses/archs, where
    ;; writes need to be posted. And it's not needed if a write is
    ;; followed by a read.
    #f)

  (define (dma-flush-writes)
    #f)

  (define (copy-bytevector-to-memory bv &addr)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i (bytevector-length bv)))
      (put-mem-u8 (fx+ &addr i) (bytevector-u8-ref bv i))))

  (define (copy-memory-to-bytevector &addr bv)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i (bytevector-length bv)) bv)
      (bytevector-u8-set! bv i (get-mem-u8 (fx+ &addr i)))))

;;; Register definitions

  ;; Command register

  (define (USBCMD-ref) (reg-u16-ref #x00))
  (define (USBCMD-set! v) (reg-u16-set! #x00 v))
  (define CMD-MAXP     #b10000000)      ;Max Packet, 64/32 bytes
  (define CMD-CF       #b01000000)      ;Tell BIOS to go away
  (define CMD-SWDBG    #b00100000)      ;Software Debug
  (define CMD-FGR      #b00010000)      ;Force Global Resume
  (define CMD-EGSM     #b00001000)      ;Enter Global Suspend Mode
  (define CMD-GRESET   #b00000100)      ;Global Reset
  (define CMD-HCRESET  #b00000010)      ;Host Controller Reset
  (define CMD-RS       #b00000001)      ;Run/Stop

  ;; Status register

  (define (USBSTS-ref) (reg-u16-ref #x02))
  (define (USBSTS-set! v) (reg-u16-set! #x02 v))
  (define STS-HCHalted #b100000)        ;Host Controller halted
  (define STS-HCPError #b010000)        ;Host Controller Process Error
  (define STS-HSError  #b001000)        ;Host System Error
  (define STS-Resume   #b000100)        ;Resume Detect
  (define STS-USBError #b000010)        ;USB Error Interrupt
  (define STS-USBINT   #b000001)        ;USB Interrupt
  (define STS-all-bits #b111111)
  (define (USBSTS-clear!)               ;clear all status bits
    (USBSTS-set! STS-all-bits))

  ;; Interrupt enable register

  (define (USBINTR-ref) (reg-u16-ref #x04))
  (define (USBINTR-set! v) (reg-u16-set! #x04 v))
  (define INTR-Short       #b1000) ;Short Packet Interrupt Enable
  (define INTR-IOC         #b0100) ;Interrupt On Complete (IOC) Enable
  (define INTR-Resume      #b0010) ;Resume Interrupt Enable
  (define INTR-Timeout/CRC #b0001) ;Timeout/CRC Interrupt Enable

  ;; Frame number register

  (define (FRNUM-ref) (reg-u16-ref #x06))
  (define (FRNUM-set! v) (reg-u16-set! #x06 v))
  (define FRNUM-MAX 1024)

  ;; Frame list base address register

  (define (FRBASEADD-ref) (reg-u32-ref #x08))
  (define (FRBASEADD-set! v) (reg-u32-set! #x08 v))

  ;; Start of frame (SOF) modify register

  (define (SOFMOD-ref) (reg-u8-ref #x0C))
  (define (SOFMOD-set! v) (reg-u8-set! #x0C v))
  (define SOF-base 11936)               ;timing

  ;; Port status and control register

  (define (PortSCn-ref n) (reg-u16-ref (fx+ #x10 (fx* n 2))))
  (define (PortSCn-set! n v) (reg-u16-set! (fx+ #x10 (fx* n 2)) v))
  (define PortSC-Suspend        #b1000000000000) ;Port suspend
  (define PortSC-Reset          #b0001000000000) ;Port reset
  (define PortSC-Low-Speed      #b0000100000000) ;Low speed device attached
  (define PortSC-Reserved-1     #b0000010000000) ;Always read as 1
  (define PortSC-Resume-Detect  #b0000001000000) ;Resume Detect
  (define PortSC-Line-D-        #b0000000100000) ;D- line status
  (define PortSC-Line-D+        #b0000000010000) ;D+ line status
  (define PortSC-Enable-Change  #b0000000001000) ;Enable/disable change
  (define PortSC-Enable         #b0000000000100) ;Enable/disable
  (define PortSC-Connect-Change #b0000000000010) ;Connect status change
  (define PortSC-Connect        #b0000000000001) ;Current connect status
  (define PortSC-R/WC-mask      #b0000000001010) ;Cleared when written as 1

;;; Hardware (DMA) data structures

  ;; Frame list pointer. One u32.
  (define FLP-mask #xFFFFFFF0)        ;valid address bits
  (define FLP-QH  #b10)               ;1=QH, 0=TD
  (define FLP-T   #b01)               ;1=Terminate (address not valid)
  (define (FLP-link-queue! &addr idx &queue-physaddr)
    (put-mem-u32 (fx+ &addr (fx* idx 4)) (fxior &queue-physaddr FLP-QH)))

  ;; Transfer descriptor. 16-byte alignment. Eight u32. Link, control
  ;; and status, token, buffer pointer and four u32 reserved for the
  ;; driver (as if).
  (define TD-size 32)
  (define TD-alignment 16)
  (define TD0-LINK-LP-mask #xFFFFFFF0) ;valid address bits
  (define TD0-LINK-Vf  #b100)          ;1=depth first, 0=breadth first
  (define TD0-LINK-Q   #b010)          ;1=QH, 0=TD
  (define TD0-LINK-T   #b001)         ;1=Terminate (address not valid)
  (define (TD-link-td! &addr &queue-physaddr)
    (put-mem-u32 &addr (fxior TD0-LINK-Vf &queue-physaddr)))
  (define (TD-link-terminate! &addr)
    (put-mem-u32 &addr TD0-LINK-T))

  (define TD1-CTRL-SPD         #b100000000000000000000000000000) ;Short Packet Detect
  (define TD1-CTRL-Errors-mask #b011000000000000000000000000000) ;Error down counter
  (define TD1-CTRL-LS          #b000100000000000000000000000000) ;Target is low speed
  (define TD1-CTRL-ISO         #b000010000000000000000000000000) ;1=Isochronous
  (define TD1-CTRL-IOC         #b000001000000000000000000000000) ;1=Interrupt on Complete
  (define TD1-STS-Active       #b000000100000000000000000000000) ;1=Active
  (define TD1-STS-Stalled      #b000000010000000000000000000000) ;1=Stalled
  (define TD1-STS-BufferErr    #b000000001000000000000000000000) ;1=Data Buffer Error
  (define TD1-STS-Babble       #b000000000100000000000000000000) ;1=Babble detected
  (define TD1-STS-NAK          #b000000000010000000000000000000) ;1=NAK Received
  (define TD1-STS-CRC/Timeout  #b000000000001000000000000000000) ;1=CRC/Time Out Error
  (define TD1-STS-BitstuffErr  #b000000000000100000000000000000) ;1=Bitstuff Error
  (define TD1-STS-ActLen-mask  #b000000000000000000011111111111) ;Actual Length
  (define (TD-set-control! &addr low-speed? isochronous? ioc?)
    (put-mem-u32 (fx+ &addr 4)
                  (fxior (fxior TD1-CTRL-Errors-mask
                                TD1-STS-Active)
                         (fxior (if low-speed? TD1-CTRL-LS 0)
                                (if isochronous? TD1-CTRL-ISO 0)
                                (if ioc? TD1-CTRL-IOC 0)))))
  (define (TD-status-active? &addr)
    (not (fxzero? (fxand (get-mem-u32 (fx+ &addr 4))
                         TD1-STS-Active))))
  (define (TD-status-error? &addr)
    (not (fxzero? (fxand (get-mem-u32 (fx+ &addr 4))
                         (fxior TD1-STS-Stalled
                                TD1-STS-BufferErr
                                TD1-STS-Babble
                                TD1-STS-NAK
                                TD1-STS-CRC/Timeout
                                TD1-STS-BitstuffErr)))))

  (define TD2-TOKEN-MaxLen  #b11111111111000000000000000000000) ;Maximum Length
  (define TD2-TOKEN-D                   #b10000000000000000000) ;0=DATA0, 1=DATA1
  (define TD2-TOKEN-EndPt               #b01111000000000000000) ;Endpoint
  (define TD2-TOKEN-Address             #b00000111111100000000) ;Device Address
  (define TD2-TOKEN-PID                 #b00000000000011111111) ;Packet Identification
  (define mask-TD2-TOKEN-MaxLen #b11111111111)
  (define shift-TD2-TOKEN-MaxLen 21)
  (define shift-TD2-TOKEN-D 19)
  (define shift-TD2-TOKEN-EndPt 15)
  (define shift-TD2-TOKEN-Address 8)
  (define Length0 0)
  (define DATA1 1)
  (define DATA0 0)
  (define EndPt0 0)
  (define Address0 0)
  (define PID-SETUP #x2D)
  (define PID-OUT   #xE1)
  (define PID-IN    #x69)
  (define (TD-set-token! &addr MaxLen D EndPt Address PID)
    (assert (fx<=? 0 MaxLen 1280))
    (assert (fx<=? 0 D 1))
    (put-mem-u32 (fx+ &addr 8)
                  (bitwise-ior
                   (bitwise-arithmetic-shift-left
                    (fxand (fx- MaxLen 1) mask-TD2-TOKEN-MaxLen)
                    shift-TD2-TOKEN-MaxLen)
                   (fxior
                    (fxand TD2-TOKEN-D
                           (fxarithmetic-shift-left D shift-TD2-TOKEN-D))
                    (fxand TD2-TOKEN-EndPt
                           (fxarithmetic-shift-left EndPt shift-TD2-TOKEN-EndPt))
                    (fxand TD2-TOKEN-Address
                           (fxarithmetic-shift-left Address shift-TD2-TOKEN-Address))
                    (fxand TD2-TOKEN-PID
                           PID)))))

  (define TD3-MASK #xFFFFFFFF)          ;valid address bits
  (define (TD-set-address! &addr data-physaddr)
    (put-mem-u32 (fx+ &addr 12) data-physaddr))
  (define (TD->list &addr)
    (assert &addr)
    (let* ((link (get-mem-u32 &addr))
           (sts (get-mem-u32 (fx+ &addr 4)))
           (token (get-mem-u32 (fx+ &addr 8))))
      `((Link-LP . ,(fxand link TD0-LINK-LP-mask))
        (Link-Vf . ,(fxand link TD0-LINK-Vf))
        (Link-Q . ,(fxand link TD0-LINK-Q))
        (Link-T . ,(fxand link TD0-LINK-T))
        (STS-Active . ,(not (fxzero? (fxand sts TD1-STS-Active))))
        (STS-Stalled . ,(not (fxzero? (fxand sts TD1-STS-Stalled))))
        (STS-BufferErr . ,(not (fxzero? (fxand sts TD1-STS-BufferErr))))
        (STS-Babble . ,(not (fxzero? (fxand sts TD1-STS-Babble))))
        (STS-NAK . ,(not (fxzero? (fxand sts TD1-STS-NAK))))
        (STS-CRC/Timeout . ,(not (fxzero? (fxand sts TD1-STS-CRC/Timeout))))
        (STS-BitstuffErr . ,(not (fxzero? (fxand sts TD1-STS-BitstuffErr))))
        (STS-ActLen . ,(fxand sts TD1-STS-ActLen-mask))
        (TOKEN-PID . ,(fxbit-field token 0 8))
        (TOKEN-Address . ,(fxand #b1111111
                                 (fxarithmetic-shift-right token shift-TD2-TOKEN-Address)))
        (TOKEN-EndPt ,(fxand #xf (fxarithmetic-shift-right token shift-TD2-TOKEN-EndPt)))
        (TOKEN-D . ,(fxand 1 (fxarithmetic-shift-right token shift-TD2-TOKEN-D)))
        (TOKEN-MaxLen . ,(fxand mask-TD2-TOKEN-MaxLen
                                (bitwise-arithmetic-shift-right
                                 token shift-TD2-TOKEN-MaxLen))))))

  ;; Queue Head. 16-byte alignment. Two u32 for the controller and two
  ;; reserved for the driver.
  (define QH-size 16)
  (define QH-alignment 16)
  (define QH-HEAD-mask #xFFFFFFF0)    ;address of next object
  (define QH-HEAD-Q #b10)             ;1=QH, 0=TD
  (define QH-HEAD-T #b01)             ;1=Terminate (address not valid)
  (define (QH-HEAD-terminate! &addr)
    (put-mem-u32 &addr QH-HEAD-T))
  (define (QH-HEAD-link-queue! &addr &queue-physaddr)
    (put-mem-u32 &addr (fxior &queue-physaddr QH-HEAD-Q)))

  (define QH-ELEMENT-mask #xFFFFFFF0)   ;valid address bits
  (define QH-ELEMENT-Q #b10)            ;1=QH 0=TD
  (define QH-ELEMENT-T #b01)            ;1=Terminate (no valid queue entries)
  (define (QH-ELEMENT-terminate! &addr)
    (put-mem-u32 (fx+ &addr 4) QH-ELEMENT-T))
  (define (QH-ELEMENT-link-td! &addr &td-physaddr)
    (put-mem-u32 (fx+ &addr 4) &td-physaddr))
  (define (QH-ELEMENT-td &addr)
    (let ((x (get-mem-u32 (fx+ &addr 4))))
      (and (fxzero? (bitwise-and x QH-ELEMENT-Q))
           (bitwise-and x QH-ELEMENT-mask))))
  (define (QH->list &addr)
    (let ((head (get-mem-u32 &addr))
          (element (get-mem-u32 (fx+ &addr 4))))
      `((HEAD . ,(bitwise-and head QH-HEAD-mask))
        (HEAD-Q . ,(bitwise-and head QH-HEAD-Q))
        (HEAD-T . ,(bitwise-and head QH-HEAD-T))
        (ELEMENT . ,(bitwise-and element QH-ELEMENT-mask))
        (ELEMENT-Q . ,(bitwise-and element QH-ELEMENT-Q))
        (ELEMENT-T . ,(bitwise-and element QH-ELEMENT-T)))))

;;; Driver state

  (define %devices (make-vector 256 #f))
  ;; %num-ports
  (define %original-SOF (SOFMOD-ref))   ;SOF determined by BIOS (1ms timing)
  (define %&frame-list #f)              ;address of the frame list
  (define %&queues #f)                  ;vector of skeleton queue heads

  (define &frame (dma-allocate 4096 #xFFFFF000))
  (define &queues (dma-allocate 4096 #xFFFFF000))
  (define &probe-mem (dma-allocate 4096 #xFFFFF000)) ;memory for probing devices

;;; Driver

  ;; The address that will be assigned to the next device that shows up
  (define (next-free-address)
    (let lp ((i 1))
      (cond ((fx=? i (vector-length %devices)) #f)
            ((eqv? #f (vector-ref %devices i)) i)
            (else (lp (fx+ i 1))))))

  (define (millisleep ms)
    (sleep (/ ms 1000)))

  ;; FIXME: Make this and the callers asynchronous
  (define (await-transfer-descriptor &td timeout)
    (let lp ((waited 1))
      (millisleep 1)                    ;XXX: might not be working
      (cond ((TD-status-active? &td)
             (if (fx>=? waited timeout)
                 'timeout
                 (lp (fx+ waited 1))))
            ((TD-status-error? &td)
             'error)
            (else 'ok))))

  ;; Reset the controller and attached devices
  (define (global-reset)
    (USBCMD-set! CMD-GRESET)
    (millisleep 11)
    (USBCMD-set! 0))

  ;; True if a UHCI controller is detected.
  (define (detect-controller)
    (do ((i 0 (fx+ i 1)))
        ((fx=? i 5))
      (global-reset))
    (and (fxzero? (USBCMD-ref))
         (fx=? (USBSTS-ref) STS-HCHalted)
         (begin
           (USBSTS-clear!)
           (and (fx=? (SOFMOD-ref) #x40) ;default SOFMOD
                (begin
                  ;; See if the controller resets bit 1.
                  (USBCMD-set! CMD-HCRESET)
                  (millisleep 50)
                  (fxzero? (fxand (USBCMD-ref) CMD-HCRESET)))))))

  (define (count-ports)
    ;; Check if a port is present by testing bit 7, which always is 1
    (do ((maxports (fxdiv (fx- reg-size 16) 2))
         (i 0 (fx+ i 1)))
        ((or (fx=? i maxports)
             (fxzero? (fxand PortSC-Reserved-1 (PortSCn-ref i))))
         i)))

  (define (init-controller)
    (log/debug "Starting UHCI controller...")
    (USBINTR-set! (fxior INTR-Short INTR-IOC INTR-Resume INTR-Timeout/CRC))
    (FRNUM-set! #x0000)
    (%allocate-frame-list!)             ;set %&frame-list and %&queues
    (FRBASEADD-set! %&frame-list)
    (SOFMOD-set! %original-SOF)
    (USBSTS-clear!)
    (USBCMD-set! (fxior CMD-CF CMD-RS))
    (reg-flush-writes))

  (define (uninit-controller)
    (log/debug "Stopping UHCI controller...")
    (global-reset)
    (millisleep 50)
    (SOFMOD-set! %original-SOF))

  ;; Indices into the %&queues vector
  (define idx-Q1 0)
  (define idx-Q2 1)
  (define idx-Q4 2)
  (define idx-Q8 3)
  (define idx-Q16 4)
  (define idx-Q32 5)
  (define idx-Q64 6)
  (define idx-Q128 7)
  (define idx-QISO 8)
  (define idx-QLS 9)
  (define idx-QFS 10)

  ;; Insert a transfer descriptor in the queue. FIXME: This needs to
  ;; be cleverer. XXX: the controller removes the td itself.
  (define (schedule-insert-td queue-index &td-physaddr)
    (QH-ELEMENT-link-td! (vector-ref %&queues queue-index) &td-physaddr)
    (dma-flush-writes))

  (define (%allocate-frame-list!)
    ;; TODO: check that the order and use of the queues actually makes sense.
    ;; Allocate addresses for the queue heads pointed to from the
    ;; frame list.
    (let ((Q1 (fx+ &queues (* 16 0))) ;every 1ms
          (Q2 (fx+ &queues (* 16 1))) ;every 2ms
          (Q4 (fx+ &queues (* 16 2))) ;etc
          (Q8 (fx+ &queues (* 16 3)))
          (Q16 (fx+ &queues (* 16 4)))
          (Q32 (fx+ &queues (* 16 5)))
          (Q64 (fx+ &queues (* 16 6)))
          (Q128 (fx+ &queues (* 16 7))) ;every 128ms
          ;; Control, bulk and isochronous
          (QISO (fx+ &queues (* 16 8)))
          (QLS (fx+ &queues (* 16 9)))
          (QFS (fx+ &queues (* 16 10))))
      (set! %&frame-list &frame)
      (set! %&queues (vector Q1 Q2 Q4 Q8 Q16 Q32 Q64 Q128 QISO QLS QFS))

      ;; Initialize each queue head and element to T
      (vector-for-each (lambda (&queue)
                         (QH-HEAD-terminate! &queue)
                         (QH-ELEMENT-terminate! &queue))
                       %&queues)

      ;; Set up links between the queues. QFS, QLS and QISO are last
      ;; and appear in every frame. QFS and QLS are for bulk
      ;; transfers; QISO for isochronous; the numbered queues are for
      ;; interrupt transfers.
      (QH-HEAD-link-queue! QLS QISO)
      (QH-HEAD-link-queue! QFS QLS)
      (QH-HEAD-link-queue! Q1 QFS)
      (QH-HEAD-link-queue! Q2 Q1)
      (QH-HEAD-link-queue! Q4 Q2)
      (QH-HEAD-link-queue! Q8 Q4)
      (QH-HEAD-link-queue! Q16 Q8)
      (QH-HEAD-link-queue! Q32 Q16)
      (QH-HEAD-link-queue! Q64 Q32)
      (QH-HEAD-link-queue! Q128 Q64)

      ;; Set up the frame list. This sets e.g. Q4 to be part of every
      ;; fourth frame, because it is either the first QH or linked
      ;; from there.
      (do ((i 0 (fx+ i 1)))
          ((fx=? i 1024))
        (FLP-link-queue! &frame i
                         (cond ((fx=? 0 (fxmod i 2)) Q1)
                               ((fx=? 1 (fxmod i 4)) Q2)
                               ((fx=? 3 (fxmod i 8)) Q4)
                               ((fx=? 7 (fxmod i 16)) Q8)
                               ((fx=? 15 (fxmod i 32)) Q16)
                               ((fx=? 31 (fxmod i 64)) Q32)
                               ((fx=? 63 (fxmod i 128)) Q64)
                               (else Q128))))
      (dma-flush-writes)))

  (define (reset-and-enable-port port)
    (log/debug "Resetting port " port)
    (PortSCn-set! port PortSC-Reset)
    (reg-flush-writes)
    (millisleep 50)
    (PortSCn-set! port 0)
    (reg-flush-writes)
    (let lp ((i 0))
      (millisleep 10)
      (let ((x (PortSCn-ref port)))
        (cond
          ((eqv? i 10)
           (log/error "Timed out trying to enable the port")
           #f)
          ((eqv? 0 (fxand x PortSC-Connect))
           (log/debug "Nothing attached")
           #f)
          ((not (eqv? 0 (fxand x (fxior PortSC-Connect-Change PortSC-Enable-Change))))
           (PortSCn-set! port (fxior PortSC-Connect-Change PortSC-Enable-Change))
           (lp (fx+ i 1)))
          ((not (eqv? 0 (fxand x PortSC-Enable)))
           (log/debug "Port " port " enabled!")
           #t)
          (else
           (PortSCn-set! port PortSC-Enable)
           (lp (fx+ i 1)))))))

  ;; Perform a control transfer and read back a response from the
  ;; device
  (define (perform-devreq/response dev endpoint devreq timeout)
    ;; TODO: Use the technique described on page 11-15 and also fetch
    ;; the actual length of the received data
    (let ((address (usb-device-address dev))
          (low-speed? (eq? (usb-device-speed dev) 'low))
          (bytes/req (usb-device-max-packet-size-0 dev)))
      (let* ((total-length (devreq-wLength devreq))
             ;; Manual memory management for the &probe-mem page
             (&td0 &probe-mem)             ;TDs
             (&td1 (fx+ &td0 TD-size))
             (&data-setup (fx+ &probe-mem 1024)) ;SETUP data
             (&data-in (fx+ &probe-mem 2048)))  ;IN data
        (assert (fx<? (bytevector-length devreq) 1024))
        (assert (fx<? total-length 2048)) ;1280 is max anyway
        (log/debug "-> " devreq)
        (copy-bytevector-to-memory devreq &data-setup)
        (TD-link-td!     &td0 &td1)
        (TD-set-control! &td0 low-speed? #f #f)
        (TD-set-token!   &td0 (bytevector-length devreq) DATA0 endpoint address PID-SETUP)
        (TD-set-address! &td0 &data-setup)
        (let ((&last-td
               (do ((index 0 (fx+ index bytes/req))
                    (&td &td1 (fx+ &td TD-size))
                    (&data-in &data-in (fx+ &data-in bytes/req))
                    (D DATA1 (fxxor D 1)))
                   ((fx>=? index total-length)
                    &td)
                 ;; Read the next part of the response
                 (let ((data-in-len (fxmin (fx- total-length index) bytes/req)))
                   (assert (fx<? (fx+ &td TD-size) &data-in))
                   (assert (fx<? (fx- (fx+ &data-in data-in-len) &probe-mem) 4096))
                   (TD-link-td!     &td (fx+ &td TD-size))
                   (TD-set-control! &td low-speed? #f #f)
                   (TD-set-token!   &td data-in-len D endpoint address PID-IN)
                   (TD-set-address! &td &data-in)))))
          ;; Acknowledge receipt of the response
          (TD-link-terminate! &last-td)
          (TD-set-control!    &last-td low-speed? #f #f)
          (TD-set-token!      &last-td 0 DATA1 endpoint address PID-OUT)
          (TD-set-address!    &last-td 0)
          ;; Wait for the transfer to complete
          (schedule-insert-td (if low-speed? idx-QLS idx-QFS) &td0)
          (let ((status (await-transfer-descriptor &last-td timeout)))
            (case status
              ((ok)
               ;; TODO: a better API will take the bytevector as an argument
               (let ((resp (copy-memory-to-bytevector &data-in (make-bytevector total-length))))
                 (log/debug "<- " resp)
                 (values 'ok resp)))
              (else
               (log/debug "TD0: " (TD->list &td0))
               (log/debug "Last TD: " (TD->list &last-td))
               (values status #f))))))))

  ;; TODO: perform-devreq/output

  ;; Synchronous bulk transfer
  (define (perform-bulk dev endpoint data timeout)
    (assert (not (eq? (usb-device-speed dev) 'low)))
    ;; XXX: need special a special case for this?
    (assert (not (equal? data #vu8())))
    (let ((address (usb-device-address dev))
          (PID (if (endpoint:host->device? endpoint) PID-OUT PID-IN))
          (bytes/req 64)) ;FIXME: get this from the endpoint descriptor
      (let* ((total-length (bytevector-length data))
             ;; Manual memory management for the &probe-mem page
             (&td0 &probe-mem)
             (&data0 (fx+ &probe-mem 2048)))
        (assert (fx<? (bytevector-length data) 2048))
        (when (endpoint:host->device? endpoint)
          (log/debug "-> (" endpoint ") " data)
          (copy-bytevector-to-memory data &data0))
        (do ((index 0 (fx+ index bytes/req))
             (&td &td0 (fx+ &td TD-size))
             (&prev #f &td)
             (&data &data0 (fx+ &data bytes/req))
             (D DATA0 (fxxor D 1)))
            ((fx>=? index total-length)
             ;; Wait for the transfer to complete
             (schedule-insert-td idx-QFS &td0)
             (let ((status (await-transfer-descriptor &prev timeout)))
               (case status
                 ((ok)
                  ;; TODO: get the actual length of the data
                  (cond ((endpoint:host->device? endpoint)
                         (values 'ok 0))
                        (else
                         (copy-memory-to-bytevector &data0 data)
                         (log/debug "<- (" endpoint ") " data)
                         (values 'ok 0))))
                 (else
                  (log/debug "TD0: " (TD->list &td0))
                  (log/debug "Last TD: " (TD->list &td))
                  (values status #f)))))
          ;; Enqueue the next packet
          (let ((packet-len (fxmin (fx- total-length index) bytes/req)))
            (assert (fx<? (fx+ &td TD-size) &data))
            (when &prev
              (TD-link-td! &prev &td))
            (TD-link-terminate! &td)
            (TD-set-control! &td #f #f #f)
            (TD-set-token!   &td packet-len D endpoint address PID)
            (TD-set-address! &td &data))))))

  (define (set-address dev new-address)
    (let ((low-speed? (eq? (usb-device-speed dev) 'low))
          (address (usb-device-address dev)))
      (log/debug "Setting address...")
      (let* ((&td0 &probe-mem)
             (&td1 (fx+ &td0 TD-size))
             (&data-setup (fx+ &td1 TD-size)))
        (let ((bv (make-devreq-set-address new-address)))
          (log/debug "-> " bv)
          (copy-bytevector-to-memory bv &data-setup)
          ;; Set the address
          (TD-link-td!     &td0 &td1)
          (TD-set-control! &td0 low-speed? #f #f)
          (TD-set-token!   &td0 (bytevector-length bv) DATA0 EndPt0 address PID-SETUP)
          (TD-set-address! &td0 &data-setup))
        ;; Read back the status
        (TD-link-terminate! &td1)
        (TD-set-control!    &td1 low-speed? #f #f)
        (TD-set-token!      &td1 Length0 DATA1 EndPt0 address PID-IN)
        (TD-set-address!    &td1 0)       ;unused
        ;; Wait for the transfer to complete
        (schedule-insert-td (if low-speed? idx-QLS idx-QFS) &td0)
        (case (await-transfer-descriptor &td1 10)
          ((ok)
           (log/debug "Address set to " new-address)
           new-address)
          (else
           (log/debug "Error setting the address")
           (log/debug "TD0: " (TD->list &td0))
           (log/debug "TD1: " (TD->list &td1))
           #f)))))

  (define (set-configuration dev configuration)
    (let ((low-speed? (eq? (usb-device-speed dev) 'low))
          (address (usb-device-address dev)))
      (log/debug "Setting configuration...")
      (let* ((&td0 &probe-mem)
             (&td1 (fx+ &td0 TD-size))
             (&data-setup (fx+ &td1 TD-size)))
        (let ((bv (make-devreq-set-configuration address)))
          (log/debug "-> " bv)
          (copy-bytevector-to-memory bv &data-setup)
          ;; Set the configuration
          (TD-link-td!     &td0 &td1)
          (TD-set-control! &td0 low-speed? #f #f)
          (TD-set-token!   &td0 (bytevector-length bv) DATA0 EndPt0 address PID-SETUP)
          (TD-set-address! &td0 &data-setup))
        ;; Read back the status
        (TD-link-terminate! &td1)
        (TD-set-control!    &td1 low-speed? #f #f)
        (TD-set-token!      &td1 Length0 DATA1 EndPt0 address PID-IN)
        (TD-set-address!    &td1 0)       ;unused
        ;; Wait for the transfer to complete
        (schedule-insert-td (if low-speed? idx-QLS idx-QFS) &td0)
        (case (await-transfer-descriptor &td1 10)
          ((ok)
           (log/debug "Configuration set to " configuration)
           #t)
          (else
           (log/debug "Error setting the configuration")
           (log/debug "TD0: " (TD->list &td0))
           (log/debug "TD1: " (TD->list &td1))
           #f)))))

  (define %num-ports
    (begin
      (unless (detect-controller)
        (error 'uhci-driver "The UHCI controller did not do its dance"
               reg-type reg-base reg-size))
      (init-controller)
      (count-ports)))

  (define shutdown-cvar (make-cvar))
  (define (cleanup)
    (signal-cvar! shutdown-cvar)
    (uninit-controller)
    (dma-free &probe-mem)
    (dma-free &frame)
    (dma-free &queues))

;;; UHCI Root hub

  (define (fxtest a b) (not (eqv? 0 (fxand a b))))

  (define (fxtest-set-bit x PortSC-mask bit)
    (if (fxtest x PortSC-mask) (fxarithmetic-shift-left 1 bit) 0))

  (define (PortSCn->wPortStatus x)
    ;; Bit positions
    (define PORT_CONNECTION   0)
    (define PORT_ENABLE       1)
    (define PORT_SUSPEND      2)
    (define PORT_OVER_CURRENT 3)
    (define PORT_RESET        4)
    (define PORT_POWER        8)
    (define PORT_LOW_SPEED    9)
    (fxior (fxtest-set-bit x PortSC-Connect PORT_CONNECTION)
           (fxtest-set-bit x PortSC-Enable PORT_ENABLE)
           (fxtest-set-bit x PortSC-Suspend PORT_SUSPEND)
           (fxtest-set-bit x PortSC-Reset PORT_RESET)
           (fxtest-set-bit x PortSC-Low-Speed PORT_LOW_SPEED)
           (fxarithmetic-shift-left 1 PORT_POWER)))

  (define (PortSCn->wChangeStatus x)
    ;; Bit positions
    (define C_PORT_CONNECTION   0)
    (define C_PORT_ENABLE       1)
    (define C_PORT_SUSPEND      2)
    (define C_PORT_OVER_CURRENT 3)
    (define C_PORT_RESET        4)
    (fxior (fxtest-set-bit x PortSC-Connect-Change C_PORT_CONNECTION)
           (fxtest-set-bit x PortSC-Enable-Change C_PORT_ENABLE)
           ;; TODO: Implement these two properly
           #;C_PORT_SUSPEND
           (fxarithmetic-shift-left 1 C_PORT_RESET)))

  (define (handle-hub-request ch req)
    (match req
      ;; [('ClearHubFeature port) #f]
      ;; [('ClearPortFeature port) #f]
      ;; [('GetBusState port) #f]
      [#('GetHubDescriptor 0)
       ;; Create a hub descriptor for UHCI
       (let ((x (pack "<uCCCSCC CC" (format-size "<uCCCSCC CC")
                      #x29 %num-ports
                      ;; Ganged port power switching; not a compound
                      ;; device; global over-current protection
                      0
                      ;; Wait 10 ms for good power after power on
                      10/2 0 #xff #xff)))
         (put-message ch (cons 'ok x)))]
      ;; [('GetHubStatus port) #f]
      [#('GetPortStatus port)
       (if (not (fx<? -1 port %num-ports))
           (put-message ch (cons 'fail 'bad-port))
           (let ((x (PortSCn-ref port)))
             (let ((wPortStatus (PortSCn->wPortStatus x))
                   (wChangeStatus (PortSCn->wChangeStatus x)))
               (put-message ch (cons 'ok (pack "<SS" wPortStatus wChangeStatus))))))]
      ;; [('SetHubDescriptor port) #f]
      ;; [('SetHubFeature port) #f]
      [#('SetPortFeature feature port)
       (if (not (fx<? -1 port %num-ports))
           (put-message ch (cons 'fail 'bad-port))
           (let* ((x (PortSCn-ref port))
                  (ok (case feature
                        [(PORT_RESET)
                         ;; TODO: keep track of "reset change" and do
                         ;; the reset in the background so the root
                         ;; hub is not blocked here
                         (reset-and-enable-port port)
                         #t]
                        [(PORT_SUSPEND)
                         (PortSCn-set! port (fxior PortSC-Suspend (fxand x (fxnot PortSC-R/WC-mask))))
                         (reg-flush-writes)
                         #t]
                        [else #f])))
             (put-message ch (if ok '(ok . #vu8()) '(fail . bad-request)))))]
      [#('ClearPortFeature feature port)
       (if (not (fx<? -1 port %num-ports))
           (put-message ch (cons 'fail 'bad-port))
           (let* ((x (PortSCn-ref port))
                  (ok (case feature
                        [(C_PORT_RESET)
                         ;; FIXME: See PORT_RESET above
                         #t]
                       [(PORT_SUSPEND)
                        (PortSCn-set! port (fxand x (fxnot (fxior PortSC-Suspend PortSC-R/WC-mask))))
                        #t]
                       [(PORT_ENABLE)
                        (PortSCn-set! port (fxand x (fxnot (fxior PortSC-Enable PortSC-R/WC-mask))))
                        #t]
                       [(C_PORT_CONNECTION)
                        (PortSCn-set! port (fxior PortSC-Connect-Change
                                                  (fxand x (fxnot PortSC-R/WC-mask))))
                        #t]
                       [(C_PORT_ENABLE)
                        (PortSCn-set! port (fxior PortSC-Enable-Change
                                                  (fxand x (fxnot PortSC-R/WC-mask))))
                        #t]
                       [else #f])))
             (reg-flush-writes)
             (put-message ch (if ok '(ok . #vu8()) '(fail . bad-request)))))]
      [_
       (put-message ch (cons 'fail 'unknown-req))]))

  (define (root-hub-task)
    (define (change-report)
      ;; Build a byte like the one sent in a hub interrupt endpoint
      (let lp ((port 0)
               (change-report 0))
        (if (fx=? port %num-ports)
            change-report
            (lp (fx+ port 1)
                (if (fxzero? (fxand (PortSCn-ref port) PortSC-Connect-Change))
                    change-report
                    (fxior change-report (fxarithmetic-shift-left 1 (fx+ port 1))))))))

    (define request-ch (usb-hub-request-channel root-hub))
    (define notify-ch (usb-hub-notify-channel root-hub))

    (let loop ((sleep-op (sleep-operation 1))
               (notify-op #f))
      (match (perform-operation
              (choice-operation
               (wrap-operation (wait-operation shutdown-cvar) (lambda _ 'shutdown))
               (wrap-operation (get-operation request-ch) (lambda (req) (cons 'req req)))
               (if notify-op
                   (wrap-operation notify-op (lambda _ 'notified))
                   (wrap-operation sleep-op (lambda _ 'sleep)))))
        ['sleep
         ;; Every interval we probe the ports for change indications
         ;; and prepare to send them. The change indication bit is
         ;; cleared by writing it, which will happen when the port is
         ;; tested.
         (let ((changes (change-report)))
           (log/debug "Changes: #b" (number->string changes 2))
           (loop (sleep-operation 1) (put-operation notify-ch changes)))]
        ['notified
         ;; Someone received our notification
         (loop sleep-op #f)]
        [('req ch . req)
         (log/debug "root hub req: " req)
         (handle-hub-request ch req)
         (loop sleep-op notify-op)]
        ['shutdown #f])))

;;; Main loop

  (define (handle-hci-request req)
    (match req
      [('perform-devreq/response ch dev endpoint devreq timeout)
       (let-values ([(status resp) (perform-devreq/response dev endpoint devreq timeout)])
         (put-message ch (cons status resp)))]
      [('perform-bulk ch dev endpoint data timeout)
       (let-values ([(status resp) (perform-bulk dev endpoint data timeout)])
         (put-message ch (cons status resp)))]
      ;; FIXME: Is this necessary?
      [('set-configuration ch dev configuration)
       (put-message ch (if (set-configuration dev configuration) (cons 'ok configuration) '(fail . #f)))]
      ;; This is a special case in how the packets are sent since the
      ;; address changes in the middle of the request
      [('set-address ch dev address)
       (put-message ch (if (set-address dev address) (cons 'ok address) '(fail . #f)))]
      [(req-type ch . _)
       (log/debug "unknown request: " req-type)
       (put-message ch (cons 'fail 'unknown-req))]))

  (define root-hub (usb-controller-root-hub controller))
  (define request-ch (usb-controller-request-channel controller))
  (define notify-ch (usb-controller-notify-channel controller))

  (define TIMEOUT (sleep-operation 30)) ;DEBUGGING

  (log/debug "Detected " %num-ports " USB ports")

  (guard (exn
          (else
           (display "UHCI crashed\n" (current-error-port))
           (write exn (current-error-port))
           (newline)
           (cleanup)
           (raise exn)))
    (spawn-fiber root-hub-task)
    (spawn-fiber (lambda ()
                   (usb-enumerator controller shutdown-cvar)))
    (let loop ()
      (let ((x (perform-operation
                (choice-operation
                 (wrap-operation (get-operation request-ch)
                                 (lambda (req) (cons 'req req)))
                 (wrap-operation TIMEOUT (lambda _ '(stop)))))))
        (case (car x)
          ((req)
           (handle-hci-request (cdr x))
           (loop)))))
    (signal-cvar! shutdown-cvar)
    (display "UHCI exited\n" (current-error-port))
    (cleanup)))

;; Check that this is a device the driver supports
(define (probe·pci·uhci? dev)
  (and (eqv? (pcidev-base-class dev) #x0c)
       (eqv? (pcidev-sub-class dev) #x03)
       (eqv? (pcidev-interface dev) #x00)))

;; Main procedure for UHCI devices connected by PCI
(define (driver·pci·uhci dev controller)
  (let ((bar (vector-ref (pcidev-BARs dev) 4)))
    ;; Disable keyboard and mouse legacy support
    (pci-put-u16 dev #xC0 #x0000)
    (driver·uhci (if (pcibar-i/o? bar) 'i/o 'mem)
                 (pcibar-base bar)
                 (pcibar-size bar)
                 (pcidev-irq dev)
                 controller)))

)
