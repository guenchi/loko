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

|#

(library (loko drivers usb uhci)
  (export
    probe·pci·uhci?
    driver·pci·uhci
    driver·uhci)
  (import
    (rnrs (6))
    (loko system fibers)
    (loko system unsafe)
    (only (loko system $host) dma-allocate dma-free
          enable-irq acknowledge-irq wait-irq-operation)
    (loko drivers pci)
    (loko drivers usb core))

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
          ;; Various control, bulk and interrupt
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
      ;; and appear in every frame.
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

  (define (detect-new-devices)
    ;; XXX: The important part here is that only one device at a time
    ;; should have the default address. Anything else would be
    ;; hilarious, of course.
    ;; TODO: This should not block the main loop
    (define (reset-and-create-device port low-speed?)
      (let ((dev0 (make-usb-device controller port 0 8
                                   (if low-speed? 'low 'full) #f)))
        (cond
          ((next-free-address) =>
           (lambda (address)
             (cond
               ((reset-and-enable-port (usb-device-port dev0))
                (cond
                  ((get-device-descriptor dev0 8) =>
                   (lambda (desc)
                     (reset-and-enable-port (usb-device-port dev0))
                     (cond
                       ((set-address dev0 address)
                        (let ((dev (make-usb-device controller port address
                                                    (devdesc-bMaxPacketSize0 desc)
                                                    (usb-device-speed dev0)
                                                    desc)))
                          (cond
                            ((get-device-descriptor dev (desc-bLength desc)) =>
                             (lambda (full-desc)
                               (usb-device-$descriptor-set! dev full-desc)
                               (vector-set! %devices address dev)
                               dev))
                            (else
                             (log/debug "Failed to get full device descriptor on port " port)
                             #f))))
                       (else
                        (log/debug "Failed to assign address to device on port " port)
                        #f))))
                  (else
                   (log/debug "Failed to get device descriptor from port " port)
                   #f)))
               (else
                (log/debug "Failed to reset and enable port " port)
                #f))))
          (else
           (log/debug "No more free USB addresses")
           #f))))
    (do ((port 0 (fx+ port 1)))
        ((eqv? port %num-ports))
      (let* ((x (PortSCn-ref port))
             (low-speed? (fx=? PortSC-Low-Speed (fxand x PortSC-Low-Speed))))
        (unless (fxzero? (fxand x PortSC-Connect-Change))
          (log/debug "Probing new device on " port)
          (cond
            ((reset-and-create-device port low-speed?) =>
             (lambda (dev)
               ;; Send a notification that there's a new device.
               ;; Whoever is listening will be responsible for
               ;; fetching the additional descriptors.
               (spawn-fiber
                (lambda ()
                  (put-message notify-ch (cons 'new-device dev)))))))))))

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

  (define (get-device-descriptor dev total-length)
    (let ((req (make-devreq-get-descriptor desctype-DEVICE 0 0 total-length)))
      (log/debug "Getting " total-length " bytes of the device descriptor")
      (let-values ([(status resp) (perform-devreq/response dev EndPt0 req 100)])
        (unless (eq? status 'ok)
          (log/error "Could not retrieve device descriptor: " status))
        resp)))

  (define (set-address dev new-address)
    (let ((low-speed? (eq? (usb-device-speed dev) 'low))
          (old-address (usb-device-address dev)))
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
          (TD-set-token!   &td0 (bytevector-length bv) DATA0 EndPt0 old-address PID-SETUP)
          (TD-set-address! &td0 &data-setup))
        ;; Read back the status
        (TD-link-terminate! &td1)
        (TD-set-control!    &td1 low-speed? #f #f)
        (TD-set-token!      &td1 Length0 DATA1 EndPt0 old-address PID-IN)
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

  (define (cleanup)
    (uninit-controller)
    (dma-free &probe-mem)
    (dma-free &frame)
    (dma-free &queues))

  (define (handle-request req)
    (let ((req-type (car req))
          (args (cdr req)))
      ;; FIXME: This uses put-message, but that can block
      (case req-type
        [(perform-devreq/response)
         (apply
          (case-lambda
            ((ch dev endpoint devreq timeout)
             (let-values ([(status resp) (perform-devreq/response dev endpoint devreq timeout)])
               (put-message ch (cons status resp)))))
          args)]
        [(perform-bulk)
         (apply
          (case-lambda
            ((ch dev endpoint data timeout)
             (let-values ([(status resp) (perform-bulk dev endpoint data timeout)])
               (put-message ch (cons status resp)))))
          args)]
        [(set-configuration)
         (apply
          (case-lambda
            ((ch dev configuration)
             (set-configuration dev configuration)
             (put-message ch (cons 'ok configuration))))
          args)]
        [else
         (log/debug "unknown request: " req-type args)
         (let ((ch (car args)))
           (put-message ch (cons 'fail 'unknown-req)))])))

;;; Main loop

  (define request-ch (usb-controller-request-channel controller))
  (define notify-ch (usb-controller-notify-channel controller))

  (log/debug "Detected " %num-ports " USB ports")

  (guard (exn
          (else
           (display "UHCI crashed\n" (current-error-port))
           (write exn (current-error-port))
           (newline)
           (cleanup)
           (raise exn)))
    (let loop ((sleep-op (sleep-operation 0.001)))
      (detect-new-devices)
      (let ((x (perform-operation
                (choice-operation
                 (wrap-operation sleep-op (lambda _ '(sleep)))
                 (wrap-operation (get-operation request-ch)
                                 (lambda (req) (cons 'req req)))))))
        (case (car x)
          ((sleep)
           (loop (sleep-operation 0.001)))
          ((req)
           (handle-request (cdr x))
           (loop sleep-op)))))
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
    (driver·uhci (if (pcibar-i/o? bar) 'i/o 'mem)
                 (pcibar-base bar)
                 (pcibar-size bar)
                 (pcidev-irq dev)
                 controller))))
