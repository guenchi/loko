;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; 8250 UART driver

;; TODO: A natural extension is to take a register width, since these
;; same UARTs often show up with e.g. u32 registers.

(library (loko drivers uart ns8250)
  (export
    driver·uart)
  (import
    (rnrs (6))
    (only (loko system $host) enable-irq acknowledge-irq wait-irq-operation)
    (loko match)
    (loko u8rings)
    (loko system fibers)
    (loko system unsafe))

(define (driver·uart i/o-base irq read-ch write-ch)
  ;; Registers
  (define RBR (fx+ i/o-base 0))
  (define THB (fx+ i/o-base 0))
  (define IER (fx+ i/o-base 1))
  (define IIR (fx+ i/o-base 2))
  (define FCR (fx+ i/o-base 2))
  (define LCR (fx+ i/o-base 3))
  (define MCR (fx+ i/o-base 4))
  (define LSR (fx+ i/o-base 5))
  (define MSR (fx+ i/o-base 6))
  (define SCR (fx+ i/o-base 7))
  ;; Accessible when LCR[7]=1
  (define DL (fx+ i/o-base 0))
  (define DH (fx+ i/o-base 1))
  ;; Bits in various registers
  (define IIR-INTERRUPT-PENDING #b1)
  (define IER-READ #b01)
  (define IER-R/W #b11)
  (define IER-LINE-STATUS #b100)
  (define IER-MODEM-STATUS #b1000)
  (define LSR-DATA-READY #b0000001)
  (define LSR-RBR-EMPTY  #b1000000)
  (define LSR-THR-EMPTY  #b0100000)
  (define MCR-DTR  #b00000001)
  (define MCR-RTS  #b00000010)
  (define MCR-OUT2 #b00001000)
  ;; FIFO control
  (define FCR-ENABLE           #b00000001)
  (define FCR-CLEAR-RX         #b00000010)
  (define FCR-CLEAR-TX         #b00000100)
  (define FCR-TRIGGER-LEVEL-1  #b00000000)
  (define FCR-TRIGGER-LEVEL-4  #b01000000)
  (define FCR-TRIGGER-LEVEL-8  #b10000000)
  (define FCR-TRIGGER-LEVEL-14 #b11000000)

  (define (ready-to-rx?)
    (eqv? (fxand (get-i/o-u8 LSR) LSR-DATA-READY) LSR-DATA-READY))
  (define (ready-to-tx?)
    (eqv? (fxand (get-i/o-u8 LSR) LSR-THR-EMPTY) LSR-THR-EMPTY))

  (define (uart-set-baudrate rate)
    (let ((latch (fxdiv 115200 rate)))
      (unless (fx<=? 1 latch #xffff)
        (error 'uart-set-baudrate "Invalid baud rate" rate))
      (let ((lcr (get-i/o-u8 LCR)))
        ;; Set the Divisor Latch Access Bit
        (put-i/o-u8 LCR (fxior lcr #x80))
        ;; Latch the new baudrate
        (put-i/o-u8 DL (fxbit-field latch 0 8))
        (put-i/o-u8 DH (fxbit-field latch 8 16))
        ;; Clear the DLAB
        (put-i/o-u8 LCR (fxand lcr #x7f)))))

  (define (uart-tx-loop)
    (let lp ()
      (when (and (not (u8ring-empty? tx-buf))
                 (ready-to-tx?))
        (put-i/o-u8 THB (u8ring-dequeue! tx-buf))
        (lp))))

  (define (uart-set-protocol bits parity stop)
    (let ((par (cdr (assq parity '((none . #b000)
                                   (odd . #b001)
                                   (even . #b011)
                                   (mark . #b101)
                                   (space . #b111)))))
          (bits (cdr (assq bits '((5 . #b00)
                                  (6 . #b01)
                                  (7 . #b10)
                                  (8 . #b11)))))
          (stop (if (eqv? stop 1) 0 1)))
      (put-i/o-u8 LCR (fxior (fxarithmetic-shift-left par 3)
                             (fxarithmetic-shift-left stop 2)
                             bits))))

  (define (uart-handle-irq)
    (let ((iir (get-i/o-u8 IIR)))
      (put-i/o-u8 IER 0)
      (when (eqv? (fxand iir IIR-INTERRUPT-PENDING) 0)
        (case (fxand (fxarithmetic-shift-right iir 1) #b111)
          [(#b010 #b110)
           ;; There is data to read. #b110 = time-out
           (let loop ()
             (when (ready-to-rx?)
               (u8ring-enqueue! rx-buf (get-i/o-u8 RBR))
               (loop)))]
          [(#b000)
           (get-i/o-u8 MSR)]
          [(#b001)
           ;; It's ok to write now
           (uart-tx-loop)]
          [(#b011)
           ;; Receiver Line Status Interrupt
           (get-i/o-u8 LSR)]
          (else #f)))))

  (define (uart-init)
    (uart-set-protocol 8 'none 1)
    (uart-set-baudrate 9600)
    (put-i/o-u8 MCR (fxior MCR-DTR MCR-RTS MCR-OUT2))
    (put-i/o-u8 FCR (fxior FCR-TRIGGER-LEVEL-4 FCR-CLEAR-RX FCR-CLEAR-TX FCR-ENABLE))
    (put-i/o-u8 IER 0))

  (define tx-buf (make-u8ring 1024))
  (define rx-buf (make-u8ring 1024))

  (uart-init)
  (uart-handle-irq)
  (acknowledge-irq irq)
  (enable-irq irq)

  (let lp ((tx-resp-op #f))
    (if (u8ring-empty? tx-buf)
        (put-i/o-u8 IER (fxior IER-READ IER-LINE-STATUS IER-MODEM-STATUS))
        (put-i/o-u8 IER (fxior IER-R/W IER-LINE-STATUS IER-MODEM-STATUS)))
    (match (perform-operation
            (choice-operation
             (wrap-operation (wait-irq-operation irq) (lambda _ 'int))
             (or tx-resp-op
                 (if (u8ring-full? tx-buf)
                     (choice-operation)
                     (wrap-operation (get-operation write-ch) (lambda (x) (cons 'tx x)))))
             (cond ((u8ring-head rx-buf) =>
                    (lambda (byte)
                      (wrap-operation (put-operation read-ch byte) (lambda _ 'rx))))
                   (else (choice-operation)))))
      ['int
       (uart-handle-irq)
       (acknowledge-irq irq)
       (lp tx-resp-op)]
      ['rx
       ;; We've passed on a byte received from the UART
       ;; TODO: Better RX API
       (u8ring-dequeue! rx-buf)
       (lp tx-resp-op)]
      ['tx-ack
       (lp #f)]
      [('tx . #(ch bv start count))
       ;; We've received a bytevector to transmit to the UART. Enqueue
       ;; it and write as much as the UART will accept. Avoids IRQs
       ;; for short bursts. We will respond with how much data we
       ;; accepted.
       (let ((n (u8ring-enqueue-bytevector! tx-buf bv start count)))
         (uart-tx-loop)
         (lp (wrap-operation (put-operation ch n) (lambda _ 'tx-ack))))]))))
