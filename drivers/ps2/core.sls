;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; PS/2 bus core driver

;; XXX: "ports" in this file are PS/2 ports

(library (loko drivers ps2 core)
  (export
    make-PS/2-controller
    PS/2-controller-notify-channel
    PS/2-controller-command-channel

    make-PS/2-port
    PS/2-port-controller PS/2-port-number
    PS/2-port-rx-channel PS/2-port-tx-channel
    PS/2-read PS/2-write PS/2-command PS/2-flush

    ;; These seem to be common between all PS/2 devices
    PS/2-RESET PS/2-RESEND PS/2-ENABLE-SCANNING PS/2-IDENTIFY

    RESP-PS/2-ACK RESP-PS/2-RESEND RESP-PS/2-ERROR

    driver·PS/2·hotplug)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers))

;; PS/2 commands
(define PS/2-RESET             #xFF)
(define PS/2-RESEND            #xFE)
(define PS/2-ENABLE-SCANNING   #xF4)
(define PS/2-IDENTIFY          #xF2)

;; PS/2 responses
(define RESP-PS/2-ACK          #xFA)
(define RESP-PS/2-RESEND       #xFE)
(define RESP-PS/2-ERROR        #xFF)

;;; Controller abstraction

(define-record-type PS/2-controller
  (sealed #t)
  (fields notify-channel command-channel)
  (protocol
   (lambda (p)
     (lambda ()
       (p (make-channel) (make-channel))))))

(define (PS/2-controller-command controller cmd)
  (let ((ch (make-channel)))
    (put-message (PS/2-controller-command-channel controller) (cons ch cmd))
    (let ((resp (get-message ch)))
      (unless (eq? resp 'ok)
        (error 'PS/2-controller-command "Error from the controller driver"
               controller cmd resp)))))

;;; PS/2 port abstraction

(define-record-type PS/2-port
  (sealed #t)
  (fields controller number rx-channel tx-channel)
  (protocol
   (lambda (p)
     (lambda (controller number)
       (p controller number (make-channel) (make-channel))))))

(define (PS/2-read port timeout)
  (perform-operation
   (choice-operation (if timeout
                         (wrap-operation (sleep-operation (/ timeout 1000))
                                         (lambda _ 'timeout))
                         (choice-operation))
                     (get-operation (PS/2-port-rx-channel port)))))

;; Write a byte to a port. The response shows up on the read port. If
;; there is a timeout then this procedure returns 'timeout; otherwise
;; it returns #f.
(define (PS/2-write port byte timeout)
  ;; (display "=> ")
  ;; (display (number->string byte 16))
  ;; (newline)
  (let ((ch (make-channel)))
    (put-message (PS/2-port-tx-channel port) (vector ch byte timeout))
    (get-message ch)))

;; Send a PS/2 command and handle the response. Scanning needs to be
;; disabled while this procedure is used or the response may be mixed
;; up with scan data.
(define (PS/2-command port byte)
  (PS/2-flush port)
  (let lp ((retries 10))
    (sleep #e0.008)
    (or (PS/2-write port byte 500)
        (let lp-read ((retries retries))
          (let ((resp (PS/2-read port 1000)))
            (cond ((eqv? resp RESP-PS/2-ACK) #t)
                  ((eqv? resp RESP-PS/2-RESEND)
                   (sleep #e0.008)
                   (if (eqv? retries 0)
                       (error 'PS/2-command "Exceeded number of retries")
                       (lp (fx- retries 1))))
                  ((eqv? resp RESP-PS/2-ERROR)
                   (error 'PS/2-command "Error from device"
                          port byte resp))
                  ((eqv? retries 0)
                   (error 'PS/2-command "Unknown response from device"
                          port byte resp))
                  (else
                   ;; Could be junk data; try to resync.
                   (lp-read (fx- retries 1)))))))))

;; Flush the receive buffer
(define (PS/2-flush port)
  (PS/2-controller-command (PS/2-port-controller port)
                           (list 'flush (PS/2-port-number port))))

;;; Basic hotplug driver for when there is no device

(define (driver·PS/2·hotplug port notify-channel)
  ;; FIXME: Reset and wait for #xAA #x00. This is also relevant for
  ;; hotplug. Wait for up to a second (Synaptics).
  (let lp ()
    (let* ((b0 (PS/2-read port #f))
           (b1 (PS/2-read port #f)))
      'TODO)
    (lp))))
