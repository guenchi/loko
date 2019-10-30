;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; PS/2 mouse driver

;; XXX: "ports" in this file are PS/2 ports

(library (loko drivers ps2 mouse)
  (export
    probe·PS/2·mouse
    driver·PS/2·mouse)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko drivers mouse)
    (loko drivers ps2 core))

(define PS/2-MOUSE-SET-RESOLUTION   #xE8)
(define PS/2-MOUSE-SET-SAMPLE-RATE  #xF3)
(define PS/2-MOUSE-DISABLE-SCANNING #xF5)

(define RESOLUTION-1/mm #x00)
(define RESOLUTION-2/mm #x01)
(define RESOLUTION-4/mm #x02)
(define RESOLUTION-8/mm #x03)

(define (probe·PS/2·mouse port)
  (PS/2-flush port)
  (identify port))

(define (identify port)
  (PS/2-command port PS/2-IDENTIFY)
  (case (PS/2-read port 200)
    ((#x00) 'mouse)
    ((#x03) 'mouse/z-axis)
    ((#x04) 'mouse/5-buttons)
    (else #f)))

;; Set the resolution (1-8 counts/mm)
(define (PS/2-mouse-set-resolution port value)
  (PS/2-command port PS/2-MOUSE-SET-RESOLUTION)
  (PS/2-command port value))

;; Set the sample rate (10, 20, 40, 60, 80, 100, 200 Hz)
(define (PS/2-mouse-set-sample-rate port rate)
  (PS/2-command port PS/2-MOUSE-SET-SAMPLE-RATE)
  (PS/2-command port rate))

(define (probe-mouse-type port device-type)
  ;; Mouse-compatible devices start out reporting themselves as a
  ;; plain old mouse. If we have a driver for newer protocol then we
  ;; can try to enable it and send a new IDENTIFY.
  (case device-type
    ((mouse)
     ;; Try to enable the Z axis
     (PS/2-mouse-set-sample-rate port 200)
     (PS/2-mouse-set-sample-rate port 100)
     (PS/2-mouse-set-sample-rate port 80))
    ((mouse/z-axis)
     ;; Try to enable the 4th and 5th buttons
     (PS/2-mouse-set-sample-rate port 200)
     (PS/2-mouse-set-sample-rate port 200)
     (PS/2-mouse-set-sample-rate port 80)))
  ;; Let's see if the device type changed
  (let ((new-type (identify port)))
    (if (eq? new-type device-type)
        device-type
        (probe-mouse-type port new-type))))

;; Read a mouse data packet, either 3 or 4 bytes
(define (get-mouse-data-packet port long-report)
  (let lp ()
    (let ((b1 (PS/2-read port #f)))
      (if (not (fxbit-set? b1 3))
          (lp)                          ;not the start byte
          (let* ((b2 (PS/2-read port 10))
                 (b3 (PS/2-read port 10)))
            (if (not (and (fixnum? b2) (fixnum? b3)))
                (lp)                    ;timed out
                (if (not long-report)
                    (values b1 b2 b3)
                    (let ((b4 (PS/2-read port 10)))
                      (if (not (fixnum? b4))
                          (lp)          ;timed out (FIXME: wrong protocol?)
                          (values b1 b2 b3 b4))))))))))

(define (parse-mouse-flags flags x y)
  (define (decode-signed-9-bit sign byte)
    (fx- byte (fxarithmetic-shift-left sign 8)))
  (let ((y-overflow (fxand flags #b10000000))
        (x-overflow (fxand flags #b01000000))
        (y-sign     (fxand flags #b00100000))
        (x-sign     (fxand flags #b00010000)))
    (values (decode-signed-9-bit (fxarithmetic-shift-right x-sign 4) x)
            (decode-signed-9-bit (fxarithmetic-shift-right y-sign 5) y))))

(define (driver·PS/2·mouse port hotplug-channel device-type mouse)
  (define (decode-s8 x)
    (if (fx>? x (fx- (fxarithmetic-shift-left 1 7) 1))
        (fx- x (fxarithmetic-shift-left 1 8))
        x))
  (define (decode-s4 x)
    (if (fx>? x (fx- (fxarithmetic-shift-left 1 3) 1))
        (fx- x (fxarithmetic-shift-left 1 4))
        x))
  ;; TODO: Send an echo command during inactivity to see if the device
  ;; has been removed, maybe detect #xAA #x00. TODO: Handle commands
  ;; on the mouse-command-channel.
  (let ((device-type (probe-mouse-type port device-type)))
    (PS/2-mouse-set-sample-rate port 60)
    (case device-type
      ((mouse)
       (PS/2-command port PS/2-ENABLE-SCANNING)
       (let loop ()
         (let*-values ([(flags x y) (get-mouse-data-packet port #f)]
                       [(x y) (parse-mouse-flags flags x y)])
           (let ((buttons (fxand flags #b00000111))
                 (z 0))
             (put-message (mouse-event-channel mouse) (vector x y z buttons #f))
             ;; (display (list x y 0 buttons))
             ;; (newline)
             (loop)))))
      ((mouse/z-axis)
       (PS/2-command port PS/2-ENABLE-SCANNING)
       (let loop ()
         (let*-values ([(flags x y z) (get-mouse-data-packet port #t)]
                       [(x y) (parse-mouse-flags flags x y)])
           (let ((buttons (fxand flags #b00000111))
                 (z (decode-s8 z)))
             (put-message (mouse-event-channel mouse) (vector x y z buttons #f))
             ;; (display (list x y z (number->string buttons 2)))
             ;; (newline)
             (loop)))))
      ((mouse/5-buttons)
       (PS/2-command port PS/2-ENABLE-SCANNING)
       (let loop ()
         (let*-values ([(flags x y z/btn) (get-mouse-data-packet port #t)]
                       [(x y) (parse-mouse-flags flags x y)])
           (let ((buttons (fxior (fxand flags #b00111)
                                 (fxarithmetic-shift-right (fxand z/btn #b110000) 1)))
                 (z (decode-s4 (fxbit-field z/btn 0 4))))
             (put-message (mouse-event-channel mouse) (vector x y z buttons #f))
             ;; (display (list x y z (number->string buttons 2)))
             ;; (newline)
             (loop)))))
      (else
       (error 'driver·PS/2·mouse "Unsupported device type" port device-type))))))
