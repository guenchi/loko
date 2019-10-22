;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; PS/2 keyboard driver

;; XXX: "ports" in this file are PS/2 ports

(library (loko drivers ps2 keyboard)
  (export
    probe·PS/2·keyboard
    driver·PS/2·keyboard)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko drivers ps2 core))

;; PS/2 keyboard commands
(define PS/2-KBD-SCANCODE-SET            #xF0)
(define PS/2-KBD-SET-TYPEMATIC-RATE      #xF3)
(define PS/2-KBD-SET-LEDS                #xED)
(define PS/2-KBD-RESET/ENABLE-SCANNING   #xF6)
(define PS/2-KBD-RESET/DISABLE-SCANNING  #xF5)

;; Scancode set 3 commands
(define PS/2-KBD-SET-ALL-TYPEMATIC/AUTORELEASE              #xF7)
(define PS/2-KBD-SET-ALL-MAKE/RELEASE                       #xF8)
(define PS/2-KBD-SET-ALL-MAKE                               #xF9)
(define PS/2-KBD-SET-ALL-TYPEMATIC/AUTORELEASE/MAKE/RELEASE #xFA)
(define PS/2-KBD-SET-ONE-TYPEMATIC/AUTORELEASE              #xFB)
(define PS/2-KBD-SET-ONE-MAKE/RELEASE                       #xFC)
(define PS/2-KBD-SET-ONE-MAKE                               #xFD)

(define LED-SCROLL-LOCK #b001)
(define LED-NUMBER-LOCK #b010)
(define LED-CAPS-LOCK   #b100)

;; PS/2 keyboard responses
(define RESP-PS/2-ERROR-0          #x00)
(define RESP-PS/2-SELF-TEST-OK     #xAA)
(define RESP-PS/2-SELF-TEST-FAIL-1 #xFC)
(define RESP-PS/2-SELF-TEST-FAIL-2 #xFD)
(define RESP-PS/2-ECHO             #xEE)

(define (probe·PS/2·keyboard port)
  (identify port))

(define (identify port)
  (PS/2-flush port)
  (PS/2-command port PS/2-IDENTIFY)
  (case (PS/2-read port 200)
    ((timeout) 'AT-keyboard)
    ((#xAB)
     (case (PS/2-read port 200)
       ((timeout) #f)
       ((#x41 #xC1) 'MF2-keyboard/translation)
       ((#x83) 'MF2-keyboard)
       (else #f)))
    (else #f)))

(define (PS/2-kbd-set-scancode-set port set)
  (PS/2-flush port)
  (PS/2-command port PS/2-KBD-SCANCODE-SET)
  (PS/2-command port set))

(define (PS/2-kbd-get-scancode-set port)
  (guard (exn ((error? exn) #f))
    (PS/2-kbd-set-scancode-set port 0)
    (PS/2-read port 1000)))

(define (driver-PS/2-kbd-set-1 port hotplug-channel KEYBOARD)
  ;; TODO: Implement
  (let loop ()
    (let loop-break ((break #f))
      (let ((scancode (PS/2-read port #f)))
        (case scancode
          ((#xF0)
           (loop-break #t))
          (else
           (put-message KEYBOARD (vector 1 (if break 'release 'press) scancode))
           (loop)))))))

(define (driver-PS/2-kbd-set-2 port hotplug-channel KEYBOARD)
  ;; TODO: Implement fully.
  (let loop ()
    (let loop-ext ((extended 0))
      (let ((scancode (fxior extended (PS/2-read port #f))))
        (case scancode
          ((#xE0)
           ;; Extended scancodes. XXX: It's more complicated than this.
           (loop-ext #xE000))
          (else
           (cond
             ((fxbit-set? scancode 7)
              ;; Released key
              (put-message KEYBOARD (vector 2 'release scancode))
              (loop))
             (else
              ;; Pressed key
              (put-message KEYBOARD (vector 2 'press scancode))
              (loop)))))))))

(define (driver-PS/2-kbd-set-3 port hotplug-channel KEYBOARD)
  (let loop ()
    (let loop-break ((break #f))
      (let ((scancode (PS/2-read port #f)))
        (case scancode
          ((#xF0)
           (loop-break #t))
          (else
           (put-message KEYBOARD (vector 3 (if break 'release 'press) scancode))
           (loop)))))))

#;
(define (ps/2-kbd-maybe-set-scancode-set-3 port)
  (let ((set (PS/2-kbd-get-scancode-set port)))
    (case set
      ((3) set)
      ((#x43 #x41 #x3f)
       ;; Yep. What is this? The hex digits just correspond to 1, 2
       ;; and 3 after translation. This tells us that the PS/2
       ;; controller is in translation mode. Setting any other set now
       ;; can confuse it, e.g. making it translate set 1 as if it were
       ;; originally mode 2. "This shouldn't happen."
       1)
      (else
       (let ((set (guard (exn ((error? exn) #f))
                    ;; Try to use scancode set 3.
                    (PS/2-kbd-set-scancode-set port 3)
                    (PS/2-kbd-get-scancode-set port))))
         (cond ((memv set '(2 3))
                set)                       ;pretty ok
               (else
                ;; Well. Try set 2 at least. Then go with whatever it
                ;; became.
                (guard (exn ((error? exn) #f))
                  (PS/2-kbd-set-scancode-set port 2))
                (PS/2-kbd-get-scancode-set port))))))))

(define (driver·PS/2·keyboard port hotplug-channel id KEYBOARD)
  ;; TODO: Send an echo command during inactivity to see if the device
  ;; has been removed, maybe detect #xAA #x00.

  ;; There are three types of scan codes: 1 (XT), 2 (AT) and 3 (PS/2).
  ;; Let's try with scancode set 2, the most common one.
  (PS/2-kbd-set-scancode-set port 2)
  (let ((set (PS/2-kbd-get-scancode-set port)))
    (PS/2-command port PS/2-ENABLE-SCANNING)
    (case set
      ((3) (driver-PS/2-kbd-set-3 port hotplug-channel KEYBOARD))
      ((2) (driver-PS/2-kbd-set-2 port hotplug-channel KEYBOARD))
      ((1 #x43 #x41 #x3f) (driver-PS/2-kbd-set-1 port hotplug-channel KEYBOARD))
      (else
       (driver·PS/2·hotplug port hotplug-channel))))))
