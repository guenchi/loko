;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; PS/2 keyboard driver

;; XXX: "ports" in this file are PS/2 ports

;; A pretty good reference:
;; http://www.scs.stanford.edu/12au-cs140/pintos/specs/kbd/scancodes-9.html

(library (loko drivers ps2 keyboard)
  (export
    probe·PS/2·keyboard
    driver·PS/2·keyboard

    ;; Exported for testing
    driver-PS/2-kbd-scancodes)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers)
    (loko drivers keyboard)
    (loko drivers ps2 core)
    (loko drivers usb hid-numbers))

(define SCANCODE-TIMEOUT 100)
(define COMMAND-TIMEOUT 1000)

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
     ;; XXX: There are different IDs here, like #xAB #x83 etc etc, but
     ;; they all seem to be keyboards anyway.
     (case (PS/2-read port 200)
       ((timeout) #f)
       (else 'MF2-keyboard)))
    (else #f)))

(define (PS/2-kbd-set-scancode-set port set)
  (PS/2-flush port)
  (PS/2-command port PS/2-KBD-SCANCODE-SET)
  (PS/2-command port set))

(define (PS/2-kbd-get-scancode-set port)
  (guard (exn ((error? exn) #f))
    (PS/2-kbd-set-scancode-set port 0)
    (PS/2-read port 1000)))

;; Keyboard translation happens in layers. First bytes from the
;; keyboard are translated from scancode format to make/break codes.
;; These are mapped to USB HUT usage IDs. Bytes from the keyboard can
;; also indicate exceptional events (hotplug, echo response, error
;; codes), and while waiting for a scancode we can also handle a
;; command from the system (e.g. to set LEDs or send an echo request).

;; Translate from PS/2 codes to USB codes. Returns a procedure that
;; takes a scancode and returns usage-page and usage-id. If the key is
;; not decoded, it returns #f and #f.
(define (PS/2-translator codes extended special)
  (lambda (scancode)
    (define (maybe-special scancode)
      (cond ((assv scancode special) =>
             (lambda (usage)
               (values (vector-ref (cdr usage) 0)
                       (vector-ref (cdr usage) 1))))
            (else (values #f #f))))
    (let ((lower (fxbit-field scancode 0 8))
          (upper (fxbit-field scancode 8 16)))
      (cond ((eqv? #x00 upper)
             (if (fx<? lower (bytevector-length codes))
                 (let ((id (bytevector-u8-ref codes lower)))
                   (if (not (eqv? id 0))
                       (values 7 id)
                       (maybe-special scancode)))
                 (maybe-special scancode)))
            ((eqv? #xE0 upper)
             (if (fx<? lower (bytevector-length extended))
                 (let ((id (bytevector-u8-ref extended lower)))
                   (if (not (eqv? id 0))
                       (values 7 id)
                       (maybe-special scancode)))
                 (maybe-special scancode)))
            (else (maybe-special scancode))))))

(define (PS/2-set-1-translator)
  (define codes
    #vu8(#x0 #x29 #x1E #x1F #x20 #x21 #x22 #x23 #x24 #x25 #x26 #x27
         #x2D #x2E #x2A #x2B #x14 #x1A #x8 #x15 #x17 #x1C #x18 #xC
         #x12 #x13 #x2F #x30 #x28 #xE0 #x4 #x16 #x7 #x9 #xA #xB #xD
         #xE #xF #x33 #x34 #x35 #xE1 #x32 #x1D #x1B #x6 #x19 #x5 #x11
         #x10 #x36 #x37 #x38 #xE5 #x55 #xE2 #x2C #x39 #x3A #x3B #x3C
         #x3D #x3E #x3F #x40 #x41 #x42 #x43 #x53 #x47 #x5F #x60 #x61
         #x56 #x5C #x5D #x5E #x57 #x59 #x5A #x5B #x62 #x63 #x0 #x0
         #x64 #x44 #x45 #x67 #x0 #x0 #x8C #x68 #x69 #x6A #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x88 #x0
         #x0 #x87 #x0 #x0 #x94 #x93 #x92 #x8A #x0 #x8B #x0 #x89 #x85
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x91 #x90 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x2 #x0 #x0 #x1))
  (define extended
    #vu8(#x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x58
         #xE4 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x54 #x0 #x46 #xE6 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x48 #x4A
         #x52 #x4B #x0 #x50 #x0 #x4F #x0 #x4D #x51 #x4E #x49 #x4C #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #xE3 #xE7 #x65))
  (define special
    '((#xE066 . #(#xC #x22A)) (#xE067 . #(#xC #x227))
      (#xE068 . #(#xC #x226)) (#xE069 . #(#xC #x225))
      (#xE06A . #(#xC #x224)) (#xE032 . #(#xC #x223))
      (#xE065 . #(#xC #x221)) (#xE06B . #(#xC #x194))
      (#xE021 . #(#xC #x192)) (#xE06C . #(#xC #x18A))
      (#xE06D . #(#xC #x183)) (#xE02E . #(#xC #xEA))
      (#xE030 . #(#xC #xE9)) (#xE020 . #(#xC #xE2))
      (#xE022 . #(#xC #xCD)) (#xE024 . #(#xC #xB7))
      (#xE010 . #(#xC #xB6)) (#xE019 . #(#xC #xB5))
      (#xE11D45 . #(#x7 #x48)) (#xE063 . #(#x1 #x83))
      (#xE05F . #(#x1 #x82)) (#xE05E . #(#x1 #x81))))
  (PS/2-translator codes extended special))

(define (PS/2-set-2-translator)
   (define codes
    #vu8(#x1 #x42 #x0 #x3E #x3C #x3A #x3B #x45 #x0 #x43 #x41 #x3F
         #x3D #x2B #x35 #x67 #x0 #xE2 #xE1 #x88 #xE0 #x14 #x1E #x0
         #x0 #x0 #x1D #x16 #x4 #x1A #x1F #x0 #x0 #x6 #x1B #x7 #x8
         #x21 #x20 #x8C #x0 #x2C #x19 #x9 #x17 #x15 #x22 #x68 #x0
         #x11 #x5 #xB #xA #x1C #x23 #x69 #x0 #x0 #x10 #xD #x18 #x24
         #x25 #x6A #x0 #x36 #xE #xC #x12 #x27 #x26 #x0 #x0 #x37 #x38
         #xF #x33 #x13 #x2D #x0 #x0 #x87 #x34 #x0 #x2F #x2E #x0 #x0
         #x39 #xE5 #x28 #x30 #x0 #x32 #x0 #x94 #x0 #x64 #x93 #x92
         #x8A #x0 #x2A #x8B #x0 #x59 #x89 #x5C #x5F #x85 #x0 #x0 #x62
         #x63 #x5A #x5D #x5E #x60 #x29 #x53 #x44 #x57 #x5B #x56 #x55
         #x61 #x47 #x0 #x0 #x0 #x0 #x40 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x91 #x90
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x2))
  (define extended
    #vu8(#x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #xE6 #x0 #x0 #xE4 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #xE3 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #xE7 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x65 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x54 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x58 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x4D #x0 #x50 #x4A #x0 #x0 #x0 #x49 #x4C #x51 #x0
         #x4F #x52 #x0 #x0 #x0 #x0 #x4E #x0 #x46 #x4B #x48))
  (define special
    '((#xE018 . #(#xC #x22A)) (#xE020 . #(#xC #x227))
      (#xE028 . #(#xC #x226)) (#xE030 . #(#xC #x225))
      (#xE038 . #(#xC #x224)) (#xE03A . #(#xC #x223))
      (#xE010 . #(#xC #x221)) (#xE040 . #(#xC #x194))
      (#xE02B . #(#xC #x192)) (#xE048 . #(#xC #x18A))
      (#xE050 . #(#xC #x183)) (#xE021 . #(#xC #xEA))
      (#xE032 . #(#xC #xE9)) (#xE023 . #(#xC #xE2))
      (#xE034 . #(#xC #xCD)) (#xE03B . #(#xC #xB7))
      (#xE015 . #(#xC #xB6)) (#xE04D . #(#xC #xB5))
      (#xE11477 . #(#x7 #x48)) (#xE05E . #(#x1 #x83))
      (#xE03F . #(#x1 #x82)) (#xE037 . #(#x1 #x81))))
  (PS/2-translator codes extended special))

(define (PS/2-set-3-translator)
  (define codes
    #vu8(#x1 #x0 #x0 #x0 #x0 #x0 #x0 #x3A #x29 #x0 #x0 #x0 #x0 #x2B
         #x35 #x3B #x0 #xE0 #xE1 #x0 #x39 #x14 #x1E #x3C #x0 #xE2
         #x1D #x16 #x4 #x1A #x1F #x3D #x0 #x6 #x1B #x7 #x8 #x21 #x20
         #x3E #x0 #x2C #x19 #x9 #x17 #x15 #x22 #x3F #x0 #x11 #x5 #xB
         #xA #x1C #x23 #x40 #x0 #xE6 #x10 #xD #x18 #x24 #x25 #x41 #x0
         #x36 #xE #xC #x12 #x27 #x26 #x42 #x0 #x37 #x38 #xF #x33 #x13
         #x2D #x43 #x0 #x0 #x34 #x0 #x2F #x2E #x44 #x9A #xE4 #xE5
         #x28 #x30 #x31 #x0 #x45 #x47 #x51 #x50 #x48 #x52 #x4C #x4D
         #x2A #x49 #x0 #x59 #x4F #x5C #x5F #x4E #x4A #x4B #x62 #x63
         #x5A #x5D #x5E #x60 #x53 #x54 #x0 #x58 #x5B #x0 #x57 #x61
         #x55 #x0 #x0 #x0 #x0 #x0 #x56 #x0 #x0 #x0 #x0 #x0 #x0 #xE3
         #xE7 #x65 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0
         #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x2))
  (define extended #vu8())
  (define special
    '((#x7F . #(#x1 #x82)) (#x62 . #(#x1 #xA5))))
  (PS/2-translator codes extended special))

;; This decodes the scancode format and passes the results to the
;; callback. It also forwards commands to the keyboard and passes on
;; responses to the callback.
(define (driver-PS/2-kbd-scancodes port code-set command-ch callback)
  (define (get-byte)
    (let ((b
           (perform-operation
            (choice-operation (wrap-operation (sleep-operation SCANCODE-TIMEOUT) (lambda _ #f))
                              (get-operation (PS/2-port-rx-channel port))))))
      ;; (display "<- ")
      ;; (display (if (fixnum? b) (number->string b 16) b))
      ;; (newline)
      b))
  (define (get-scancode a)
    ;; PS/2 scancodes are sent in the following format (please take
    ;; with a grain of salt, this is untested and applies to set 2):

    ;; <code> ::= E1 <make> <make> || E1 <break> <break> || <extended> || <make>
    ;; <extended> ::= E0 <break> || E0 <make>
    ;; <break> ::= F0 <nn>
    ;; <make> ::= <nn>
    ;; <nn> ::= [0-9A-F][0-9A-F]

    ;; A code with E2 is alleged to exist, but I don't have the
    ;; grammar for that one.

    ;; If in scancode set 1 the highest bit would be set, the other
    ;; codes would instead send an #xF0 byte as a prefix.
    (case a
      ((#xE0)
       (cond ((get-byte) =>
              (lambda (b)
                (cond ((eqv? code-set 1)
                       (if (fxbit-set? b 7)
                           (cons 'break (fxior #xE000 (fxbit-field b 0 7)))
                           (cons 'make (fxior #xE000 b))))
                      ((and (eqv? b #xF0))
                       (cond ((get-byte) =>
                              (lambda (c)
                                (cons 'break (fxior #xE000 c))))
                             (else #f)))
                      (else
                       (cons 'make (fxior #xE000 b))))))
             (else #f)))
      ((#xE1)
       (cond ((get-scancode (get-byte)) =>
              (lambda (ev1)
                (cond ((get-scancode (get-byte)) =>
                       (lambda (ev2)
                         (and (pair? ev1) (pair? ev2)
                              (eq? (car ev1) (car ev2))
                              (cons (car ev1)
                                    (fxior #xE10000 (fxarithmetic-shift-left (cdr ev1) 8) (cdr ev2))))))
                      (else #f))))
             (else #f)))
      ((#xF0)
       (cond ((get-byte) =>
              (lambda (b) (cons 'break b)))
             (else #f)))
      ((#f) #f)
      (else
       (if (and (eqv? code-set 1) (fxbit-set? a 7))
           (cons 'break (fxbit-field a 0 7))
           (cons 'make a)))))
  (define (state-init)
    (match (perform-operation
            (choice-operation (wrap-operation (get-operation command-ch)
                                              (lambda (x) (cons 'cmd x)))
                              (get-operation (PS/2-port-rx-channel port))))
      [(or #x00)
       (callback 'error)
       (state-init)]
      [#xFA
       ;; Acknowledgement for a command
       (state-init)]
      #;
      [(or #xAA #xFC #xFD)
       ;; XXX: #xAA #x00 is the hotplug thing
       (callback 'hotplug)]
      [#xEE
       (callback 'echo)
       (state-init)]
      [(? fixnum? byte)
       ;; (newline)
       ;; (display "<= ")
       ;; (display (number->string byte 16))
       ;; (newline)
       (cond ((get-scancode byte) =>
              (match-lambda
               [(make/break . scancode)
                (let-values ([(page usage) (translator scancode)])
                  (callback (vector make/break (list 'PS/2 code-set scancode)
                                    page usage #f)))])))
       (state-init)]
      [('cmd . cmd)
       (match cmd
         ['echo
          ;; XXX: hm
          (PS/2-write port #xEE COMMAND-TIMEOUT)
          (state-init)]
         [('set-leds state)
          ;; State is according to USB HUT, LED page
          (let ((leds (fxior (if (fxbit-set? state HID-LED-Scroll-Lock) #b1 0)
                             (if (fxbit-set? state HID-LED-Num-Lock) #b10 0)
                             (if (fxbit-set? state HID-LED-Caps-Lock) #b100 0))))
            (PS/2-write port PS/2-KBD-SET-LEDS COMMAND-TIMEOUT)
            (PS/2-write port leds COMMAND-TIMEOUT))
          (state-init)]
         [('set-repeat-rate rate delay)
          (state-init)]
         [cmd
          (state-init)])]
      [x
       (write x)
       (newline)
       (state-init)]))
  (define translator
    (case code-set
      ((1) (PS/2-set-1-translator))
      ((2) (PS/2-set-2-translator))
      ((3) (PS/2-set-3-translator))
      (else (lambda _ (values #f #f)))))
  (state-init))

(define (driver·PS/2·keyboard port hotplug-ch id keyboard)
  ;; There are three types of scan codes: 1 (XT), 2 (AT) and 3 (PS/2).
  ;; Let's try with scancode set 2, the most common one. Sometimes
  ;; keyboards happily accept the command to use set 1 or 3, but
  ;; actually continue to use set 2. There are buggy controllers as
  ;; well and buggy USB legacy emulation BIOSes.
  (define command-ch (keyboard-command-channel keyboard))
  (define (callback event)
    (put-message (keyboard-event-channel keyboard) (cons keyboard event)))
  (PS/2-kbd-set-scancode-set port 2)
  (let ((set (PS/2-kbd-get-scancode-set port)))
    (PS/2-command port PS/2-ENABLE-SCANNING)
    (case set
      ((1 2 3) (driver-PS/2-kbd-scancodes port set command-ch callback))
      ;; The controller is translating to set 1
      ((#x43 #x41 #x3f) (driver-PS/2-kbd-scancodes port 1 command-ch callback))
      (else
       (display "PS/2 keyboard scancode set response not understood: ")
       (write set)
       (newline)
       (driver·PS/2·hotplug port hotplug-ch))))))
