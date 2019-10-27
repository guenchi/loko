;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; USB HID definitions

(library (loko drivers usb hid-numbers)
  (export
    HID-page-LED
    HID-LED-Num-Lock
    HID-LED-Caps-Lock
    HID-LED-Scroll-Lock
    HID-LED-Compose
    HID-LED-Kana

    HID-page-Keyboard/Keypad
    Keyboard-LeftControl
    Keyboard-LeftShift
    Keyboard-LeftAlt
    Keyboard-LeftGUI
    Keyboard-RightControl
    Keyboard-RightShift
    Keyboard-RightAlt
    Keyboard-RightGUI)
  (import
    (only (rnrs (6))
          define-syntax syntax-rules identifier-syntax))

(define-syntax define-inlined
  (syntax-rules ()
    ((_ name v)
     (define-syntax name (identifier-syntax v)))))

(define-inlined HID-page-LED         #x08)
(define-inlined HID-LED-Num-Lock     #x01)
(define-inlined HID-LED-Caps-Lock    #x02)
(define-inlined HID-LED-Scroll-Lock  #x03)
(define-inlined HID-LED-Compose      #x04)
(define-inlined HID-LED-Kana         #x05)

(define-inlined HID-page-Keyboard/Keypad #x07)
(define-inlined Keyboard-LeftControl     #xE0)
(define-inlined Keyboard-LeftShift       #xE1)
(define-inlined Keyboard-LeftAlt         #xE2)
(define-inlined Keyboard-LeftGUI         #xE3)
(define-inlined Keyboard-RightControl    #xE4)
(define-inlined Keyboard-RightShift      #xE5)
(define-inlined Keyboard-RightAlt        #xE6)
(define-inlined Keyboard-RightGUI        #xE7))
