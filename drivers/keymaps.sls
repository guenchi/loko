;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; Default keyboard maps

;; The key codes are according to the USB HID Usage Tables. It is
;; intended to be used for all types of keyboards. Other scancodes
;; need to be translated to USB HID page/usage numbers in the
;; respective keyboard drivers.

;; This library should eventually provide a fully capable mechanism
;; for keyboard handling, but is currently quite limited.

(library (loko drivers keymaps)
  (export
    map-key
    make-keymap-state
    default-keymap
    keymap-sv/fi)
  (import
    (rnrs (6))
    (loko match)
    (loko drivers usb hid-numbers))

;; Map a key by using a keymap state.
;;
;; Takes a keymap state, the current state of the LEDs and modifiers,
;; the symbol make or break (for press or release), a HID HUT page and
;; usage ID. Returns a new state, new LEDs and modifiers and a
;; mapping to a character or string.
;;
;; XXX: Must not modify the event. It may be processed by other keymaps.
(define (map-key state leds mods event)
  (define (nothing)
    (values state leds mods #f))
  (define (return str/char)
    (values state leds mods str/char))
  (define (toggle-led make? led)
    (if make?
        (values state (fxxor leds (fxarithmetic-shift-left 1 led)) mods #f)
        (nothing)))
  (define (handle-modifier make? idx)
    (let* ((mod (fxarithmetic-shift-left 1 idx))
           (mods (if make?
                     (fxior mods mod)
                     (fxand mods (fxnot mod)))))
      (values state leds mods #f)))
  (define hid-shift-bits
    (fxior (fxarithmetic-shift-left 1 (fx- Keyboard-LeftShift Keyboard-LeftControl))
           (fxarithmetic-shift-left 1 (fx- Keyboard-RightShift Keyboard-LeftControl))))
  (define keymap (keymap-state-keymap state))
  (define modifier-mapping (vector-ref keymap 0))
  (define page7 (vector-ref keymap 1))
  (match event
    (#(make/break _raw page usage #f)
     (let ((make? (eq? make/break 'make)))
       (case page
         ((7)
          (match (and (fx<? usage (vector-length page7))
                      (vector-ref page7 usage))
            [((and (or 'Alphabetic 'Regular 'Keypad) type) . mappings)
             (let* ((fixed-mods
                     (case type
                       ((Alphabetic)
                        (cond ((fxbit-set? leds HID-LED-Caps-Lock)
                               (fxxor mods hid-shift-bits))
                              (else
                               mods)))
                       ((Keypad)
                        (if (fxbit-set? leds HID-LED-Num-Lock)
                            (fxxor mods hid-shift-bits)
                            mods))
                       (else mods)))
                    ;; The index into the translation vector is based
                    ;; on the current mods.
                    (idx (do ((i 0 (fx+ i 1))
                              (mods fixed-mods (fxarithmetic-shift-right mods 1))
                              (idx 0 (if (fxbit-set? mods 0)
                                         (fxior idx (vector-ref modifier-mapping i))
                                         idx)))
                             ((fx=? i (vector-length modifier-mapping))
                              idx))))
               (match (vector-ref mappings idx)
                 [('C combining-mark second-press)
                  ;; TODO: Record the mark in the state (dead-key) and
                  ;; do this properly
                  (return second-press)]
                 [#f (nothing)]
                 [mapping
                  (return mapping)]))]
            (('Modifier . idx)
             (handle-modifier make? idx))
            [('LED . led)
             (toggle-led make? led)]
            [else (nothing)]))
         (else (nothing)))))))

(define-record-type keymap-state
  (sealed #t)
  (fields keymap mods dead-key)
  (protocol
   (lambda (p)
     (lambda (keymap)
       (p keymap 0 #f)))))

;; Converts a somewhat readable keymap to an easily-used keymap. A bit messy.
(define-syntax define-keymap
  (lambda (x)
    (define (list-pad list len fill)
      (append list
              (vector->list
               (make-vector (fxmax 0 (fx- len (length list))) fill))))
    (define (process key-list)
      (define num-translations
        (fx+ 1 (fold-left (lambda (sum key)
                            (let ((type+args (cddr key)))
                              (case (car type+args)
                                ((Modifier) (fxior sum (caddr type+args)))
                                (else sum))))
                          0 key-list)))
      (define num-modifiers
        (length (filter (lambda (x) (eq? (caddr x) 'Modifier)) key-list)))
      (define modifiers (make-vector num-modifiers 0))
      (define page7 (make-vector 256 #f))
      (for-each
       (lambda (key)
         (let ((page (car key)) (code (cadr key))
               (type (caddr key)) (translations (cdddr key)))
           (when (eqv? page HID-page-Keyboard/Keypad)
             (case type
               ((Alphabetic Regular Keypad)
                (vector-set! page7 code
                             (cons type (list->vector
                                         (list-pad translations num-translations #f)))))
               ((LED)
                (let ((led-page (car translations)))
                  (vector-set! page7 code (cons 'LED led-page))))
               ((Modifier)
                (let ((mod-index (car translations))
                      (translation-mask (cadr translations)))
                  (vector-set! modifiers mod-index translation-mask)
                  (vector-set! page7 code (cons 'Modifier mod-index))))
               (else (syntax-violation 'define-keymap "Bad type" x type))))
           key))
       key-list)
      (vector modifiers page7))
    (syntax-case x ()
      [(_ name key* ...)
       (identifier? #'name)
       (with-syntax ((processed
                      (datum->syntax #'name
                                     (process (syntax->datum #'(key* ...))))))
         #'(define name 'processed))])))

;; XXX: This is not complete
(define-keymap keymap-sv/fi
  ;; Page Code Type          Plain               Shift              AltGr          Shift+AltGr
  (#x07 #x04  Alphabetic     #\a                 #\A                               )
  (#x07 #x05  Alphabetic     #\b                 #\B                               )
  (#x07 #x06  Alphabetic     #\c                 #\C                #\©            )
  (#x07 #x07  Alphabetic     #\d                 #\D                               )
  (#x07 #x08  Alphabetic     #\e                 #\E                #\€            )
  (#x07 #x09  Alphabetic     #\f                 #\F                               )
  (#x07 #x0A  Alphabetic     #\g                 #\G                               )
  (#x07 #x0B  Alphabetic     #\h                 #\H                               )
  (#x07 #x0C  Alphabetic     #\i                 #\I                               )
  (#x07 #x0D  Alphabetic     #\j                 #\J                               )
  (#x07 #x0E  Alphabetic     #\k                 #\K                               )
  (#x07 #x0F  Alphabetic     #\l                 #\L                               )
  (#x07 #x10  Alphabetic     #\m                 #\M                               )
  (#x07 #x11  Alphabetic     #\n                 #\N                               )
  (#x07 #x12  Alphabetic     #\o                 #\O                               )
  (#x07 #x13  Alphabetic     #\p                 #\P                               )
  (#x07 #x14  Alphabetic     #\q                 #\Q                               )
  (#x07 #x15  Alphabetic     #\r                 #\R                #\®            )
  (#x07 #x16  Alphabetic     #\s                 #\S                               )
  (#x07 #x17  Alphabetic     #\t                 #\T                               )
  (#x07 #x18  Alphabetic     #\u                 #\U                               )
  (#x07 #x19  Alphabetic     #\v                 #\V                               )
  (#x07 #x1A  Alphabetic     #\w                 #\W                               )
  (#x07 #x1B  Alphabetic     #\x                 #\X                #\»            )
  (#x07 #x1C  Alphabetic     #\y                 #\Y                               )
  (#x07 #x1D  Alphabetic     #\z                 #\Z                #\«            )
  (#x07 #x1E  Regular        #\1                 #\!                #\¡            )
  (#x07 #x1F  Regular        #\2                 #\"                #\@            )
  (#x07 #x20  Regular        #\3                 #\#                #\£            )
  (#x07 #x21  Regular        #\4                 #\¤                #\$            )
  (#x07 #x22  Regular        #\5                 #\%                #\€            )
  (#x07 #x23  Regular        #\6                 #\&                #\¥            )
  (#x07 #x24  Regular        #\7                 #\/                #\{            )
  (#x07 #x25  Regular        #\8                 #\(                #\[            )
  (#x07 #x26  Regular        #\9                 #\)                #\]            )
  (#x07 #x27  Regular        #\0                 #\=                #\}            )
  (#x07 #x28  Regular        #\return            #f                                )
  (#x07 #x29  Regular        #\esc               #f                                )
  (#x07 #x2A  Regular        #\backspace         #f                                )
  (#x07 #x2B  Regular        #\tab               #f                                )
  (#x07 #x2C  Regular        #\space             #f                                )
  (#x07 #x2D  Regular        #\+                 #\?                #\\            )
  (#x07 #x2E  Regular        (C #\x0301 #\´)     (C #\x0300 #\`)    #\±            )
  (#x07 #x2F  Alphabetic     #\å                 #\Å                               )
  (#x07 #x30  Regular        (C #\x0308 #\¨)     (C #\x0302 #\^)    (C #\x0303 #\~))
  (#x07 #x32  Regular        #\'                 #\*                #\´            )
  (#x07 #x33  Alphabetic     #\ö                 #\Ö                #\ø            )
  (#x07 #x34  Alphabetic     #\ä                 #\Ä                #\æ            )
  (#x07 #x35  Regular        #\§                 #\½                #\¶            )
  (#x07 #x36  Regular        #\,                 #\;                (C #\x0327 #\¸))
  (#x07 #x37  Regular        #\.                 #\:                #\·            )
  (#x07 #x38  Regular        #\-                 #\_                (C #\x0323 #\̣))
  ;; The LEDs are mapped to indices in the USB HUT LED page.
  (#x07 #x39  LED            #x02 #;Caps-Lock                                      )
  (#x07 #x47  LED            #x03 #;Scroll-Lock                                    )
  (#x07 #x53  LED            #x01 #;Num-Lock                                       )
  (#x07 #x54  Keypad         #\/                 #\/                               )
  (#x07 #x55  Keypad         #\*                 #\*                               )
  (#x07 #x56  Keypad         #\-                 #\-                               )
  (#x07 #x57  Keypad         #\+                 #\+                               )
  (#x07 #x58  Keypad         #\return            #\return                          )
  (#x07 #x59  Keypad         #f                  #\1                               )
  (#x07 #x5A  Keypad         #f                  #\2                               )
  (#x07 #x5B  Keypad         #f                  #\3                               )
  (#x07 #x5C  Keypad         #f                  #\4                               )
  (#x07 #x5D  Keypad         #f                  #\5                               )
  (#x07 #x5E  Keypad         #f                  #\6                               )
  (#x07 #x5F  Keypad         #f                  #\7                               )
  (#x07 #x60  Keypad         #f                  #\8                               )
  (#x07 #x61  Keypad         #f                  #\9                               )
  (#x07 #x62  Keypad         #f                  #\0                               )
  (#x07 #x63  Keypad         #f                  #\.                               )
  (#x07 #x64  Regular        #\<                 #\>                #\|            )
  (#x07 #x67  Keypad         #\=                                                   )
  ;; The first number is the bit to set in the modifier integer. The
  ;; bitwise sum of the second, for all modifiers currently in effect,
  ;; is used as an index into Alphabetic/Regular/Keypad translations.
  (#x07 #xE0  Modifier       0 #b00   #;LeftControl                                )
  (#x07 #xE1  Modifier       1 #b01   #;LeftShift                                  )
  (#x07 #xE2  Modifier       2 #b00   #;LeftAlt                                    )
  (#x07 #xE3  Modifier       3 #b00   #;LeftGUI                                    )
  (#x07 #xE4  Modifier       4 #b00   #;RightControl                               )
  (#x07 #xE5  Modifier       5 #b01   #;RightShift                                 )
  (#x07 #xE6  Modifier       6 #b10   #;RightAlt                                   )
  (#x07 #xE7  Modifier       7 #b00   #;RightGUI                                   ))

;; Loko starts in the true keymap, Swedish/Finnish (just like Linux
;; did originally).
(define default-keymap keymap-sv/fi))
