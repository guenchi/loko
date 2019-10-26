;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; Keyboard abstraction based on USB HID

;; This library contains a basic keyboard abstraction that handles the
;; channels for a keyboard driver.

;; There is also a keyboard manager. It can tie together multiple
;; keyboards (if so desired) and handles keymaps and LEDs.

(library (loko drivers keyboard)
  (export
    make-keyboard
    keyboard-event-channel
    keyboard-command-channel

    make-keyboard-manager
    make-managed-keyboard
    keyboard-manager-remove-keyboard

    LED-PAGE LED-PAGE-NUM-LOCK LED-PAGE-CAPS-LOCK
    LED-PAGE-SCROLL-LOCK LED-PAGE-COMPOSE)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers))

(define LED-PAGE #x08)
(define LED-PAGE-NUM-LOCK #x01)
(define LED-PAGE-CAPS-LOCK #x02)
(define LED-PAGE-SCROLL-LOCK #x03)
(define LED-PAGE-COMPOSE #x04)

(define-record-type keyboard
  (fields event-channel
          command-channel)
  (protocol
   (lambda (p)
     (case-lambda
       (()
        (p (make-channel) (make-channel)))
       ((event-channel)
        (p event-channel (make-channel)))))))

(define-record-type keyboard-manager
  (parent keyboard)
  (fields mux-event-channel
          (mutable number-keyboards))
  (protocol
   (lambda (p)
     (lambda ()
       (let ((manager-kbd ((p) (make-channel) 0)))
         (spawn-fiber (lambda () (manager·keyboard manager-kbd)))
         manager-kbd)))))

;; Proxy keyboard driver that adds keymap lookup, tracks and updates
;; caps lock, etc., and can bind multiple keyboards together.
(define (manager·keyboard manager-kbd)
  (define (broadcast cmd keyboards)
    (for-each (lambda (kbd)
                (put-message (keyboard-command-channel kbd) cmd))
              keyboards))
  (define (keymap page usage leds mods)
    ;; TODO: Make this handle all keys.
    (let-values ([(sym str)
                  (case page
                    ((7)
                     ;; TODO: Where to take the symbolic name from?
                     ;; These are from X11.
                     (case usage
                       ((#x53) (values 'Num_Lock #f))
                       ((#x39) (values 'Caps_Lock #f))
                       ((#x47) (values 'Scroll_Lock #f))
                       ((#xE1) (values 'Shift_L #f))
                       ((#xE5) (values 'Shift_R #f))
                       ((#x04)
                        (if (or (fxbit-set? mods 5)
                                (fxbit-set? mods 1)
                                (fxbit-set? leds LED-PAGE-CAPS-LOCK))
                            (values 'A "A")
                            (values 'a "a")))
                       (else (values #f #f))))
                    (else (values #f #f)))])
      (vector sym str leds mods)))
  (let lp ((real* '()) (leds 0) (mods 0))
    (match (perform-operation
            (choice-operation (wrap-operation
                               (get-operation
                                (keyboard-command-channel manager-kbd))
                               (lambda (x) (cons 'cmd x)))
                              (wrap-operation
                               (get-operation
                                (keyboard-manager-mux-event-channel manager-kbd))
                               (lambda (x) (cons 'event x)))))
      [('cmd . cmd)
       (match cmd
         ;; This handler adds two commands for adding and removing
         ;; keyboards from the handler. Everything else is broadcast
         ;; to all real keyboards.
         [('new-keyboard . new-kbd)
          (let ((real* (cons new-kbd (remq new-kbd real*))))
            (keyboard-manager-number-keyboards-set! manager-kbd (length real*))
            (lp real* leds mods))]
         [('remove-keyboard . old-kbd)
          (let ((real* (remq old-kbd real*)))
            (keyboard-manager-number-keyboards-set! manager-kbd (length real*))
            (lp real* leds mods))]
         [else
          (broadcast cmd real*)
          (lp real* leds mods)])]
      [('event . event)
       ;; This passes the event to the keymap and handles the buttons
       ;; that toggle the LEDs.
       (letrec ((handle-event
                 (match-lambda
                   [#(make/break set scancode page usage #f)
                    (let* ((symbolic (keymap page usage leds mods))
                           (new-event (vector make/break set scancode page usage symbolic))
                           (sym (vector-ref symbolic 0)))
                      (case sym
                        [(Num_Lock Scroll_Lock Caps_Lock)
                         (if (eq? make/break 'make)
                             (let* ((led (case sym
                                           ([Num_Lock] LED-PAGE-NUM-LOCK)
                                           ([Scroll_Lock] LED-PAGE-SCROLL-LOCK)
                                           (else LED-PAGE-CAPS-LOCK)))
                                    (leds (fxxor leds (fxarithmetic-shift-left 1 led))))
                               (broadcast `(set-leds ,leds) real*)
                               (values new-event leds mods))
                             (values new-event leds mods))]
                        ;; FIXME: Generic handling
                        [(Shift_R)
                         (let ((mods (if (eq? make/break 'make)
                                         (fxior mods #x20)
                                         (fxand mods (fxnot #x20)))))
                           (values new-event leds mods))]
                        [(Shift_L)
                         (let ((mods (if (eq? make/break 'make)
                                         (fxior mods #x02)
                                         (fxand mods (fxnot #x02)))))
                           (values new-event leds mods))]
                        [else (values new-event leds mods)]))]
                   [event
                    (values event leds mods)])))
         (let-values ([(event leds mods) (handle-event event)])
           (put-message (keyboard-event-channel manager-kbd) event)
           (lp real* leds mods)))])))

;; Make a new keyboard that is managed by the given manager. The event
;; channel of the new keyboard is directed to the manager's event
;; channel, so all managed keyboards appear to be the same.
(define (make-managed-keyboard manager)
  (let ((kbd (make-keyboard (keyboard-manager-mux-event-channel manager))))
    (put-message (keyboard-command-channel manager) `(new-keyboard . ,kbd))
    kbd))

;; Remove a keyboard from a keyboard manager
(define (keyboard-manager-remove-keyboard manager-kbd kbd)
  (put-message (keyboard-command-channel manager-kbd) `(remove-keyboard . ,kbd))))
