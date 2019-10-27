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
    keyboard-manager-remove-keyboard)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (loko match)
    (loko system fibers)
    (loko drivers keymaps))

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
       ;; The manager's mux event channel is the one that the managed
       ;; keyboards send their events on. These are processed and
       ;; forwarded to the regular event channel.
       (let ((manager-kbd ((p) (make-channel) 0)))
         (spawn-fiber (lambda () (manager·keyboard manager-kbd)))
         manager-kbd)))))

;; Proxy keyboard driver that adds keymap lookup, tracks and updates
;; caps lock, etc., and can bind multiple keyboards together.
(define (manager·keyboard manager-kbd)
  (define (broadcast cmd keyboards)
    (for-each (lambda (kbd+state)
                (put-message (keyboard-command-channel (car kbd+state)) cmd))
              keyboards))
  ;; All managed keyboards have their own state (kbd* is an alist of
  ;; keyboard to state). They all share LEDs and modifier states. This
  ;; allows a user to have e.g. a foot pedal for their shift key.
  (let loop ((kbd* '()) (leds 0) (mods 0))
    (let lp-kbd ((kbd* kbd*))
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
            (let ((kbd* (cons (cons new-kbd (make-keymap-state default-keymap))
                               (remp (lambda (x) (eq? (car x) new-kbd)) kbd*))))
              (keyboard-manager-number-keyboards-set! manager-kbd (length kbd*))
              (lp-kbd kbd*))]
           [('remove-keyboard . old-kbd)
            (let ((kbd* (remp (lambda (x) (eq? (car x) old-kbd)) kbd*)))
              (keyboard-manager-number-keyboards-set! manager-kbd (length kbd*))
              (lp-kbd kbd*))]
           [else
            (broadcast cmd kbd*)
            (lp-kbd kbd*)])]
        [('event . event)
         (match event
           [(kbd . kbd-event)
            (cond ((assq kbd kbd*) =>
                   (lambda (kbd+state)
                     ;; We received an event from one of the managed
                     ;; keyboards. Let the keymap process it and
                     ;; record the new state. The keymap will fill in
                     ;; an interpretation of the key event, such as
                     ;; which character the press corresponds to.
                     (let-values ([(state^ leds^ mods^ mapping)
                                   (map-key (cdr kbd+state) leds mods kbd-event)])
                       (match kbd-event
                         [#(make/break raw page usage _)
                          (let ((event^ (vector make/break raw page usage
                                                (list leds^ mods^ mapping))))
                            (put-message (keyboard-event-channel manager-kbd) event^))])
                       (set-cdr! kbd+state state^)
                       (unless (fx=? leds leds^)
                         (broadcast `(set-leds ,leds^) kbd*))
                       (loop kbd* leds^ mods^))))
                  (else
                   ;; Unknown keyboard... should not happen.
                   (put-message (keyboard-event-channel manager-kbd) event)
                   (loop kbd* leds mods)))]
           [else
            (put-message (keyboard-event-channel manager-kbd) event)
            (loop kbd* leds mods)])]))))

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
