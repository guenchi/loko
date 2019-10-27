;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; Mouse abstraction

;; This library contains a basic mouse abstraction that handles the
;; channels for a mouse driver.

;; There is also a mouse manager. It can tie together multiple
;; mice (if so desired) and process the events.

(library (loko drivers mouse)
  (export
    make-mouse
    mouse-event-channel
    mouse-command-channel

    make-mouse-manager
    make-managed-mouse
    mouse-manager-remove-mouse)
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers))

(define-record-type mouse
  (fields event-channel
          command-channel)
  (protocol
   (lambda (p)
     (case-lambda
       (()
        (p (make-channel) (make-channel)))
       ((event-channel)
        (p event-channel (make-channel)))))))

(define-record-type mouse-manager
  (parent mouse)
  (fields mux-event-channel
          (mutable number-mice))
  (protocol
   (lambda (p)
     (lambda ()
       (let ((manager-mouse ((p) (make-channel) 0)))
         (spawn-fiber (lambda () (manager·mouse manager-mouse)))
         manager-mouse)))))

;; Proxy mouse driver that adds keymap lookup, tracks and updates
;; caps lock, etc., and can bind multiple mice together.
(define (manager·mouse manager-mouse)
  (define (broadcast cmd mice)
    (for-each (lambda (mouse)
                (put-message (mouse-command-channel mouse) cmd))
              mice))
  (let lp ((real* '()) (leds 0) (mods 0))
    (match (perform-operation
            (choice-operation (wrap-operation
                               (get-operation
                                (mouse-command-channel manager-mouse))
                               (lambda (x) (cons 'cmd x)))
                              (wrap-operation
                               (get-operation
                                (mouse-manager-mux-event-channel manager-mouse))
                               (lambda (x) (cons 'event x)))))
      [('cmd . cmd)
       (match cmd
         ;; This handler adds two commands for adding and removing
         ;; mice from the handler. Everything else is broadcast
         ;; to all real mice.
         [('new-mouse . new-mouse)
          (let ((real* (cons new-mouse (remq new-mouse real*))))
            (mouse-manager-number-mice-set! manager-mouse (length real*))
            (lp real* leds mods))]
         [('remove-mouse . old-mouse)
          (let ((real* (remq old-mouse real*)))
            (mouse-manager-number-mice-set! manager-mouse (length real*))
            (lp real* leds mods))]
         [else
          (broadcast cmd real*)
          (lp real* leds mods)])]
      [('event . event)
       ;; TODO: Detect click, double click and keep track of the position
       (put-message (mouse-event-channel manager-mouse) event)
       (lp real* leds mods)])))

;; Make a new mouse that is managed by the given manager. The event
;; channel of the new mouse is directed to the manager's event
;; channel, so all managed mice appear to be the same.
(define (make-managed-mouse manager)
  (let ((mouse (make-mouse (mouse-manager-mux-event-channel manager))))
    (put-message (mouse-command-channel manager) `(new-mouse . ,mouse))
    mouse))

;; Remove a mouse from a mouse manager
(define (mouse-manager-remove-mouse manager-mouse mouse)
  (put-message (mouse-command-channel manager-mouse) `(remove-mouse . ,mouse))))
