;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Initialize PS/2 devices and present a non-functional UI

(import
  (rnrs (6))
  (loko match)
  (loko system fibers)
  (loko drivers ps2 core)
  (loko drivers ps2 i8042)
  (loko drivers ps2 mouse)
  (loko drivers ps2 keyboard)
  #;(loko drivers pci)
  (loko system unsafe))

(define MOUSE-CHANNEL (make-channel))   ;TODO: fix something nicer
(define KEYBOARD-CHANNEL (make-channel))

(define (manage-ps/2 controller)
  (define hotplug-channel (make-channel))
  (let lp ()
    (match (get-message (PS/2-controller-notify-channel controller))
      [('new-port . port)
       ;; At this point we should probe the port, find a driver for
       ;; the device and spawn a fiber to handle it.
       (let ((reset-id
              (guard (exn ((error? exn) #f))
                (PS/2-command port PS/2-RESET)
                (let ((result (PS/2-read port 4000)))
                  (PS/2-read port 1000)))))
         (cond
           ((and (eqv? reset-id 0) (probe·PS/2·mouse port))
            => (lambda (id)
                 (spawn-fiber (lambda ()
                                (driver·PS/2·mouse port hotplug-channel id MOUSE-CHANNEL)))))
           ((probe·PS/2·keyboard port)
            => (lambda (id)
                 (spawn-fiber (lambda ()
                                (driver·PS/2·keyboard port hotplug-channel id KEYBOARD-CHANNEL)))))
           (else
            ;; Probing failed, we have no driver. Start the hotplug
            ;; driver, that should tell us when a new device has been
            ;; attached.
            (spawn-fiber (lambda ()
                           (driver·PS/2·hotplug port hotplug-channel))))))])
    (lp)))

(define (put-text str)
  (define mem-base #xb8000)
  (do ((i 0 (fx+ i 1)))
      ((fx=? i 80))
    (put-mem-u8 (fx+ mem-base (fx+ i i))
                (if (fx<? i (string-length str))
                    (fxand (char->integer (string-ref str i)) #xff)
                    (char->integer #\space)))))

(define (->string x)
  (call-with-string-output-port
    (lambda (p)
      (display x p))))

(display "Starting i8042 controller...\n")

#;
(begin
  (display "Disabling USB legacy support...\n")
  (for-each
   (lambda (dev)
     (define (probe·pci·uhci? dev)
       (and (eqv? (pcidev-base-class dev) #x0c)
            (eqv? (pcidev-sub-class dev) #x03)
            (eqv? (pcidev-interface dev) #x00)))
     (define (uhci-disable-legacy dev)
       ;; Disable keyboard and mouse legacy support
       (pci-put-u16 dev #xC0 #x0000))
     (when  (probe·pci·uhci? dev)
       (uhci-disable-legacy dev)))
   (pci-scan-bus #f)))

(spawn-fiber
 (lambda ()
   (let ((controller (make-PS/2-controller)))
     (spawn-fiber (lambda () (manage-ps/2 controller)))
     (driver·isa·i8042 controller))))

(put-text "Move the mouse or press any key")
(let lp ((x 0) (y 0) (z 0) (key-presses 0))
  (match (perform-operation
          (choice-operation (get-operation MOUSE-CHANNEL)
                            (get-operation KEYBOARD-CHANNEL)))
    [#(xd yd zd buttons)
     (let ((x (fxmax 0 (fxmin 80 (fx+ x xd))))
           (y (fxmax 0 (fxmin 25 (fx+ y yd))))
           (z (+ z zd)))
       (put-text (->string (list x y z key-presses 'mouse xd yd z buttons)))
       (lp x y z key-presses))]
    [#(set is-press scancode)
     ;; TODO: a translation to USB HID codes would be excellent before
     ;; going further with this
     (put-text (->string (list x y z key-presses 'keyboard set is-press scancode)))
     (lp x y z (+ key-presses 1))]))
