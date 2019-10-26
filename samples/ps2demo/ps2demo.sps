;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Initialize PS/2 devices and present a non-functional UI

(import
  (rnrs (6))
  (loko match)
  (loko system fibers)
  (loko drivers keyboard)
  (loko drivers ps2 core)
  (loko drivers ps2 i8042)
  (loko drivers ps2 mouse)
  (loko drivers ps2 keyboard)
  (loko drivers pci)
  (loko system unsafe))

(define MOUSE-CHANNEL (make-channel))   ;TODO: fix something nicer
(define keyboard-manager (make-keyboard-manager))

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
                                (let ((keyboard (make-managed-keyboard keyboard-manager)))
                                  (driver·PS/2·keyboard port hotplug-channel id keyboard))))))
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
     (when (probe·pci·uhci? dev)
       (uhci-disable-legacy dev)))
   (pci-scan-bus #f)))

(display "Starting i8042 controller...\n")
(spawn-fiber
 (lambda ()
   (let ((controller (make-PS/2-controller)))
     (spawn-fiber (lambda () (manage-ps/2 controller)))
     (driver·isa·i8042 controller))))

(put-text "Move the mouse or press any key")
(let lp ((x 0) (y 0) (z 0) (key-presses 0))
  (match (perform-operation
          (choice-operation (wrap-operation (get-operation MOUSE-CHANNEL)
                                            (lambda (x) (cons 'mouse x)))
                            (wrap-operation (get-operation
                                             (keyboard-event-channel keyboard-manager))
                                            (lambda (x) (cons 'kbd x)))))
    [(mouse . #(xd yd zd buttons))
     (let ((x (fxmax 0 (fxmin 80 (fx+ x xd))))
           (y (fxmax 0 (fxmin 25 (fx+ y yd))))
           (z (+ z zd)))
       (put-text (->string (list x y z key-presses 'mouse xd yd z buttons)))
       (lp x y z key-presses))]
    [(kbd . #(make/break set scancode page usage symbolic))
     (display (list make/break (number->string scancode 16)
                    (and page (number->string page 16))
                    (and page (number->string usage 16))
                    symbolic))
     (newline)
     (put-text (->string (list x y z key-presses 'keyboard make/break
                               (number->string scancode 16))))
     (lp x y z (+ key-presses 1))]
    [(kbd . event)
     (put-text (->string (list x y z key-presses 'keyboard event)))
     (lp x y z (+ key-presses 1))]))
