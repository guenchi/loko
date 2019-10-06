;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Initialize USB controllers and list devices as they appear

(import
  (rnrs (6))
  (loko match)
  (loko system fibers)
  (loko drivers pci)
  (loko drivers usb core)
  (loko drivers usb uhci)
  (loko drivers usb hub))

(define (manage-usb-hci controller)
  (let lp ()
    (match (get-message (usb-controller-notify-channel controller))
      [('new-device . usbdev)
       ;; (display "\nlsusb: Fetching descriptors\n")
       ;; Fetch all descriptors from the hardware and cache them
       ;; on the usb-device record
       (usb-fetch-descriptors usbdev)
       ;; At this point we should find a driver for the device and
       ;; spawn a fiber to handle it.
       (print-usb-descriptor (usb-get-device-descriptor usbdev))
       (for-each (lambda (cfgdesc*)
                   (for-each print-usb-descriptor cfgdesc*))
                 (usb-device-$configurations usbdev))
       (write usbdev)
       (newline)
       (cond
         ((probe·usb·hub? usbdev)
          (spawn-fiber
           (lambda ()
             (driver·usb·hub usbdev)))))])
    (lp)))

(display "Scanning the PCI bus\n")
(for-each
 (lambda (dev)
   (when (probe·pci·uhci? dev)
     (display "Found UHCI controller: ")
     (write dev)
     (newline)
     (spawn-fiber
      (lambda ()
        (let ((controller (make-usb-controller)))
          (spawn-fiber (lambda () (manage-usb-hci controller)))
          (driver·pci·uhci dev controller))))))
 (pci-scan-bus #f))

(display "Waiting for USB devices\n")

;; Keep the process alive
(let lp ()
  (sleep 60)
  (lp))
