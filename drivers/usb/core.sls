;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; USB abstraction layer

(library (loko drivers usb core)
  (export
    ;; USB controllers
    make-usb-controller
    usb-controller-request-channel
    usb-controller-notify-channel
    usb-controller-root-hub

    ;; USB hubs
    make-usb-hub
    usb-hub-request-channel
    usb-hub-notify-channel
    usb-hub-descriptor
    usb-hub-descriptor-set!

    ;; USB devices
    make-usb-device
    usb-device-controller
    usb-device-port
    usb-device-address
    usb-device-max-packet-size-0
    usb-device-speed
    usb-get-device-descriptor
    usb-get-configuration-descriptors

    usb-device-$descriptor-set!
    usb-device-$descriptor
    usb-device-$configurations-set!
    usb-device-$configurations

    ;; bmRequestType constants
    devreq-GET_STATUS
    devreq-CLEAR_FEATURE
    devreq-SET_FEATURE
    devreq-SET_ADDRESS
    devreq-GET_DESCRIPTOR
    devreq-SET_DESCRIPTOR
    devreq-GET_CONFIGURATION
    devreq-SET_CONFIGURATION
    devreq-GET_INTERFACE
    devreq-SET_INTERFACE
    devreq-SYNCH_FRAME

    ;; bDescriptorType constants
    desctype-DEVICE
    desctype-CONFIGURATION
    desctype-STRING
    desctype-INTERFACE
    desctype-ENDPOINT

    ;; USB device requests
    devreq-bmRequestType
    devreq-bRequest
    devreq-wValue
    devreq-wIndex
    devreq-wLength

    request-type:device->host?
    request-type:host->device?
    endpoint:device->host?
    endpoint:host->device?

    make-devreq-get-descriptor
    make-devreq-set-address
    make-devreq-set-configuration
    make-devreq-get-interface

    ;; Common to all descriptors
    desc-bLength
    desc-bDescriptorType

    ;; Device descriptor
    devdesc-bcdUSB
    devdesc-bDeviceClass
    devdesc-bDeviceSubClass
    devdesc-bDeviceProtocol
    devdesc-bMaxPacketSize0
    devdesc-idVendor
    devdesc-idProduct
    devdesc-bcdDevice
    devdesc-iManufacturer
    devdesc-iProduct
    devdesc-iSerialNumber
    devdesc-bNumConfigurations
    devdesc-standard-length

    ;; Configuration descriptor
    cfgdesc-wTotalLength
    cfgdesc-bNumInterfaces
    cfgdesc-bConfigurationValue
    cfgdesc-iConfiguration
    cfgdesc-bmAttributes
    cfgdesc-MaxPower
    cfgdesc-standard-length

    ;; Interface descriptor
    intdesc-bInterfaceNumber
    intdesc-bAlternateSetting
    intdesc-bNumEndpoints
    intdesc-bInterfaceClass
    intdesc-bInterfaceSubClass
    intdesc-bInterfaceProtocol
    intdesc-iInterface

    ;; Endpoint descriptor
    epdesc-bEndpointAddress
    epdesc-bmAttributes
    epdesc-wMaxPacketSize
    epdesc-bInterval

    print-usb-descriptor
    string-language-descriptor->list
    string-language-descriptor->string
    split-configuration-descriptor

    ;; Perform USB transfers (synchronous calls)
    usb-control-transfer
    usb-bulk-transfer
    ;; usb-interrupt-transfer

    ;; Change a device's configuration (synchronous call)
    usb-set-configuration
    ;; Change a device's address (synchronous call)
    usb-set-address

    ;; Fetch and cache the device's string and configuration descriptors
    usb-fetch-descriptors
    usb-fetch-device-descriptor   ;involves bus access
    )
  (import
    (rnrs (6))
    (loko match)
    (loko system fibers))

(define (print . x) (for-each display x) (newline))
(define (log/debug . _) #f)

(define-syntax define-vu8-ref
  (lambda (x)
    (syntax-case x ()
      ((_ name index)
       #'(define-vu8-ref name index 1))
      ((_ name index size)
       (eqv? (syntax->datum #'size) 1)
       #'(define (name desc) (bytevector-u8-ref desc index)))
      ((_ name index size)
       (eqv? (syntax->datum #'size) 2)
       #'(define (name desc) (bytevector-u16-ref desc index (endianness little)))))))

(define devreq-GET_STATUS         0)
(define devreq-CLEAR_FEATURE      1)
(define devreq-SET_FEATURE        3)
(define devreq-SET_ADDRESS        5)
(define devreq-GET_DESCRIPTOR     6)
(define devreq-SET_DESCRIPTOR     7)
(define devreq-GET_CONFIGURATION  8)
(define devreq-SET_CONFIGURATION  9)
(define devreq-GET_INTERFACE     10)
(define devreq-SET_INTERFACE     11)
(define devreq-SYNCH_FRAME       12)

(define desctype-DEVICE        1)
(define desctype-CONFIGURATION 2)
(define desctype-STRING        3)
(define desctype-INTERFACE     4)
(define desctype-ENDPOINT      5)

(define-vu8-ref devreq-bmRequestType 0)
(define-vu8-ref devreq-bRequest      1)
(define-vu8-ref devreq-wValue        2 2)
(define-vu8-ref devreq-wIndex        4 2)
(define-vu8-ref devreq-wLength       6 2)

(define (request-type:device->host? x) (fxbit-set? x 7))

(define (request-type:host->device? x) (not (fxbit-set? x 7)))

(define (endpoint:device->host? x) (fxbit-set? x 7))

(define (endpoint:host->device? x) (not (fxbit-set? x 7)))

(define (make-devreq-get-descriptor desctype descindex index length)
  (let ((bv (make-bytevector 8)))
    (bytevector-u8-set! bv 0 #x80) ;device to host
    (bytevector-u8-set! bv 1 devreq-GET_DESCRIPTOR)
    (bytevector-u16-set! bv 2
                         (fxior (fxarithmetic-shift-left desctype 8) descindex)
                         (endianness little))
    (bytevector-u16-set! bv 4 index (endianness little))
    (bytevector-u16-set! bv 6 length (endianness little))
    bv))

(define (make-devreq-set-address address)
  (let ((bv (make-bytevector 8 0)))
    (bytevector-u8-set! bv 0 0)
    (bytevector-u8-set! bv 1 devreq-SET_ADDRESS)
    (bytevector-u16-set! bv 2 address (endianness little))
    bv))

(define (make-devreq-set-configuration configuration)
  (let ((bv (make-bytevector 8 0)))
    (bytevector-u8-set! bv 0 0)
    (bytevector-u8-set! bv 1 devreq-SET_CONFIGURATION)
    (bytevector-u16-set! bv 2 configuration (endianness little))
    bv))

(define (make-devreq-get-interface interface)
  (let ((bv (make-bytevector 8 0)))
    (bytevector-u8-set! bv 0 #x81)
    (bytevector-u8-set! bv 1 devreq-GET_INTERFACE)
    (bytevector-u16-set! bv 4 interface (endianness little))
    (bytevector-u16-set! bv 6 1 (endianness little))
    bv))

;; Descriptor fields
(define-vu8-ref desc-bLength                  0)
(define-vu8-ref desc-bDescriptorType          1)

;; Device descriptor fields
(define-vu8-ref devdesc-bcdUSB                2 2)
(define-vu8-ref devdesc-bDeviceClass          4)
(define-vu8-ref devdesc-bDeviceSubClass       5)
(define-vu8-ref devdesc-bDeviceProtocol       6)
(define-vu8-ref devdesc-bMaxPacketSize0       7)
(define-vu8-ref devdesc-idVendor              8 2)
(define-vu8-ref devdesc-idProduct            10 2)
(define-vu8-ref devdesc-bcdDevice            12 2)
(define-vu8-ref devdesc-iManufacturer        14)
(define-vu8-ref devdesc-iProduct             15)
(define-vu8-ref devdesc-iSerialNumber        16)
(define-vu8-ref devdesc-bNumConfigurations   17)
(define devdesc-standard-length 18)

;; Configuration descriptor fields
(define-vu8-ref cfgdesc-wTotalLength         2 2)
(define-vu8-ref cfgdesc-bNumInterfaces       4)
(define-vu8-ref cfgdesc-bConfigurationValue  5)
(define-vu8-ref cfgdesc-iConfiguration       6) ;barely used
(define-vu8-ref cfgdesc-bmAttributes         7)
(define-vu8-ref cfgdesc-MaxPower             8)
(define cfgdesc-standard-length 9)

;; Interface descriptor fields
(define-vu8-ref intdesc-bInterfaceNumber   2)
(define-vu8-ref intdesc-bAlternateSetting  3)
(define-vu8-ref intdesc-bNumEndpoints      4)
(define-vu8-ref intdesc-bInterfaceClass    5)
(define-vu8-ref intdesc-bInterfaceSubClass 6)
(define-vu8-ref intdesc-bInterfaceProtocol 7)
(define-vu8-ref intdesc-iInterface         8)

;; Endpoint descriptor fields
(define-vu8-ref epdesc-bEndpointAddress 2)
(define-vu8-ref epdesc-bmAttributes     3)
(define-vu8-ref epdesc-wMaxPacketSize   4 2)
(define-vu8-ref epdesc-bInterval        6)

(define (print-usb-descriptor desc)
  (define (fmt x) (number->string x 16))
  (define (print . x) (for-each display x) (newline))
  (let ((type (desc-bDescriptorType desc)))
    (cond
      ((eqv? type desctype-DEVICE)
       (print "Device descriptor of length " (desc-bLength desc)
            ", USB version " (fmt (devdesc-bcdUSB desc))
            ", Max packet size: " (devdesc-bMaxPacketSize0 desc))
       (when (fx>=? (bytevector-length desc) devdesc-standard-length)
         (print " Class: #x" (fmt (devdesc-bDeviceClass desc))
              ", SubClass: #x" (fmt (devdesc-bDeviceSubClass desc))
              ", Protocol: #x" (fmt (devdesc-bDeviceProtocol desc)))
         (print " Vendor: #x" (fmt (devdesc-idVendor desc))
              ", Product: #x" (fmt (devdesc-idProduct desc)))
         (print " Number of configurations: " (devdesc-bNumConfigurations desc))))
      ((eqv? type desctype-STRING)
       (print "String descriptor of length " (desc-bLength desc)))
      ((eqv? type desctype-CONFIGURATION)
       (print "  Configuration descriptor of length " (desc-bLength desc))
       (print "   Number of interfaces: " (cfgdesc-bNumInterfaces desc))
       (print "   Attributes: #b" (number->string (cfgdesc-bmAttributes desc) 2))
       (print "   Max power: " (fx* 2 (cfgdesc-MaxPower desc)) " mA"))
      ((eqv? type desctype-INTERFACE)
       (print "   Interface descriptor of length " (desc-bLength desc))
       (print "    Interface number " (intdesc-bInterfaceNumber desc))
       (print "    Class: #x" (fmt (intdesc-bInterfaceClass desc))
            ", SubClass: #x" (fmt (intdesc-bInterfaceSubClass desc))
            ", Protocol: #x" (fmt (intdesc-bInterfaceProtocol desc)))
       (print "    Number of endpoints: " (intdesc-bNumEndpoints desc)))
      ((eqv? type desctype-ENDPOINT)
       (print "    Endpoint descriptor of length " (desc-bLength desc))
       (let ((addr (epdesc-bEndpointAddress desc)))
         (print "     Address: #x" (fmt addr) "  EP " (fxbit-field addr 0 4) " "
              (if (fxbit-set? addr 7) "IN" "OUT")))
       (print "     Attributes: #b" (number->string (epdesc-bmAttributes desc) 2) " "
            (case (fxbit-field (epdesc-bmAttributes desc) 0 2)
              ((#b00) "Control")
              ((#b01) "Isochronous")
              ((#b10) "Bulk")
              (else "Interrupt")))
       (print "     Max packet size: " (epdesc-wMaxPacketSize desc))
       (print "     Interval: " (epdesc-bInterval desc)))
      (else
       (print "Unknown device descriptor type: " desc)))))

(define (string-language-descriptor->list desc)
  (assert (eqv? (desc-bDescriptorType desc) desctype-STRING))
  (do ((len (fxdiv (fx- (fxmin (bytevector-length desc) (desc-bLength desc)) 2) 2))
       (i 0 (fx+ i 1))
       (lang* '() (cons (bytevector-u16-ref desc (fx+ i 2) (endianness little)) lang*)))
      ((fx=? i len) (reverse lang*))))

(define (string-language-descriptor->string desc)
  (assert (eqv? (desc-bDescriptorType desc) desctype-STRING))
  (utf16->string (call-with-bytevector-output-port
                   (lambda (p)
                     (put-bytevector p desc 2 (fx- (fxmin (bytevector-length desc)
                                                          (desc-bLength desc))
                                                   2))))
                 (endianness little)))

(define (split-configuration-descriptor desc)
  (assert (eqv? (desc-bDescriptorType desc) desctype-CONFIGURATION))
  (let ((p (open-bytevector-input-port desc)))
    (let lp ()
      (if (port-eof? p)
          '()
          (let ((cfgdesc (get-bytevector-n p (lookahead-u8 p))))
            (cons cfgdesc (lp)))))))

;; Each USB controller (HCI) gets one of these
(define-record-type usb-controller
  (fields
   ;; Requests to the controller.
   request-channel
   ;; Notifications from the controller.
   notify-channel
   ;; The "root hub", which is a fake hub that handles the ports
   ;; directly on the controller.
   root-hub)
  (protocol
   (lambda (p)
     (lambda ()
       (p (make-channel) (make-channel)
          (make-usb-hub))))))

;; Each device on the USB gets one of these
(define-record-type usb-device
  (fields controller hub
          port
          address
          max-packet-size-0          ;max packet size for endpoint 0
          speed                      ;low, full, high, super, super+
          (mutable $descriptor)
          ;; ((cfgdesc intdesc ... epdesc ...) ...)
          (mutable $configurations)
          ;; ((lang-id manufacturer product serial-no) ...)
          (mutable $device-strings))
  (protocol
   (lambda (p)
     (lambda (controller hub port address max-packet-size-0 speed desc)
       (p controller hub port address max-packet-size-0 speed desc #f #f)))))

;; Each USB hub gets one of these and the controller also gets one
(define-record-type usb-hub
  (fields
   ;; Control requests to the hub
   request-channel
   ;; Status change notifications from the hub
   notify-channel
   ;; Hub descriptor
   (mutable descriptor))
  (protocol
   (lambda (p)
     (lambda ()
       (p (make-channel) (make-channel) #f)))))

;; Gets the USB device descriptor without generating bus traffic.
(define usb-get-device-descriptor usb-device-$descriptor)

;; Same as above for the configuration descriptors. The
;; usb-fetch-descriptors procedure must be called first.
(define usb-get-configuration-descriptors usb-device-$configurations)

;;; Requests

(define EndPt0 0)

(define (perform-devreq/response dev endpoint devreq timeout)
  (let ((ch (make-channel)))
    (put-message (usb-controller-request-channel (usb-device-controller dev))
                 (list 'perform-devreq/response ch dev endpoint devreq timeout))
    (let ((resp (get-message ch)))
      (values (car resp) (cdr resp)))))

;; Synchronous procedure to perform a control transfer on the device.
;; The request-type indicates the direction and whether the *data*
;; bytevector is read or written.
(define (usb-control-transfer dev request-type bRequest wValue wIndex data timeout)
  (define (make-devreq request-type bRequest wValue wIndex)
    (let ((bv (make-bytevector 8)))
      (bytevector-u8-set! bv 0 request-type)
      (bytevector-u8-set! bv 1 bRequest)
      (bytevector-u16-set! bv 2 wValue (endianness little))
      (bytevector-u16-set! bv 4 wIndex (endianness little))
      (bytevector-u16-set! bv 6 (bytevector-length data) (endianness little))
      bv))
  (assert (fx<? (bytevector-length data) 1280))
  (let-values ([(status resp)
                (if (request-type:host->device? request-type)
                    (error 'usb-control-transfer "TODO: Host to device")
                    (let ((req (make-devreq request-type bRequest wValue wIndex)))
                      (perform-devreq/response dev EndPt0 req timeout)))])
    (case status
      ((ok)
       ;; XXX: This data is copied twice
       (bytevector-copy! resp 0 data 0 (fxmin (bytevector-length resp)
                                              (bytevector-length data))))
      (else (error 'usb-control-transfer "Transfer error"
                   dev request-type bRequest wValue wIndex data timeout)))))

;; Synchronous procedure to perform a bulk transfer.
(define (usb-bulk-transfer dev endpoint data timeout)
  (define (perform-bulk dev endpoint data timeout)
    (let ((ch (make-channel)))
      (put-message (usb-controller-request-channel (usb-device-controller dev))
                   (list 'perform-bulk ch dev endpoint data timeout))
      (let ((resp (get-message ch)))
        (values (car resp) (cdr resp)))))
  (assert (fx<? (bytevector-length data) 1280))
  (let-values ([(status resp) (perform-bulk dev endpoint data timeout)])
    (case status
      ((ok) resp)                     ;actual length
      (else (error 'usb-bulk-transfer "Transfer error" dev endpoint timeout)))))

;; Synchronous procedure to select a device configuration. Only one
;; device configuration can be active at any time.
(define (usb-set-configuration dev configuration)
  (define (perform-set-configuration dev configuration)
    (let ((ch (make-channel)))
      (put-message (usb-controller-request-channel (usb-device-controller dev))
                   (list 'set-configuration ch dev configuration))
      (let ((resp (get-message ch)))
        (values (car resp) (cdr resp)))))
  (let-values ([(status resp) (perform-set-configuration dev configuration)])
    (case status
      ((ok) resp)
      (else (error 'usb-set-configuration "Failed" dev configuration)))))

(define (usb-set-address dev address)
  (let ((ch (make-channel)))
    (put-message (usb-controller-request-channel (usb-device-controller dev))
                 (list 'set-address ch dev address))
    (match (get-message ch)
      [('ok . resp)
       resp]
      [('fail . resp)
       (error 'usb-set-address "Failed to set address" dev address resp)])))

;;;

(define (usb-fetch-descriptors dev)
  (let ((desc (usb-get-device-descriptor dev))
        (strdesc (get-string-descriptor dev 0 0)))

    ;; Fetch string descriptors
    (let ((strings
           (map
            (lambda (langid)
              (let ((manufacturer  (get-string-descriptor* dev langid (devdesc-iManufacturer desc)))
                    (product       (get-string-descriptor* dev langid (devdesc-iProduct desc)))
                    (serial-number (get-string-descriptor* dev langid (devdesc-iSerialNumber desc))))
                (list langid manufacturer product serial-number)))
            (string-language-descriptor->list strdesc))))
      (usb-device-$device-strings-set! dev strings))

    ;; Fetch configuration descriptors
    (do ((conf 0 (fx+ conf 1))
         (cfgdesc** '()
                    (cons (split-configuration-descriptor
                           (get-configuration-descriptor dev conf))
                          cfgdesc**)))
        ((fx=? conf (devdesc-bNumConfigurations
                     (usb-get-device-descriptor dev)))
         (usb-device-$configurations-set! dev (reverse cfgdesc**))))))

(define (usb-fetch-device-descriptor dev total-length)
  (let ((req0 (make-devreq-get-descriptor desctype-DEVICE 0 0 total-length)))
    (let-values ([(status resp) (perform-devreq/response dev EndPt0 req0 100)])
      (unless (eqv? status 'ok)
        (error 'get-device-descriptor "Could not retrieve device descriptor"
               dev total-length status))
      (usb-device-$descriptor-set! dev resp))))

(define (get-full-descriptor dev index descindex desctype)
  (let ((req0 (make-devreq-get-descriptor desctype descindex index 8)))
    (let-values ([(status0 resp0) (perform-devreq/response dev EndPt0 req0 100)])
      (case status0
        ((ok)
         (let ((full-length (if (eqv? desctype desctype-CONFIGURATION)
                                (cfgdesc-wTotalLength resp0)
                                (desc-bLength resp0))))
           (if (fx>? full-length 8)
               (let ((req1 (make-devreq-get-descriptor desctype descindex index full-length)))
                 (let-values ([(status1 resp1) (perform-devreq/response dev EndPt0 req1 100)])
                   (case status1
                     ((ok)
                      resp1)
                     (else
                      (print "Could not retrieve full descriptor: " status1)
                      resp1))))
               ;; XXX: subbytevector?
               resp0)))
        (else
         (print "Could not retrieve descriptor: " status0)
         resp0)))))

(define (get-string-descriptor dev langid descindex)
  (get-full-descriptor dev langid descindex desctype-STRING))

(define (get-string-descriptor* dev langid descindex)
  (cond ((eqv? descindex 0) #f)
        ((get-string-descriptor dev langid descindex)
         => string-language-descriptor->string)
        (else #f)))

(define (get-configuration-descriptor dev descindex)
  (get-full-descriptor dev 0 descindex desctype-CONFIGURATION))

)
