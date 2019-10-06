;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; USB hub driver and hub abstraction

(library (loko drivers usb hub)
  (export
    probe·usb·hub?
    driver·usb·hub
    usb-enumerator)
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system fibers)
    (loko drivers usb core))

;;; USB hub class (external hubs)

;; bRequest codes for hubs
(define GET_STATUS     0)
(define CLEAR_FEATURE  1)
(define GET_STATE      2)
(define SET_FEATURE    3)
(define GET_DESCRIPTOR 6)
(define SET_DESCRIPTOR 7)

;; These need to talk with usb-enumerator using the same protocol as
;; root hubs.

(define (intdesc? desc)
  (eqv? (desc-bDescriptorType desc) desctype-INTERFACE))

(define (conf-hub? conf)
  (exists (lambda (desc)
            (and (intdesc? desc)
                 (eqv? #x09 (intdesc-bInterfaceClass desc))))
          conf))

(define (probe·usb·hub? dev)
  (find conf-hub? (usb-device-$configurations dev)))

(define (driver·usb·hub dev)
  (cond
    ((find conf-hub? (usb-device-$configurations dev)) =>
     (lambda (conf)
       (usb-set-configuration dev (cfgdesc-bConfigurationValue (car conf)))
       (usb-hub-driver dev conf)))
    (else #f)))

(define (usb-hub-driver dev conf)
  (display "HUB DEV: ") (write dev) (newline)
  (display "CONF: ") (write conf) (newline)
  (let lp ()
    (sleep 1)
    ;; TODO: Use an interrupt transfer
    ;; (usb-control-transfer dev #b10100011 GET_STATUS 0 )
    #;
    (lp)))

;;; USB device enumeration

;; This procedure is responsible for device enumeration on a single
;; USB controller. It talks with USB controllers (root hubs) and USB
;; hubs by using the USB hub protocol. It makes sure that only one
;; device at a time has the default address (0), assigns new devices
;; addresses and requests their device descriptors.

(define (usb-enumerator controller shutdown-cvar)
  (define root-hub (usb-controller-root-hub controller))
  (define (log/error . x)
    (for-each display x) (newline))
  (define (log/debug . x)
    (for-each display x) (newline))
  (define (fetch-hub-descriptor hub)
    (let ((ch (make-channel)))
      (put-message (usb-hub-request-channel hub)
                   (cons ch '#(GetHubDescriptor 0)))
      (match (get-message ch)
        [('ok . resp) (usb-hub-descriptor-set! hub resp)]
        [('fail . x) (error 'fetch-hub-descriptor "USB request failed" hub)])))
  (define (hub-get-port-status hub port)
    (let ((ch (make-channel)))
      (put-message (usb-hub-request-channel hub)
                   (cons ch `#(GetPortStatus ,port)))
      (match (get-message ch)
        [('ok . resp) (unpack "<SS" resp)]
        [('fail . x) (error 'hub-get-port-status "USB request failed" hub x)])))
  (define (hub-set-port-feature hub port feature)
    (let ((ch (make-channel)))
      (put-message (usb-hub-request-channel hub)
                   (cons ch `#(SetPortFeature ,feature ,port)))
      (match (get-message ch)
        [('ok . resp) resp]
        [('fail . x) (error 'hub-set-port-feature "USB request failed" hub x)])))
  (define (hub-clear-port-feature hub port feature)
    (let ((ch (make-channel)))
      (put-message (usb-hub-request-channel hub)
                   (cons ch `#(ClearPortFeature ,feature ,port)))
      (match (get-message ch)
        [('ok . resp) resp]
        [('fail . x) (error 'hub-clear-port-feature "USB request failed" hub x)])))
  (define (hubdesc-bNbrPorts desc) (bytevector-u8-ref desc 2))

  (define devices (make-vector 128 #f))
  (define (next-free-address)
    (let lp ((i 1))
      (cond ((fx=? i (vector-length devices)) #f)
            ((eqv? #f (vector-ref devices i)) i)
            (else (lp (fx+ i 1))))))

  (define (reset-port hub port)
    (define PORT_CONNECTION   0)
    (define PORT_ENABLE       1)
    (define PORT_SUSPEND      2)
    (define PORT_OVER_CURRENT 3)
    (define PORT_RESET        4)
    (define PORT_POWER        8)
    (define PORT_LOW_SPEED    9)
    (define C_PORT_CONNECTION   0)
    (define C_PORT_ENABLE       1)
    (define C_PORT_SUSPEND      2)
    (define C_PORT_OVER_CURRENT 3)
    (define C_PORT_RESET        4)

    (log/debug "Resetting port " port)
    (hub-set-port-feature hub port 'PORT_RESET) ;initiate a reset
    (log/debug "Waiting for reset")
    (let lp ((i 0))
      (sleep (/ 10 1000))
      (let-values ([(wPortStatus wPortChange) (hub-get-port-status hub port)])
        (log/debug "port status: " port " = #b" (number->string wPortStatus 2)
                   " " (number->string wPortChange 2))
        (cond ((and (fxbit-set? wPortChange C_PORT_RESET)
                    (fxbit-set? wPortStatus PORT_ENABLE))
               (log/debug "Reset successful"))
              ((eqv? i 10)
               (error 'reset-port "Reset timed out" hub port))
              (else (lp (fx+ i 1)))))))

  (define (reset-and-create-device hub port low-speed?)
    (guard (exn
            ((serious-condition? exn)
             ;; Disable the port if anything goes wrong
             (log/debug "USB: Reset and create failed: " exn)
             (hub-clear-port-feature hub port 'PORT_ENABLE)
             #f))
      (let ((address (next-free-address)))
        (unless address
          (error 'reset-and-create-device "No more free addresses" hub port))
        (reset-port hub port)
        (let ((dev0 (make-usb-device controller hub port 0 8 (if low-speed? 'low 'full) #f)))
          (usb-fetch-device-descriptor dev0 8)
          (let ((desc (usb-get-device-descriptor dev0)))
            (usb-set-address dev0 address)
            (let ((dev (make-usb-device controller hub
                                        port address
                                        (devdesc-bMaxPacketSize0 desc)
                                        (usb-device-speed dev0)
                                        desc)))
              (usb-fetch-device-descriptor dev (desc-bLength desc))
              (vector-set! devices address dev)
              dev))))))

  (fetch-hub-descriptor root-hub)
  (let loop ()
    (case (perform-operation
           (choice-operation
            (wrap-operation (sleep-operation 1) (lambda _ 'poll))
            (wrap-operation (wait-operation shutdown-cvar) (lambda _ 'stop))))
      [(poll)
       (let ((hub root-hub))
         (do ((num-ports (hubdesc-bNbrPorts (usb-hub-descriptor hub)))
              (port 0 (fx+ port 1)))
             ((fx=? port num-ports))
           (let-values ([(wPortStatus wPortChange) (hub-get-port-status hub port)])
             #;
             (log/debug "port status: " port " = #b" (number->string wPortStatus 2)
                        " " (number->string wPortChange 2))
             (when (fxbit-set? wPortChange 0)
               (let ((low-speed? (fxbit-set? wPortStatus 9)))
                 (log/debug "Probing new " (if low-speed? "low speed " "") "device on " port)
                 (cond
                   ((reset-and-create-device hub port low-speed?) =>
                    (lambda (dev)
                      ;; Send a notification that there's a new device.
                      ;; Whoever is listening will be responsible for
                      ;; fetching the additional descriptors.
                      (spawn-fiber
                       (lambda ()
                         (put-message (usb-controller-notify-channel controller)
                                      (cons 'new-device dev))))))))))))
       (loop)]
      [(stop)
       #f])))


;; Device descriptor of length 18, USB version 110, Max packet size: 8
;;  Class: #x9, SubClass: #x0, Protocol: #x0
;;  Vendor: #x409, Product: #x55AA
;;  Number of configurations: 1
;;   Configuration descriptor of length 9
;;    Number of interfaces: 1
;;    Attributes: #b11100000
;;    Max power: 0 mA
;;    Interface descriptor of length 9
;;     Interface number 0
;;     Class: #x9, SubClass: #x0, Protocol: #x0
;;     Number of endpoints: 1
;;     Endpoint descriptor of length 7
;;      Address: #x81  EP 1 IN
;;      Attributes: #b11 Interrupt
;;      Max packet size: 2
;;      Interval: 255


;; #[usb-device controller: #[usb-controller request-channel: #[channel]
;;                                           notify-channel: #[channel]]
;;              port: 1
;;              address: 2
;;              max-packet-size-0: 8
;;              speed: full
;;              $descriptor: #vu8(18 1 16 1 9 0 0 8 9 4 170 85 1 1 1 2 3 1)
;;              $configurations: ((#vu8(9 2 25 0 1 1 0 224 0)
;;                                 #vu8(9 4 0 0 1 9 0 0 0)
;;                                 #vu8(7 5 129 3 2 0 255)))
;;              $device-strings: ((1033 "QEMU" "QEMU USB Hub" "314159-0000:00:01.2-2"))]


)
