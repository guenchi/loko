;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; PCI bus enumeration

(import
  (rnrs (6))
  (loko drivers pci))

(define (print . x)
  (for-each display x)
  (newline))

(define (hexdigit? c)
  (or (char<=? #\0 c #\9)
      (char<=? #\a c #\f)
      (char<=? #\A c #\F)))

;; Convention: foo% denotes a hashtable
(define-record-type pciid-db
  (fields vendors% classes%))

(define-record-type pciid-vendor
  (fields id name device%))

(define-record-type pciid-device
  (fields id name subsystem%))

(define-record-type pciid-subsystem
  (fields vendor-id device-id name))

(define-record-type pciid-class
  (fields id name subclass%))

(define-record-type pciid-subclass
  (fields id name if%))

(define-record-type pciid-if
  (fields id name))

;; Parse the data file from the PCI ID Project:
;; https://pci-ids.ucw.cz/
(define (parse-pci-ids input-port)
  (define (write . _) #f)
  (define (newline) #f)
  (define vendors (make-eqv-hashtable))
  (define classes (make-eqv-hashtable))
  (let reset ()
    (let lp-top ((top #f))
      (let lp-sub ((sub #f))
        (let lp ()
          (let ((line (get-line input-port)))
            (cond
              ((eof-object? line))
              ((fx<? (string-length line) 2)
               (lp))
              (else
               (let ((c0 (string-ref line 0)) (c1 (string-ref line 1)))
                 (cond
                   ((eqv? c0 #\#)
                    (lp))

                   ;; Top level of a device
                   ((and (hexdigit? c0) (hexdigit? c1))
                    (let ((id (string->number (substring line 0 4) 16))
                          (name (substring line 6 (string-length line))))
                      (let ((vendor (make-pciid-vendor id name (make-eqv-hashtable))))
                        (hashtable-set! vendors id vendor)
                        (lp-top vendor))))

                   ;; Top level of a class
                   ((and (eqv? c0 #\C) (eqv? c1 #\space))
                    (let ((id (string->number (substring line 2 4) 16))
                          (name (substring line 6 (string-length line))))
                      (let ((class (make-pciid-class id name (make-eqv-hashtable))))
                        (hashtable-set! classes id class)
                        (lp-top class))))

                   ;; Second level of a device or a class
                   ((and (eqv? c0 #\tab) (not (eqv? c1 #\tab)))
                    (cond
                      ((pciid-vendor? top)
                       (let ((id (string->number (substring line 1 5) 16))
                             (name (substring line 7 (string-length line))))
                         (let ((device (make-pciid-device id name
                                                          (make-hashtable equal-hash equal?))))
                           (hashtable-set! (pciid-vendor-device% top) id device)
                           (lp-sub device))))

                      ((pciid-class? top)
                       (let ((id (string->number (substring line 1 3) 16))
                             (name (substring line 5 (string-length line))))
                         (let ((sub (make-pciid-subclass id name (make-eqv-hashtable))))
                           (hashtable-set! (pciid-class-subclass% top) id sub)
                           (lp-sub sub))))
                      (else
                       (lp))))

                   ;; Deepest level
                   ((and (eqv? c0 #\tab) (eqv? c1 #\tab))
                    (cond
                      ((pciid-device? sub)
                       (let ((id (string->number (substring line 2 6) 16))
                             (dev (string->number (substring line 7 11) 16))
                             (name (substring line 13 (string-length line))))
                         (let ((subsystem (make-pciid-subsystem id dev name)))
                           (hashtable-set! (pciid-device-subsystem% sub)
                                           (cons id dev) subsystem)
                           (lp-sub sub))))

                      ((pciid-subclass? sub)
                       (let ((id (string->number (substring line 2 4) 16))
                             (name (substring line 6 (string-length line))))
                         (let ((if (make-pciid-if id name)))
                           (hashtable-set! (pciid-subclass-if% sub) id if)
                           (lp-sub sub))))

                      (else
                       (write (list 'unrecognized-tt top sub line))
                       (lp))))
                   (else
                    ;; Unrecognized line
                    (write (list 'unrecognized top sub line))
                    (newline)
                    (reset)))))))))))
  (make-pciid-db vendors classes))

;; Given a pcidev from (loko drivers pci) and a pciid-db, print some
;; useful information.
(define (print-pcidev dev db)
  (define (hex x) (number->string x 16))
  (define (lookup-class base sub int)
    (cond
      ((hashtable-ref (pciid-db-classes% db) base #f) =>
       (lambda (basedata)
         (cond
           ((hashtable-ref (pciid-class-subclass% basedata) sub #f) =>
            (lambda (subdata)
              (cond
                ((hashtable-ref (pciid-subclass-if% subdata) int #f) =>
                 (lambda (ifdata)
                   (values (pciid-class-name basedata)
                           (pciid-subclass-name subdata)
                           (pciid-if-name ifdata))))
                (else (values (pciid-class-name basedata)
                              (pciid-subclass-name subdata)
                              #f)))))
           (else (values (pciid-class-name basedata) sub int)))))
      (else (values base #f #f))))
  (define (lookup-names db vendor device subvendor subdevice)
    (cond
      ((hashtable-ref (pciid-db-vendors% db) vendor #f) =>
       (lambda (vendordata)
         (cond ((hashtable-ref (pciid-vendor-device% vendordata) device #f) =>
                (lambda (devicedata)
                  (cond ((hashtable-ref (pciid-device-subsystem% devicedata)
                                        (cons subvendor subdevice) #f)
                         =>
                         (lambda (subsystemdata)
                           (values (pciid-vendor-name vendordata)
                                   (pciid-device-name devicedata)
                                   (pciid-subsystem-name subsystemdata))))
                        (else
                         (values (pciid-vendor-name vendordata)
                                 (pciid-device-name devicedata)
                                 (string-append (hex subvendor) ":" (hex subdevice)))))))
               (else (values (pciid-vendor-name vendordata)
                             #f
                             (string-append (hex subvendor) ":" (hex subdevice)))))))
      (else (values #f #f (string-append (hex subvendor) ":" (hex subdevice))))))

  (let*-values ([(subvendor subdevice)
                 (if (eqv? (pcidev-header-layout dev) #x00)
                     (values (pci-get-u16 dev PCI-CFG-00-SUBSYSTEM-VENDOR-ID)
                             (pci-get-u16 dev PCI-CFG-00-SUBSYSTEM-ID))
                     (values #f #f))]
                [(vname dname sname)
                 (lookup-names db (pcidev-vendor-id dev) (pcidev-device-id dev)
                               subvendor subdevice)]
                [(basename subname ifname)
                 (lookup-class (pcidev-base-class dev)
                               (pcidev-sub-class dev)
                               (pcidev-interface dev))])
    (newline)
    (print (pcidev-bus dev) #\: (pcidev-dev dev) #\. (pcidev-func dev) #\tab
           (hex (pcidev-vendor-id dev)) #\:
           (hex (pcidev-device-id dev))
           "  "
           (or subname basename))
    (when vname
      (print " Vendor: " vname))
    (when dname
      (print " Device: " dname))
    (print " Subsystem: " sname)
    (unless (and (not ifname) (eqv? (pcidev-interface dev) 0))
      (print " Programming interface: " ifname)))
  (when (eqv? #x00 (pcidev-header-layout dev))
    (let ((irq (pci-get-u8 dev PCI-CFG-00-INTERRUPT-LINE)))
      (unless (eqv? irq 0)
        (print " IRQ " irq))))
  (do ((BARs (pcidev-BARs dev))
       (i 0 (fx+ i 1)))
      ((fx=? i (vector-length BARs)))
    (let ((BAR (vector-ref BARs i)))
      (when BAR
        (print " BAR" i " " (if (pcibar-i/o? BAR) 'i/o #\m)
               (if (pcibar-i/o? BAR) "" (pcibar-mem-type BAR)) " #x"
               (hex (pcibar-base BAR))
               " #x+" (hex (pcibar-size BAR))))))
  (cond ((pcidev-ROM-size dev) =>
         (lambda (size)
           (print " ROM size #x" (hex size)))))
  )

(display "Scanning the PCI bus...\n")

(let ((devs (pci-scan-bus #f))
      (db (call-with-input-file "/boot/pci.ids"
            parse-pci-ids)))
  (for-each (lambda (dev)
              (print-pcidev dev db))
            devs))

(display "End of output\n\n")
