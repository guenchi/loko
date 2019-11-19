;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; List information about disks

(import
  (rnrs (6))
  (loko match)
  (loko system fibers)
  (loko drivers ata atapi)
  (loko drivers ata core)
  (loko drivers ata drive)
  (loko drivers ata ide)
  (loko drivers ata identify)
  (loko drivers pci)
  (loko drivers storage)
  (fs fatfs))

(define devs (pci-scan-bus #f))

(define logch (make-channel))

(spawn-fiber
 (lambda ()
   (let lp ()
     (let ((msg (get-message logch)))
       (display msg)
       (newline))
     (lp))))

(define (log/info msg)
  (put-message logch msg))

(define (log-device-info identify-block)
  (let-values ([(logical physical) (ata-identify:ata-sector-size identify-block)])
    (log/info (list 'model: (ata-identify:model-number identify-block)
                    'serial: (ata-identify:serial-number identify-block)
                    'firmware: (ata-identify:firmware-revision identify-block)
                    'commands: (ata-identify:supported-command-set identify-block)
                    'major-version: (ata-identify:major-revision identify-block)
                    (cond ((ata-identify:ata-device? identify-block) 'ata:)
                          ((ata-identify:atapi-device? identify-block) 'atapi:)
                          (else 'unknown-identify:))
                    (if (ata-identify:ata-device? identify-block)
                        (list 'logical-sector-size: logical
                              'physical-sector-size: physical)
                        (list))
                    'raw: identify-block))))

(define storage-manager-ch (make-channel))

(spawn-fiber
 (lambda ()
   (let lp ()
     (match (get-message storage-manager-ch)
       [('new-storage storage)
        (log/info (list 'storage: storage))
        (spawn-fiber
         (lambda ()
           ;; TODO: Just to see that things work this is reading
           ;; sector 0, but it would be neat to scan the partition
           ;; table and show info about the first FAT file system
           (let ((resp-ch (make-channel)))
             (put-message (storage-device-request-channel storage)
                          (list resp-ch 'read 0 1))
             (log/info (list 'sector0: (get-message resp-ch))))))])
     (lp))))

(define scsi-manager-ch (make-channel))
(spawn-fiber
 (lambda ()
   (let lp ()
     (match (get-message scsi-manager-ch)
       [('new-device . ch)
        ;; TODO: probe the channel and start an appropriate driver
        (spawn-fiber
         (lambda ()
           #f))])
     (lp))))

(define (ata-manager controller)
  (let lp ()
    (match (get-message (ata-controller-notify-channel controller))
      [('new-device . ch)
       ;; Probe the device and start a driver
       (spawn-fiber
        (lambda ()
          (match (probe·ata ch)
            [('ata . identify-block)
             (log/info "ATA drive")
             (log-device-info identify-block)
             (let* ((atadev (make-ata-device controller ch identify-block))
                    (storage (make-storage-device
                              "ATA drive"
                              (let-values ([(logical _)
                                            (ata-identify:ata-sector-size identify-block)])
                                logical))))
               (put-message storage-manager-ch (list 'new-storage storage))
               (driver·ata·drive atadev
                                 storage))]

            [('atapi . identify-block)
             (log/info "ATAPI device")
             (log-device-info identify-block)
             ;; TODO: Make a new SCSI logical unit and set the ATAPI
             ;; device as the request channel
             (let ((scsi-req-ch (make-channel)))
               (put-message scsi-manager-ch (cons 'new-device 'TODO))
               ;; XXX: The ATAPI driver currently prints some debug
               ;; stuff and there's a concurrency bug around ports...
               (sleep 5)
               (driver·ata·atapi (make-ata-device controller ch identify-block)
                                 scsi-req-ch))]

            ['no-device
             (log/info "No device")]

            [x
             (log/info (list "No driver:" x))])))
       (lp)])))

(log/info "")
(log/info "Scanning for IDE devices...")
(for-each (lambda (dev)
            (when (probe·pci·ide? dev)
              (log/info dev)
              (let ((controller (make-ata-controller)))
                (spawn-fiber (lambda () (ata-manager controller)))
                (spawn-fiber (lambda () (driver·pci·ide dev controller))))))
          devs)

(let lp ()
  (sleep 60)
  (lp))
