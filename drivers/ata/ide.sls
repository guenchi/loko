;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; IDE interface driver (parallel ATA transport)

(library (loko drivers ata ide)
  (export
    probe·pci·ide?
    driver·pci·ide)
  (import
    (rnrs (6))
    (struct pack)
    (loko match)
    (loko system fibers)
    (loko system unsafe)
    (only (loko system $host)
          enable-irq acknowledge-irq wait-irq-operation
          dma-allocate dma-free)
    (loko drivers pci)
    (loko drivers ata core))

(define DEBUG #t)

(define (probe·pci·ide? dev)
  (and (eqv? (pcidev-base-class dev) #x01)
       (eqv? (pcidev-sub-class dev) #x01)))

;; The IDE controller glue for PCI IDE. Each PCI IDE device has two
;; channels, each of which needs a driver.
(define (driver·pci·ide dev controller)
  ;; PCI IDE primary channel: BAR0, BAR1, BAR4
  (define compat-command-0 #x1f0)
  (define compat-control-0 #x3f6)
  (define compat-irq-0 14)
  ;; PCI IDE secondary channel: BAR2, BAR3, BAR4+8
  (define compat-command-1 #x170)
  (define compat-control-1 #x376)
  (define compat-irq-1 15)
  (define (maybe-base BARs i)
    (cond ((vector-ref BARs i) =>
           (lambda (BAR)
             (let ((base (pcibar-base BAR)))
               (if (eqv? base 0)
                   #f
                   base))))
          (else #f)))
  (let* ((interface (pcidev-interface dev))
         (BARs (pcidev-BARs dev))
         (irq (if (memv (pcidev-irq dev) '(0 255)) #f (pcidev-irq dev)))
         ;; True if the channel is in compat mode
         (compat-0 (not (fxbit-set? interface 0)))
         (compat-1 (not (fxbit-set? interface 2))))
    (let ((channel-0 (make-channel))
          (channel-1 (make-channel))
          (channel-2 (make-channel))
          (channel-3 (make-channel)))
      (let ((primary (list (or (maybe-base BARs 0) (and compat-0 compat-command-0))
                           (or (maybe-base BARs 1) (and compat-0 compat-control-0))
                           (maybe-base BARs 4)
                           (or irq (and compat-0 compat-irq-0))
                           channel-0 channel-1))
            (secondary (list (or (maybe-base BARs 2) (and compat-1 compat-command-1))
                             (or (maybe-base BARs 3) (and compat-1 compat-control-1))
                             (cond ((maybe-base BARs 4) =>
                                    (lambda (base) (fx+ 8 base)))
                                   (else #f))
                             (or irq (and compat-1 compat-irq-1))
                             channel-2 channel-3)))
        (pci-ide-configure dev)
        ;; Start drivers for the channels. Tell whoever manages the
        ;; controller that these channels exist. The receiver will
        ;; then be responsible for starting drivers for the ATA
        ;; devices.
        (when (for-all values primary)
          (spawn-fiber (lambda () (apply driver·ide primary)))
          (put-message (ata-controller-notify-channel controller)
                       (cons 'new-device channel-0))
          (put-message (ata-controller-notify-channel controller)
                       (cons 'new-device channel-1)))
        (when (for-all values secondary)
          (spawn-fiber (lambda () (apply driver·ide secondary)))
          (put-message (ata-controller-notify-channel controller)
                       (cons 'new-device channel-2))
          (put-message (ata-controller-notify-channel controller)
                       (cons 'new-device channel-3)))))))

(define (pci-ide-configure dev)
  (pci-put-u16 dev PCI-CFG-COMMAND
               (fxior (fxand (pci-get-u16 dev PCI-CFG-COMMAND)
                             (fxnot PCI-CMD-INTERRUPT-DISABLE))
                      PCI-CMD-I/O-SPACE
                      #;PCI-CMD-MEM-SPACE
                      PCI-CMD-BUS-MASTER)))

;;;

;; Driver for a single IDE channel with up to two devices.
(define (driver·ide reg-command-blk reg-control-blk reg-bus-master-blk irq
                    channel-0 channel-1)
  (define has-bmide?
    (and reg-bus-master-blk
         (eqv? 0 (fxand bmide-status-simplex-only (get-i/o-u8 reg-bmide-status)))
         #t))

  (define command-timeout 10)

;;; Command block registers

  (define reg-cmd-data     (fx+ reg-command-blk 0))
  (define reg-cmd-error    (fx+ reg-command-blk 1)) ;read-only
  (define reg-cmd-feature  (fx+ reg-command-blk 1)) ;write-only
  (define reg-cmd-count    (fx+ reg-command-blk 2))
  (define reg-cmd-lba-low  (fx+ reg-command-blk 3)) ;legacy: sector number
  (define reg-cmd-lba-mid  (fx+ reg-command-blk 4)) ;legacy: cylinder low
  (define reg-cmd-lba-high (fx+ reg-command-blk 5)) ;legacy: cylinder high
  (define reg-cmd-device   (fx+ reg-command-blk 6)) ;legacy: head number
  (define reg-cmd-status   (fx+ reg-command-blk 7)) ;read-only
  (define reg-cmd-command  (fx+ reg-command-blk 7)) ;write-only

  (define (status-ready? x)
    (eqv? 0 (fxand x (fxior ata-status-BSY ata-status-DRQ))))

;;; Control block registers

  (define reg-ctl-alternate-status (fx+ reg-control-blk 0)) ;read-only
  (define reg-ctl-device-control   (fx+ reg-control-blk 0)) ;write-only

  (define alternate-status-BSY  #b10000000)
  (define alternate-status-DRDY #b01000000)
  (define alternate-status-DWF  #b00100000)
  (define alternate-status-DSC  #b00010000)
  (define alternate-status-DRQ  #b00001000)
  (define alternate-status-CORR #b00000100)
  (define alternate-status-IDX  #b00000010)
  (define alternate-status-ERR  #b00000001)

  (define device-control-HOB   #b10000000)
  (define device-control-SRST  #b00000100)   ;software reset
  (define device-control-nIEN  #b00000010)   ;disable interrupts

;;; Bus master IDE

  (define reg-bmide-command        (and reg-bus-master-blk (fx+ reg-bus-master-blk 0)))
  (define reg-bmide-status         (and reg-bus-master-blk (fx+ reg-bus-master-blk 2)))
  (define reg-bmide-prd-table-addr (and reg-bus-master-blk (fx+ reg-bus-master-blk 4)))

  (define bmide-command-write #b1000)   ;PCI bus master write
  (define bmide-command-start #b0001)   ;start bus master

  (define bmide-status-simplex-only        #b10000000) ;RO
  (define bmide-status-PRDIS               #b10000000) ;R/WC
  (define bmide-status-drive-1-dma-capable #b01000000) ;R/W
  (define bmide-status-drive-0-dma-capable #b00100000) ;R/W
  (define bmide-status-interrupt           #b00000100) ;R/WC
  (define bmide-status-error               #b00000010) ;R/WC
  (define bmide-status-ACT                 #b00000001) ;RO

;;;

  (define (sleep-100ns n)
    ;; Reading the alternate status register takes 100ns, it is claimed.
    (do ((n n (fx- n 1)))
        ((eqv? n 0))
      (get-i/o-u8 reg-ctl-alternate-status)))

  ;; Reset the connected devices and setup the controller
  (define (reset)
    (put-i/o-u8 reg-ctl-device-control (fxior device-control-nIEN
                                              device-control-SRST))
    (sleep 1/1000)
    (put-i/o-u8 reg-ctl-device-control #x00))

  (define (wait-for-ready)
    ;; Wait for the channel to be ready. This can take forever if
    ;; nothing is connected. FIXME: should return a timeout.
    (let lp ((i 1000))
      (cond ((eqv? i 0)
             (sleep 10)
             (lp 1000))
            (else
             (let ((status (get-i/o-u8 reg-cmd-status)))
               (unless (status-ready? status)
                 (sleep 10/1000)
                 (lp (fx- i 1))))))))

  ;; Wait for an interrupt. IRQ sharing can mean that we wake up even
  ;; when the operation is not done. Returns #f if the drive is ready
  ;; before the timeout.
  (define (wait-irq drive-number)
    (let lp ()
      (let* ((x (perform-operation (choice-operation
                                    (wrap-operation (wait-irq-operation irq)
                                                    (lambda _ 'irq))
                                    (wrap-operation (sleep-operation command-timeout)
                                                    (lambda _ 'timeout)))))
             (status (get-i/o-u8 reg-cmd-status)))
        (cond ((eqv? (fxand status ata-status-BSY) 0)
               ;; (display (list x (get-i/o-u8 reg-bmide-status)))
               ;; (newline)
               (when (and (eq? x 'timeout) (not (eqv? status 0)))
                 (display (list 'T status)) (newline)
                 #;
                 (write ;; raise-continuable
                  (condition
                   (make-warning)
                   (make-who-condition 'driver·ide)
                   (make-message-condition "The ATA controller IRQ is not working")
                   (make-irritants-condition (list drive-number)))))
               #f)
              (else
               (if (eq? x 'irq)
                   (lp)                 ;spurious wakeup
                   '(error timeout)))))))

  ;; Map ATA-8 ACS inputs to ATA8-APT
  (define (ide-send-command drive-number cmd)
    (wait-for-ready)
    (acknowledge-irq irq)
    (match cmd
      [#(feature count lba device command)
       (let* ((device (fxarithmetic-shift-right device 8))
              (device (fxior (if (eqv? drive-number 0) 0 ata-device-DRV)
                             (fxand device (fxnot ata-device-DRV))
                             ata-device-obs
                             (if (eqv? 0 (fxand device ata-device-LBA))
                                 0
                                 (fxbit-field lba 24 28)))))
         (put-i/o-u8 reg-cmd-device device)
         (sleep-100ns 4))
       (put-i/o-u8 reg-cmd-feature (fxbit-field feature 8 16))
       (put-i/o-u8 reg-cmd-feature (fxbit-field feature 0 8))
       (put-i/o-u8 reg-cmd-count (fxbit-field count 8 16))
       (put-i/o-u8 reg-cmd-count (fxbit-field count 0 8))
       (put-i/o-u8 reg-cmd-lba-high (fxbit-field lba 40 48))
       (put-i/o-u8 reg-cmd-lba-mid (fxbit-field lba 32 40))
       (put-i/o-u8 reg-cmd-lba-low (fxbit-field lba 24 32))
       (put-i/o-u8 reg-cmd-lba-high (fxbit-field lba 16 24))
       (put-i/o-u8 reg-cmd-lba-mid (fxbit-field lba 8 16))
       (put-i/o-u8 reg-cmd-lba-low (fxbit-field lba 0 8))
       (put-i/o-u8 reg-cmd-command command)]
      [else 'bad-cmd]))

  ;; Map the ATA8-APT response to ATA-8 ACS
  (define (ide-read-response)
    ;; Read higher order bits
    (put-i/o-u8 reg-ctl-device-control device-control-HOB)
    (let ((CH (get-i/o-u8 reg-cmd-count))
          (EH (get-i/o-u8 reg-cmd-error))
          (SH (get-i/o-u8 reg-cmd-status))
          (lba47:40 (get-i/o-u8 reg-cmd-lba-high))
          (lba39:32 (get-i/o-u8 reg-cmd-lba-mid))
          (lba31:24 (get-i/o-u8 reg-cmd-lba-low)))
      ;; Read lower order bits
      (put-i/o-u8 reg-ctl-device-control 0)
      (let ((CL (get-i/o-u8 reg-cmd-count))
            (EL (get-i/o-u8 reg-cmd-error))
            (SL (get-i/o-u8 reg-cmd-status))
            (lba23:16 (get-i/o-u8 reg-cmd-lba-high))
            (lba15:8 (get-i/o-u8 reg-cmd-lba-mid))
            (lba7:0 (get-i/o-u8 reg-cmd-lba-low)))
        (let ((error (fxior (fxarithmetic-shift-left EH 8) EL))
              (count (fxior (fxarithmetic-shift-left CH 8) CL))
              (lba (fxior (fxarithmetic-shift-left lba47:40 40)
                          (fxarithmetic-shift-left lba39:32 32)
                          (fxarithmetic-shift-left lba31:24 24)
                          (fxarithmetic-shift-left lba23:16 16)
                          (fxarithmetic-shift-left lba15:8 8)
                          lba7:0))
              (status (fxior (fxarithmetic-shift-left SH 8) SL)))
          ;; XXX: Sometimes "error" is called "feature return"
          (vector error count lba status)))))

  ;; Read data with PIO
  (define (pio-data-in drive-number len)
    (let lp ()
      (or
        ;; FIXME: Handle timeouts here. Can require a reset.
        (wait-irq drive-number)
        (let ((status (get-i/o-u8 reg-cmd-status)))
          (cond
            ((not (eqv? (fxand status (fxior ata-status-ERR ata-status-DF)) 0))
             ;; Some kind of error
             (list 'ata-error (ide-read-response)))

            ((not (eqv? (fxand status ata-status-DRQ) 0))
             ;; Data is ready to read
             (let ((block (make-bytevector len)))
               (if (eqv? 0 (fxand len #b11))
                   (get-i/o-u32-n! reg-cmd-data (bytevector-address block)
                                   (bytevector-length block))
                   (get-i/o-u16-n! reg-cmd-data (bytevector-address block)
                                   (bytevector-length block)))
               (list 'ok (ide-read-response) block)))

            (else
             ;; No drive, probably
             (list 'ata-error (ide-read-response))))))))

  ;; IDE bus master.

  ;; This is the Physical Region Descriptor Table. It contains PRDs
  ;; that each point to a memory region. It is a flat list that ends
  ;; with a terminating entry.
  (define num-prd 512)
  ;; TODO: (dma-free prdt)
  (define &prdt
    (and reg-bmide-prd-table-addr
         ;; XXX: This must not cross a 64K boundary, but the memory
         ;; returned is always 4K aligned.
         (dma-allocate (fx* num-prd 8) #xfffff000)))

  ;; Writes a Physical Region Descriptor (PRD) to the PRD table
  (define (bmide-write-prd idx base-address byte-count eot?)
    (assert has-bmide?)
    (assert (fx<? -1 idx num-prd))
    (assert (eqv? 0 (fxand base-address 1)))
    (assert (eqv? 0 (fxand byte-count 1)))
    (assert (fx<=? 2 byte-count #x10000))
    (let ((addr (fx+ &prdt (fx* idx 8)))
          (byte-count (if (eqv? byte-count #x10000) 0 byte-count)))
      (put-mem-u32 addr base-address)
      (put-mem-u32 (fx+ addr 4) (if eot? (fxior byte-count #x80000000) byte-count))))

  ;; Prepare a DMA transfer by filling in the scatter/gather bufs (PRD)
  (define (bmide-prepare-transfer bufs direction)
    (assert has-bmide?)
    ;; Fill in the PRD table
    (let lp ((idx 0) (bufs bufs))
      (let ((desc (car bufs)) (bufs (cdr bufs)))
        (bmide-write-prd idx (car desc) (cdr desc) (null? bufs))
        (unless (null? bufs)
          (lp (fx+ idx 1) bufs))))
    (put-i/o-u32 reg-bmide-prd-table-addr &prdt)
    (bmide-clear))

  (define (bmide-perform drive-number bufs direction)
    (assert has-bmide?)
    (put-i/o-u8 reg-bmide-command
                (case direction
                  ((read) bmide-command-start)
                  ((write) (fxior bmide-command-write
                                  bmide-command-start))))
    (let lp ()
      (match (wait-irq drive-number)
        [('error 'timeout)
         (put-i/o-u8 reg-bmide-command 0)
         (list 'error 'timeout)]
        [_
         (let ((bmide-status (get-i/o-u8 reg-bmide-status)))
           (cond
             ((not (eqv? (fxand bmide-status-ACT bmide-status) 0))
              ;; Bus mastering is still going on. Probably sent a
              ;; command that didn't transfer anything.
              (put-i/o-u8 reg-bmide-command 0)
              (list 'ata-error (ide-read-response) 'bus-master-timeout))
             (else
              (put-i/o-u8 reg-bmide-command 0)
              (let* ((status (get-i/o-u8 reg-cmd-status))
                     (resp (ide-read-response)))
                (cond
                  ((not (eqv? (fxand status (fxior ata-status-ERR ata-status-DF)) 0))
                   (list 'ata-error resp))
                  (else
                   (list 'ok resp)))))))])))

  (define (bmide-clear)
    (when has-bmide?
      (let ((status (fxand (get-i/o-u8 reg-bmide-status)
                           (fxior bmide-status-drive-1-dma-capable
                                  bmide-status-drive-0-dma-capable))))
        ;; Clear the RW/C bits, keep the capability bits
        (put-i/o-u8 reg-bmide-status
                    (fxior status bmide-status-PRDIS
                           bmide-status-interrupt bmide-status-error))
        (put-i/o-u8 reg-bmide-command 0))))

  (define (ide-protocol-dma drive-number bufs direction cmd)
    ;; XXX: The direction is from the point of view of the device
    (assert reg-bmide-prd-table-addr)
    (bmide-prepare-transfer bufs bufs)
    (ide-send-command drive-number cmd)
    (bmide-perform drive-number bufs direction))

  (define bounce-length 4096)
  (define &buf (dma-allocate bounce-length #xfffff000))

  (bmide-clear)
  (enable-irq irq)

  ;; TODO: The response should probably be sent in the choice-operation
  (let lp ()
    (match (perform-operation
            (choice-operation
             ;; Incoming requests from the rest of the system
             (wrap-operation (get-operation channel-0) (lambda (x) (cons 0 x)))
             (wrap-operation (get-operation channel-1) (lambda (x) (cons 1 x)))))

      [(drive-number resp-ch ('dma-data-in data-len) (? vector? cmd))
       (cond
         ((not has-bmide?)
          (put-message resp-ch (list 'error 'dma-not-supported)))
         ((not (fx<=? 0 data-len bounce-length))
          (put-message resp-ch (list 'error 'request-too-large)))
         (else
          (let* ((bufs (list (cons &buf data-len)))
                 (result
                  (match (ide-protocol-dma drive-number bufs 'write cmd)
                    [('ok resp)
                     (list 'ok resp
                           (let ((ret (make-bytevector data-len)))
                             (do ((i 0 (fx+ i 4)))
                                 ((fx=? i data-len))
                               (bytevector-u32-native-set! ret i (get-mem-u32 (fx+ &buf i))))
                             ret))]
                    [x x])))
            (put-message resp-ch result))))
       (lp)]

      [(drive-number resp-ch ('packet-in cdb data-len) (? vector? cmd))
       (cond
         ((not has-bmide?)
          (put-message resp-ch (list 'error 'dma-not-supported)))
         ((not (fx<=? 0 data-len bounce-length))
          (put-message resp-ch (list 'error 'request-too-large)))
         (else
          (let ((bufs (list (cons &buf bounce-length))))
            (bmide-prepare-transfer bufs bufs)
            ;; Send the ATA command
            (match cmd
              [#(feature sector-count lba device command)
               (let* ((feature (fxior feature #b1)) ;DMA
                      (cmd (vector feature 0 0 0 command)))
                 (ide-send-command drive-number cmd))])
            ;; Poll for BSY to go away
            (do () ((eqv? 0 (fxand (get-i/o-u8 reg-cmd-status) ata-status-BSY)))
              (yield-current-task))
            ;; Poll for DRQ or ERR
            (do () ((not (eqv? 0 (fxand (get-i/o-u8 reg-cmd-status)
                                        (fxior ata-status-DRQ ata-status-ERR)))))
              (yield-current-task))
            (let ((status (get-i/o-u8 reg-cmd-status)))
              (cond
                ((not (eqv? (fxand status (fxior ata-status-ERR ata-status-DF)) 0))
                 (put-message resp-ch (list 'ata-error (ide-read-response))))
                (else
                 ;; Send the SCSI CDB ("ATAPI Packet")
                 (let ((atapi-packet (make-bytevector 12 0)))
                   (bytevector-copy! cdb 0 atapi-packet 0 (fxmin 12 (bytevector-length cdb)))
                   (put-i/o-u32 reg-cmd-data (bytevector-u32-native-ref atapi-packet 0))
                   (put-i/o-u32 reg-cmd-data (bytevector-u32-native-ref atapi-packet 4))
                   (put-i/o-u32 reg-cmd-data (bytevector-u32-native-ref atapi-packet 8)))
                 ;; And finally start DMA to receive the data to the
                 ;; bounce buffer
                 (let* ((result
                         ;; XXX: direction
                         (match (bmide-perform drive-number bufs 'write)
                           [('ok resp)
                            (list 'ok resp
                                  (let ((ret (make-bytevector data-len)))
                                    (do ((i 0 (fx+ i 1)))
                                        ((fx=? i data-len))
                                      (bytevector-u8-set! ret i (get-mem-u8 (fx+ &buf i))))
                                    ret))]
                           [x x])))
                   (put-message resp-ch result))))))))
       (lp)]

      [(drive-number resp-ch ('pio-data-in data-len) (? vector? cmd))
       (ide-send-command drive-number cmd)
       (put-message resp-ch (pio-data-in drive-number data-len))
       (lp)]

      [(drive-number resp-ch protocol cmd)
       (put-message resp-ch (list 'error 'bad-request protocol cmd))
       (lp)]

      [(drive-number . _)
       (lp)]))

  (dma-free &buf)))
