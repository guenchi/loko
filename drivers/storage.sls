;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; Storage device abstraction

(library (loko drivers storage)
  (export
    make-storage-device
    storage-device-request-channel

    open-storage-device)
  (import
    (rnrs (6))
    (loko system fibers))

(define-record-type storage-device
  (fields request-channel)
  (protocol
   (lambda (p)
     (lambda ()
       (p (make-channel))))))

(define (open-storage-device storage-device block-length)
  (define lba 0)
  (define (read! bytevector start count)
    (let-values ([(blocks error) (div-and-mod count 512)])
      (unless (eqv? error 0)
        (assertion-violation 'read!
                             "Expected a multiple of the block length" count))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i blocks))
        (let ((ch (make-channel)))
          (put-message (storage-device-request-channel storage-device)
                       (list 'read ch lba 1))
          (let ((resp (get-message ch)))
            (assert (eq? (car resp) 'ok))
            (let ((data (cdr resp)))
              (unless (eqv? (bytevector-length data) 512)
                (error 'read! "Bad data length from storage device"
                       (bytevector-length data)))
              (bytevector-copy! data 0 bytevector (fx+ start (fx* i block-length))
                                (bytevector-length data))))))
      (set! lba (fx+ lba blocks))
      (fx* blocks 512)))
  (define (get-position)
    (* lba 512))
  (define (set-position! pos)
    (let-values ([(lba^ error) (div-and-mod pos 512)])
      (unless (eqv? error 0)
        (assertion-violation 'set-position!
                             "Expected a multiple of the block length"
                             pos block-length))
      (set! lba lba^)))
  (define (close)
    #f)
  (assert (eqv? block-length 512))
  (make-custom-binary-input-port "usb" read! get-position set-position! close)))
