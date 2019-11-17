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
    (loko match)
    (loko system fibers))

(define-record-type storage-device
  (sealed #t)
  (fields request-channel
          logical-sector-size)
  (protocol
   (lambda (p)
     (lambda (logical-sector-size)
       (p (make-channel)
          logical-sector-size)))))

(define (open-storage-device id storage-device)
  (define lba 0)
  (define logical-sector-size
    (storage-device-logical-sector-size storage-device))
  (define (read! bytevector start count)
    (let-values ([(blocks error) (fxdiv-and-mod count logical-sector-size)])
      (unless (eqv? error 0)
        (assertion-violation 'read!
                             "Expected a multiple of the block length" count))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i blocks))
        (let ((resp-ch (make-channel)))
          (put-message (storage-device-request-channel storage-device)
                       (list 'read resp-ch lba 1))
          (match (get-message resp-ch)
            [('ok data)
             (unless (eqv? (bytevector-length data) logical-sector-size)
               (error 'read! "Bad data length from storage device"
                      (bytevector-length data)))
             (bytevector-copy! data 0 bytevector
                               (fx+ start (fx* i logical-sector-size))
                               (bytevector-length data))]
            [('error _)
             (error 'read! "Error from storage device")])))
      (set! lba (fx+ lba blocks))
      (fx* blocks logical-sector-size)))
  (define (get-position)
    (fx* lba logical-sector-size))
  (define (set-position! pos)
    (let-values ([(lba^ error) (fxdiv-and-mod pos logical-sector-size)])
      (unless (eqv? error 0)
        (assertion-violation 'set-position!
                             "Expected a multiple of the logical sector length"
                             pos))
      (set! lba lba^)))
  (define (close)
    #f)
  (make-custom-binary-input-port id read! get-position set-position! close)))
