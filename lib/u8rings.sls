;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; Bytevector-based ring buffers

(library (loko u8rings)
  (export
    make-u8ring
    u8ring-start u8ring-length u8ring-data
    u8ring-empty? u8ring-full? u8ring-free-amount
    u8ring-head
    u8ring-clear!
    u8ring-dequeue! u8ring-dequeue-n! u8ring-dequeue-all!
    u8ring-enqueue! u8ring-enqueue-bytevector!)
  (import
    (rnrs (6)))

;; Bytevector FIFO backed by a ring buffer
(define-record-type u8ring
  (sealed #t)
  (fields (mutable start)               ;index of first valid byte
          (mutable length)              ;number of valid bytes
          data)                         ;2^n bytevector
  (protocol
   (lambda (p)
     (lambda (smallest-size)
       (define (next-power-of-2 n)
         (fxarithmetic-shift-left 1 (fxlength (fx- n 1))))
       (let ((size (next-power-of-2 smallest-size)))
         (p 0 0 (make-bytevector size)))))))

(define (u8ring-clear! r)
  (u8ring-start-set! r 0)
  (u8ring-length-set! r 0))

(define (u8ring-empty? r)
  (eqv? (u8ring-length r) 0))

(define (u8ring-full? r)
  (eqv? (u8ring-free-amount r) 0))

(define (u8ring-free-amount r)
  (fx- (bytevector-length (u8ring-data r))
       (u8ring-length r)))

(define (u8ring-head r)
  (let ((start (u8ring-start r))
        (data (u8ring-data r)))
    (if (u8ring-empty? r)
        #f
        (bytevector-u8-ref data start))))

(define (u8ring-dequeue! r)
  (let* ((start (u8ring-start r))
         (len (u8ring-length r))
         (data (u8ring-data r)))
    (unless (u8ring-empty? r)
      (u8ring-start-set! r (fxand (fx+ start 1) (fx- (bytevector-length data) 1)))
      (u8ring-length-set! r (fx- len 1))
      (bytevector-u8-ref data start))))

(define (u8ring-dequeue-n! r bv start count)
  (let* ((len (u8ring-length r))
         (count (fxmin count len)))
    (do ((i 0 (fx+ i 1)))
        ((fx=? i count))
      (bytevector-u8-set! bv (fx+ start i) (u8ring-dequeue! r)))
    count))

(define (u8ring-dequeue-all! r)
  (let* ((len (u8ring-length r))
         (bv (make-bytevector len)))
    (u8ring-dequeue-n! r bv 0 len)
    bv))

(define (u8ring-enqueue! r b)
  (let ((start (u8ring-start r))
        (len (u8ring-length r))
        (data (u8ring-data r)))
    (unless (u8ring-full? r)
      (let ((end (fxand (fx+ start len) (fx- (bytevector-length data) 1))))
        (bytevector-u8-set! data end b)
        (u8ring-length-set! r (fx+ len 1))
        b))))

(define (u8ring-enqueue-bytevector! r bv start count)
  (let ((rstart (u8ring-start r))
        (rdata (u8ring-data r))
        (rlength (u8ring-length r)))
    (let ((count (fxmin (u8ring-free-amount r) count))
          (rend (fxand (fx+ rstart rlength) (fx- (bytevector-length rdata) 1))))
      (let* ((len0 (fxmin count (fx- (bytevector-length rdata) rend)))
             (len1 (fx- count len0)))
        (u8ring-length-set! r (fx+ rlength count))
        (bytevector-copy! bv start rdata rend len0)
        (unless (eqv? len1 0)
          (bytevector-copy! bv (fx+ start len0) rdata 0 len1))
        count)))))
