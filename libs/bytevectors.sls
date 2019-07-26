;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Loko Scheme - an R6RS Scheme compiler
;; Copyright © 2019 Göran Weinholt

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
#!r6rs

;;; Bytevectors

(library (loko libs bytevectors)
  (export
    native-endianness bytevector? make-bytevector bytevector-length
    bytevector=? bytevector-fill! bytevector-copy! bytevector-copy

    bytevector-u8-ref bytevector-s8-ref
    bytevector-u8-set! bytevector-s8-set!
    bytevector->u8-list u8-list->bytevector
    bytevector-uint-ref bytevector-sint-ref
    bytevector-uint-set! bytevector-sint-set!
    bytevector->uint-list bytevector->sint-list
    uint-list->bytevector sint-list->bytevector
    bytevector-u16-ref bytevector-s16-ref
    bytevector-u16-native-ref bytevector-s16-native-ref
    bytevector-u16-set! bytevector-s16-set!
    bytevector-u16-native-set! bytevector-s16-native-set!
    bytevector-u32-ref bytevector-s32-ref
    bytevector-u32-native-ref bytevector-s32-native-ref
    bytevector-u32-set! bytevector-s32-set!
    bytevector-u32-native-set! bytevector-s32-native-set!
    bytevector-u64-ref bytevector-s64-ref
    bytevector-u64-native-ref bytevector-s64-native-ref
    bytevector-u64-set! bytevector-s64-set!
    bytevector-u64-native-set! bytevector-s64-native-set!
    bytevector-ieee-single-native-ref bytevector-ieee-single-ref
    bytevector-ieee-double-native-ref bytevector-ieee-double-ref
    bytevector-ieee-single-native-set! bytevector-ieee-single-set!
    bytevector-ieee-double-native-set! bytevector-ieee-double-set!
    string->utf8 ;;string->utf16 string->utf32
    utf8->string utf16->string utf32->string)
  (import
    (loko system $bytevectors)
    (loko system $strings)
    (except (rnrs)
            native-endianness bytevector? make-bytevector bytevector-length
            bytevector=? bytevector-fill! bytevector-copy! bytevector-copy
            bytevector-u8-ref bytevector-s8-ref
            bytevector-u8-set! bytevector-s8-set!
            bytevector->u8-list u8-list->bytevector
            bytevector-uint-ref bytevector-sint-ref
            bytevector-uint-set! bytevector-sint-set!
            bytevector->uint-list bytevector->sint-list
            uint-list->bytevector sint-list->bytevector
            bytevector-u16-ref bytevector-s16-ref
            bytevector-u16-native-ref bytevector-s16-native-ref
            bytevector-u16-set! bytevector-s16-set!
            bytevector-u16-native-set! bytevector-s16-native-set!
            bytevector-u32-ref bytevector-s32-ref
            bytevector-u32-native-ref bytevector-s32-native-ref
            bytevector-u32-set! bytevector-s32-set!
            bytevector-u32-native-set! bytevector-s32-native-set!
            bytevector-u64-ref bytevector-s64-ref
            bytevector-u64-native-ref bytevector-s64-native-ref
            bytevector-u64-set! bytevector-s64-set!
            bytevector-u64-native-set! bytevector-s64-native-set!
            bytevector-ieee-single-native-ref bytevector-ieee-single-ref
            bytevector-ieee-double-native-ref bytevector-ieee-double-ref
            bytevector-ieee-single-native-set! bytevector-ieee-single-set!
            bytevector-ieee-double-native-set! bytevector-ieee-double-set!
            string->utf8 string->utf16 string->utf32
            utf8->string utf16->string utf32->string)
    (prefix (rnrs) sys:))

;; Answers the question: are the
;; bytevector-{s,u}{8,16,32}-{native-,}{ref,set!} procedures
;; open-coded by the code generator on this architecture when the
;; endianness argument is known?
(define-syntax opencoded?
  (lambda (x)
    (syntax-case x (ref set! u8)
      ;; All archs must have bytevector-u8-{ref,set!} open-coded.
      ((_ ref u8) #'#t)
      ((_ set! u8) #'#t)
      ;; TODO: handle non-opencoding of the native procedures.
      ((_ ref size)
       #'(memq 'size '(s8 u16 s16 u32 s32)))
      ((_ set! size)
       #'#f))))

(define (native-endianness)
  (endianness little))

(define (bytevector? x) (sys:bytevector? x))

(define make-bytevector
  (case-lambda
    ;; XXX: the hope here is that the memory management will have
    ;; taken care to zero out all memory before allowing it to be
    ;; used for objects.
    ((len) (make-bytevector len 0))
    ((len fill)
     (assert (fx>=? len 0))
     (let ((bv ($make-bytevector len)))
       (unless (eqv? fill 0)
         (bytevector-fill! bv fill))
       bv))))

(define (bytevector-length x) (sys:bytevector-length x))

(define (bytevector=? bv1 bv2)
  (let ((len (bytevector-length bv1)))
    (and (fx=? len (bytevector-length bv2))
         (do ((i 0 (fx+ i 1))
              (diff 0 (fxior diff
                             (fxxor
                              (bytevector-u8-ref bv1 i)
                              (bytevector-u8-ref bv2 i)))))
             ((fx=? i len) (fxzero? diff))))))

(define (bytevector-fill! bv fill)
  (assert (fx<=? -128 fill 255))
  (let* ((fill8 (fxbit-field fill 0 8))
         (fill32 (fx* #x01010101 fill8))
         (end8 (bytevector-length bv))
         (end32 (fxand end8 -4)))
    (do ((i 0 (fx+ i 4)))
        ((fx=? i end32)
         (do ((i end32 (fx+ i 1)))
             ((fx=? i end8))
           (bytevector-u8-set! bv i fill8)))
      (bytevector-u32-native-set! bv i fill32))))

;; This should definitely be open-coded in a lot of cases. It should
;; make use of how objects are aligned in memory, etc.
(define (bytevector-copy! s ss t ts k)
  (assert (fx>=? k 0))
  (assert (and (bytevector? s) (bytevector? t)))
  (if (and (eq? t s) (fx<? ss ts))
      (do ((i (fx- k 1) (fx- i 1)))
          ((fx=? i -1))
        (bytevector-u8-set! t (fx+ ts i)
                            (bytevector-u8-ref s (fx+ ss i))))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i k))
        (bytevector-u8-set! t (fx+ ts i)
                            (bytevector-u8-ref s (fx+ ss i))))))

(define (bytevector-copy bv)
  (let ((ret (make-bytevector (bytevector-length bv))))
    (bytevector-copy! bv 0 ret 0 (bytevector-length bv))
    ret))

;;; Bytes and octets

;; XXX: unless these procedures read the length, they will not
;; trigger an alignment check.

(define (bytevector-u8-ref x i) (sys:bytevector-u8-ref x i))

(define (bytevector-s8-ref x i) (sys:bytevector-s8-ref x i))

(define (bytevector-u8-set! x i v) (sys:bytevector-u8-set! x i v))

(define (bytevector-s8-set! x i v) (sys:bytevector-s8-set! x i v))

(define (bytevector->u8-list bv)
  (when (not (bytevector? bv))
    (assertion-violation 'bytevector->u8-list
                         "Expected a bytevector" bv))
  (do ((i (fx- (bytevector-length bv) 1) (fx- i 1))
       (ret '() (cons (bytevector-u8-ref bv i) ret)))
      ((fx=? i -1) ret)))

(define (u8-list->bytevector list)
  ;; XXX: length checks for improper lists, but will report the
  ;; wrong &who condition. TODO: add a length/f procedure?
  (let ((len (length list)))
    (do ((ret (make-bytevector len))
         (i 0 (fx+ i 1))
         (u8s list (cdr u8s)))
        ((fx=? i len) ret)
      (let ((o (car u8s)))
        (when (not (and (fixnum? o) (fx<=? 0 o 255)))
          (assertion-violation 'u8-list->bytevector
                               "Expected a list of octets" list))
        (bytevector-u8-set! ret i o)))))

;;; Integers of arbitrary size

(define (bytevector-uint-ref bv k endian size)
  ;; TODO: Read more bits per loop iteration.
  (assert (fxpositive? size))
  (assert (fx<=? 0 k (fx- (bytevector-length bv) size)))
  (case endian
    ((little)
     (do ((end (fx- k 1))
          (i (fx+ k (fx- size 1)) (fx- i 1))
          (ret 0 (bitwise-ior (bitwise-arithmetic-shift-left ret 8)
                              (bytevector-u8-ref bv i))))
         ((fx=? i end)
          ret)))
    ((big)
     (do ((end (fx+ k size))
          (i k (fx+ i 1))
          (ret 0 (bitwise-ior (bitwise-arithmetic-shift-left ret 8)
                              (bytevector-u8-ref bv i))))
         ((fx=? i end)
          ret)))
    (else
     (assertion-violation 'bytevector-uint-ref
                          "Unsupported endianness"
                          bv k endian size))))

(define (bytevector-sint-ref bv k endian size)
  (assert (fxpositive? size))
  (assert (fx<=? 0 k (fx- (bytevector-length bv) size)))
  (case endian
    ((little)
     (do ((end (fx- k 1))
          (i (fx+ k (fx- size 2)) (fx- i 1))
          (ret (bytevector-s8-ref bv (fx+ k (fx- size 1)))
               (bitwise-ior (bitwise-arithmetic-shift-left ret 8)
                            (bytevector-u8-ref bv i))))
         ((fx=? i end) ret)))
    ((big)
     (do ((end (fx+ k size))
          (i (fx+ k 1) (fx+ i 1))
          (ret (bytevector-s8-ref bv k)
               (bitwise-ior (bitwise-arithmetic-shift-left ret 8)
                            (bytevector-u8-ref bv i))))
         ((fx=? i end) ret)))
    (else
     (assertion-violation 'bytevector-uint-ref "Unsupported endianness"
                          bv k endian size))))

(define (bytevector-uint-set! bv k n^ endian size)
  (assert (fxpositive? size))
  (assert (fx<=? 0 k (fx- (bytevector-length bv) size)))
  (case endian
    ((little)
     (do ((i k (fx+ i 1))
          (end (fx+ k (fx- size 1)))
          (n n^ (bitwise-arithmetic-shift-right n 8)))
         ((fx=? i end)
          (unless (fx<=? 0 n 255)
            (assertion-violation 'bytevector-uint-set! "Integer out of range"
                                 bv k n^ endian size))
          (bytevector-u8-set! bv i n))
       (bytevector-u8-set! bv i (bitwise-and n #xff))))
    ((big)
     (do ((end k)
          (i (fx+ k (fx- size 1)) (fx- i 1))
          (n n^ (bitwise-arithmetic-shift-right n 8)))
         ((fx=? i end)
          (unless (fx<=? 0 n 255)
            (assertion-violation 'bytevector-uint-set! "Integer out of range"
                                 bv k n^ endian size))
          (bytevector-u8-set! bv i n))
       (bytevector-u8-set! bv i (bitwise-and n #xff))))
    (else
     (assertion-violation 'bytevector-uint-set! "Unsupported endianness"
                          bv k endian size))))

(define (bytevector-sint-set! bv k n^ endian size)
  (assert (fxpositive? size))
  (assert (fx<=? 0 k (fx- (bytevector-length bv) size)))
  (case endian
    ((little)
     (do ((end (fx+ k (fx- size 1)))
          (i k (fx+ i 1))
          (n n^ (bitwise-arithmetic-shift-right n 8)))
         ((fx=? i end)
          (unless (fx<=? -128 n 127)
            (assertion-violation 'bytevector-sint-set! "Integer out of range"
                                 bv k n^ endian size))
          (bytevector-s8-set! bv i n))
       (bytevector-u8-set! bv i (bitwise-and n #xff))))
    ((big)
     (do ((end k)
          (i (fx+ k (fx- size 1)) (fx- i 1))
          (n n^ (bitwise-arithmetic-shift-right n 8)))
         ((fx=? i end)
          (unless (fx<=? -128 n 127)
            (assertion-violation 'bytevector-sint-set! "Integer out of range"
                                 bv k n^ endian size))
          (bytevector-s8-set! bv i n))
       (bytevector-u8-set! bv i (bitwise-and n #xff))))
    (else
     (assertion-violation 'bytevector-sint-set!
                          "Unsupported endianness"
                          bv k endian size))))

(define (bytevector->uint-list bv endian size)
  (unless (eqv? 0 (fxmod (bytevector-length bv) size))
    (assertion-violation 'bytevector->uint-list
                         "Bytevector length must be a multiple of the size"
                         bv endian size))
  (case size
    ((1)
     (do ((i (fx- (bytevector-length bv) 1) (fx- i 1))
          (ret '() (cons (bytevector-u8-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((2)
     (do ((i (fx- (bytevector-length bv) 2) (fx- i 2))
          (ret '() (cons (bytevector-u16-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((4)
     (do ((i (fx- (bytevector-length bv) 4) (fx- i 4))
          (ret '() (cons (bytevector-u32-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((8)
     (do ((i (fx- (bytevector-length bv) 8) (fx- i 8))
          (ret '() (cons (bytevector-u64-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    (else
     (do ((i (fx- (bytevector-length bv) size) (fx- i size))
          (ret '() (cons (bytevector-uint-ref bv i endian size) ret)))
         ((fx<? i 0) ret)))))

(define (bytevector->sint-list bv endian size)
  (unless (eqv? 0 (fxmod (bytevector-length bv) size))
    (assertion-violation 'bytevector->sint-list
                         "Bytevector length must be a multiple of the size"
                         bv endian size))
  (case size
    ((1)
     (do ((i (fx- (bytevector-length bv) 1) (fx- i 1))
          (ret '() (cons (bytevector-s8-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((2)
     (do ((i (fx- (bytevector-length bv) 2) (fx- i 2))
          (ret '() (cons (bytevector-s16-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((4)
     (do ((i (fx- (bytevector-length bv) 4) (fx- i 4))
          (ret '() (cons (bytevector-s32-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    ((8)
     (do ((i (fx- (bytevector-length bv) 8) (fx- i 8))
          (ret '() (cons (bytevector-s64-ref bv i endian) ret)))
         ((fx<? i 0) ret)))
    (else
     (do ((i (fx- (bytevector-length bv) size) (fx- i size))
          (ret '() (cons (bytevector-sint-ref bv i endian size) ret)))
         ((fx<? i 0) ret)))))

(define (uint-list->bytevector list endian size)
  (let ((ret (make-bytevector (fx* size (length list)))))
    (do ((k 0 (fx+ k size))
         (list list (cdr list)))
        ((null? list) ret)
      (bytevector-uint-set! ret k (car list) endian size))))

(define (sint-list->bytevector list endian size)
  (let ((ret (make-bytevector (fx* size (length list)))))
    (do ((k 0 (fx+ k size))
         (list list (cdr list)))
        ((null? list) ret)
      (bytevector-sint-set! ret k (car list) endian size))))

;;; 16-bit integers

(define (bytevector-u16-ref bv idx endian)
  (define (wrong)
    (assertion-violation 'bytevector-s16-ref
                         "Unsupported endianness"
                         bv idx endian))
  (if (opencoded? ref u16)
      (case endian
        ((little)
         (sys:bytevector-u16-ref bv idx (endianness little)))
        ((big)
         (sys:bytevector-u16-ref bv idx (endianness big)))
        (else
         (wrong)))
      (let ((b2 (bytevector-u8-ref bv (fx+ idx 1)))
            (b1 (bytevector-u8-ref bv idx)))
        (case endian
          ((little)
           (fxior (fxarithmetic-shift-left b2 8) b1))
          ((big)
           (fxior (fxarithmetic-shift-left b1 8) b2))
          (else (wrong))))))

(define (bytevector-s16-ref bv idx endian)
  (define (wrong)
    (assertion-violation 'bytevector-s16-ref
                         "Unsupported endianness"
                         bv idx endian))
  (if (opencoded? ref s16)
      (case endian
        ((little) (sys:bytevector-s16-ref bv idx (endianness little)))
        ((big) (sys:bytevector-s16-ref bv idx (endianness big)))
        (else (wrong)))
      (case endian
        ((little)
         (let ((b2 (bytevector-s8-ref bv (fx+ idx 1)))
               (b1 (bytevector-u8-ref bv idx)))
           (fxior (fxarithmetic-shift-left b2 8) b1)))
        ((big)
         (let ((b2 (bytevector-u8-ref bv (fx+ idx 1)))
               (b1 (bytevector-s8-ref bv idx)))
           (fxior (fxarithmetic-shift-left b1 8) b2)))
        (else (wrong)))))

(define (bytevector-u16-native-ref bv i) (sys:bytevector-u16-native-ref bv i))

(define (bytevector-s16-native-ref bv i) (sys:bytevector-s16-native-ref bv i))

(define (bytevector-u16-set! bv idx v endianness)
  (assert (fx<=? 0 v #xffff))
  (case endianness
    ((little)
     (bytevector-u8-set! bv idx (fxand v #xff))
     (bytevector-u8-set! bv (fx+ idx 1) (fxarithmetic-shift-right v 8)))
    ((big)
     (bytevector-u8-set! bv idx (fxarithmetic-shift-right v 8))
     (bytevector-u8-set! bv (fx+ idx 1) (fxand v #xff)))
    (else
     (assertion-violation 'bytevector-u16-set!
                          "Unsupported endianness."
                          bv idx v endianness))))

(define (bytevector-s16-set! bv idx v endian)
  (assert (fx<=? (- (expt 2 15)) v (- (expt 2 15) 1)))
  (bytevector-u16-set! bv idx (fxand v #xffff) endian))

(define (bytevector-u16-native-set! bv i v)
  (sys:bytevector-u16-native-set! bv i v))

(define (bytevector-s16-native-set! bv i v)
  (sys:bytevector-s16-native-set! bv i v))

;;; 32-bit integers

(define (bytevector-u32-ref bv idx endian)
  (define (wrong)
    (assertion-violation 'bytevector-u32-ref
                         "Unsupported endianness."
                         bv idx endian))
  (if (opencoded? ref u32)
      (case endian
        ((little) (sys:bytevector-u32-ref bv idx (endianness little)))
        ((big) (sys:bytevector-u32-ref bv idx (endianness big)))
        (else (wrong)))
      (let ((b4 (bytevector-u8-ref bv (fx+ idx 3)))
            (b3 (bytevector-u8-ref bv (fx+ idx 2)))
            (b2 (bytevector-u8-ref bv (fx+ idx 1)))
            (b1 (bytevector-u8-ref bv idx)))
        (case endian
          ((little)
           (fxior (fxarithmetic-shift-left b4 24)
                  (fxarithmetic-shift-left b3 16)
                  (fxarithmetic-shift-left b2 8)
                  b1))
          ((big)
           (fxior (fxarithmetic-shift-left b1 24)
                  (fxarithmetic-shift-left b2 16)
                  (fxarithmetic-shift-left b3 8)
                  b4))
          (else (wrong))))))

(define (bytevector-s32-ref bv idx endian)
  (define (wrong)
    (assertion-violation 'bytevector-s32-ref
                         "Unsupported endianness."
                         bv idx endian))
  (if (opencoded? ref u32)
      (case endian
        ((little) (sys:bytevector-s32-ref bv idx (endianness little)))
        ((big) (sys:bytevector-s32-ref bv idx (endianness big)))
        (else (wrong)))
      (case endian
        ((little)
         (let ((b4 (bytevector-s8-ref bv (fx+ idx 3)))
               (b3 (bytevector-u8-ref bv (fx+ idx 2)))
               (b2 (bytevector-u8-ref bv (fx+ idx 1)))
               (b1 (bytevector-u8-ref bv idx)))
           (fxior (fxarithmetic-shift-left b4 24)
                  (fxarithmetic-shift-left b3 16)
                  (fxarithmetic-shift-left b2 8)
                  b1)))
        ((big)
         (let ((b4 (bytevector-u8-ref bv (fx+ idx 3)))
               (b3 (bytevector-u8-ref bv (fx+ idx 2)))
               (b2 (bytevector-u8-ref bv (fx+ idx 1)))
               (b1 (bytevector-s8-ref bv idx)))
           (fxior (fxarithmetic-shift-left b1 24)
                  (fxarithmetic-shift-left b2 16)
                  (fxarithmetic-shift-left b3 8)
                  b4)))
        (else (wrong)))))

(define (bytevector-u32-native-ref bv i) (sys:bytevector-u32-native-ref bv i))

(define (bytevector-s32-native-ref bv i) (sys:bytevector-s32-native-ref bv i))

(define (bytevector-u32-set! bv idx v endian)
  (assert (fx<=? 0 v #xffffffff))
  (case endian
    ((little)
     (bytevector-u16-set! bv idx (bitwise-and v #xffff)
                          (endianness little))
     (bytevector-u16-set! bv (fx+ idx 2)
                          (bitwise-arithmetic-shift-right v 16)
                          (endianness little)))
    ((big)
     (bytevector-u16-set! bv idx (bitwise-arithmetic-shift-right v 16)
                          (endianness big))
     (bytevector-u16-set! bv (fx+ idx 2)
                          (bitwise-and v #xffff)
                          (endianness big)))
    (else
     (assertion-violation 'bytevector-u32-set!
                          "Unsupported endianness."
                          bv idx v endian))))

(define (bytevector-s32-set! bv idx v endian)
  (assert (<= (- (expt 2 31)) v (- (expt 2 31) 1)))
  (bytevector-u32-set! bv idx (bitwise-and v #xffffffff) endian))

(define (bytevector-u32-native-set! bv i v)
  (sys:bytevector-u32-native-set! bv i v))

(define (bytevector-s32-native-set! bv i v)
  (sys:bytevector-s32-native-set! bv i v))

;;; 64-bit integers

(define (bytevector-u64-ref bv idx endian)
  (case endian
    ((little)
     (let ((dw1 (bytevector-u32-ref bv (fx+ idx 4) (endianness little)))
           (dw0 (bytevector-u32-ref bv idx (endianness little))))
       (bitwise-ior (bitwise-arithmetic-shift-left dw1 32)
                    dw0)))
    ((big)
     (let ((dw1 (bytevector-u32-ref bv (fx+ idx 4) (endianness big)))
           (dw0 (bytevector-u32-ref bv idx (endianness big))))
       (bitwise-ior (bitwise-arithmetic-shift-left dw0 32)
                    dw1)))
    (else
     (assertion-violation 'bytevector-u64-ref
                          "Unsupported endianness."
                          bv idx endian))))

(define (bytevector-s64-ref bv idx endian)
  (case endian
    ((little)
     (let ((dw1 (bytevector-s32-ref bv (fx+ idx 4) (endianness little)))
           (dw0 (bytevector-u32-ref bv idx (endianness little))))
       (bitwise-ior (bitwise-arithmetic-shift-left dw1 32)
                    dw0)))
    ((big)
     (let ((dw1 (bytevector-u32-ref bv (fx+ idx 4) (endianness big)))
           (dw0 (bytevector-s32-ref bv idx (endianness big))))
       (bitwise-ior (bitwise-arithmetic-shift-left dw0 32)
                    dw1)))
    (else
     (assertion-violation 'bytevector-u64-ref
                          "Unsupported endianness"
                          bv idx endian))))

(define (bytevector-u64-native-ref bv idx)
  (assert (fxzero? (fxand idx #b111)))
  (bytevector-u64-ref bv idx (native-endianness)))

(define (bytevector-s64-native-ref bv idx)
  (assert (fxzero? (fxand idx #b111)))
  (bytevector-s64-ref bv idx (native-endianness)))

(define (bytevector-u64-set! bv idx v endian)
  (assert (<= 0 v #xffffffffffffffff))
  (case endian
    ((little)
     (bytevector-u32-set! bv idx
                          (bitwise-and v #xffffffff)
                          (endianness little))
     (bytevector-u32-set! bv (fx+ idx 4)
                          (bitwise-arithmetic-shift-right v 32)
                          (endianness little)))
    ((big)
     (bytevector-u32-set! bv idx
                          (bitwise-arithmetic-shift-right v 32)
                          (endianness big))
     (bytevector-u32-set! bv (fx+ idx 4)
                          (bitwise-and v #xffffffff)
                          (endianness big)))
    (else
     (assertion-violation 'bytevector-u64-set!
                          "Unsupported endianness"
                          bv idx v endian))))

(define (bytevector-s64-set! bv idx v endian)
  (assert (<= (- (expt 2 63)) v (- (expt 2 63) 1)))
  (bytevector-u64-set! bv idx (bitwise-and v #xffffffffffffffff) endian))

(define (bytevector-u64-native-set! bv idx v)
  (assert (fxzero? (fxand idx #b111)))
  (bytevector-u64-set! bv idx v (native-endianness)))

(define (bytevector-s64-native-set! bv idx v)
  (assert (fxzero? (fxand idx #b111)))
  (bytevector-s64-set! bv idx v (native-endianness)))

;;; IEEE-754 representations

(define (bytevector-ieee-single-native-ref bv idx)
  (sys:bytevector-ieee-single-native-ref bv idx))

(define (bytevector-ieee-single-ref bv idx endian)
  (let ((tmp (make-bytevector 4)))
    (bytevector-u32-native-set! tmp 0 (bytevector-u32-ref bv idx endian))
    (bytevector-ieee-single-native-ref bv 0)))

(define (bytevector-ieee-double-native-ref bv idx)
  (sys:bytevector-ieee-double-native-ref bv idx))

(define (bytevector-ieee-double-ref bv idx endian)
  (let ((tmp (make-bytevector 8)))
    (bytevector-u64-native-set! tmp 0 (bytevector-u64-ref bv idx endian))
    (bytevector-ieee-double-native-ref bv 0)))

(define (bytevector-ieee-single-native-set! bv idx v)
  (sys:bytevector-ieee-single-native-set! bv idx v))

(define (bytevector-ieee-single-set! bv idx v endian)
  (let ((tmp (make-bytevector 4)))
    (bytevector-ieee-single-native-set! tmp 0 v)
    (bytevector-u32-set! bv idx (bytevector-u32-native-ref tmp 0) endian)))

(define (bytevector-ieee-double-native-set! bv idx v)
  (sys:bytevector-ieee-double-native-set! bv idx v))

(define (bytevector-ieee-double-set! bv idx v endian)
  (let ((tmp (make-bytevector 8)))
    (bytevector-ieee-double-native-set! tmp 0 v)
    (bytevector-u64-set! bv idx (bytevector-u64-native-ref tmp 0) endian)))

;;; Strings

(define (string->utf8 x)
  (call-with-bytevector-output-port
    (lambda (p) (put-string p x))
    (native-transcoder)))

;; (TODO: string->utf16 string->utf32)

(define (utf8->string x)
  (call-with-string-output-port
    (lambda (p)
      (define ref bytevector-u8-ref)
      (define fxasl fxarithmetic-shift-left)
      (define (put c)
        ;; (display "PUT: #\\x")
        ;; (display (number->string (char->integer c) 16))
        ;; (newline)
        (put-char p c))
      (define (valid? b) (fx=? (fxand b #b11000000) #b10000000))
      (let lp ((i 0))
        (unless (fx=? i (bytevector-length x))
          (let ((b (bytevector-u8-ref x i)))
            ;; TODO: the length can be decoded like a prefix code,
            ;; might be faster.
            (cond ((fx<? b #x80) ;;(fx=? (fxand b #b10000000) #b00000000)
                   ;; One byte
                   (put (integer->char b))
                   (lp (fx+ i 1)))
                  ((fx=? (fxand b #b11100000) #b11000000)
                   ;; Two bytes
                   (cond ((fx<? (fx+ i 1) (bytevector-length x))
                          (let* ((b2 (ref x (fx+ i 1)))
                                 (v (fxior (fxasl (fxand #b00011111 b) 6)
                                           (fxand #b00111111 b2))))
                            (cond ((or (fx<? v #x80)
                                       (not (valid? b2)))
                                   (put #\xFFFD)
                                   (cond ((valid? b2)
                                          (lp (fx+ i 2)))
                                         (else
                                          (lp (fx+ i 1)))))
                                  (else
                                   (put (integer->char v))
                                   (lp (fx+ i 2))))))
                         (else        ;premature end of input
                          (put #\xFFFD)
                          (lp (fx+ i 1)))))
                  ((fx=? (fxand b #b11110000) #b11100000)
                   ;; Three bytes
                   (cond ((fx<? (fx+ i 2) (bytevector-length x))
                          (let* ((b2 (ref x (fx+ i 1)))
                                 (b3 (ref x (fx+ i 2)))
                                 (v (fxior (fxasl (fxand #b00001111 b) 12)
                                           (fxasl (fxand #b00111111 b2) 6)
                                           (fxand #b00111111 b3))))
                            (cond ((or (fx<? v #x800)
                                       ;; The forbidden surrogate pairs
                                       (fx<=? #xD800 v #xDFFF)
                                       (not (valid? b2)) (not (valid? b3)))
                                   (put #\xFFFD)
                                   (cond ((and (valid? b2) (valid? b3))
                                          (lp (fx+ i 3)))
                                         ((valid? b2)
                                          (lp (fx+ i 2)))
                                         (else
                                          (lp (fx+ i 1)))))
                                  (else
                                   (put (integer->char v))
                                   (lp (fx+ i 3))))))
                         (else
                          (put #\xFFFD)
                          (lp (fx+ i 1)))))
                  ((fx=? (fxand b #b11111000) #b11110000)
                   ;; Four bytes
                   (cond ((fx<? (fx+ i 3) (bytevector-length x))
                          (let* ((b2 (ref x (fx+ i 1)))
                                 (b3 (ref x (fx+ i 2)))
                                 (b4 (ref x (fx+ i 3)))
                                 (v (fxior (fxasl (fxand #b00000111 b) 18)
                                           (fxasl (fxand #b00111111 b2) 12)
                                           (fxasl (fxand #b00111111 b3) 6)
                                           (fxand #b00111111 b4))))
                            (cond ((or (fx<? v #x10000) (fx>? v #x10FFFF)
                                       (not (valid? b2))
                                       (not (valid? b3))
                                       (not (valid? b4)))
                                   (put #\xFFFD)
                                   (cond ((and (valid? b2) (valid? b3) (valid? b4))
                                          (lp (fx+ i 4)))
                                         ((and (valid? b2) (valid? b3))
                                          (lp (fx+ i 3)))
                                         ((valid? b2)
                                          (lp (fx+ i 2)))
                                         (else
                                          (lp (fx+ i 1)))))
                                  (else
                                   (put (integer->char v))
                                   (lp (fx+ i 4))))))
                         (else
                          (put #\xFFFD)
                          (lp (fx+ i 1)))))
                  (else
                   ;; Invalid byte
                   (put #\xFFFD)
                   (lp (fx+ i 1))))))))))

(define utf16->string
  (case-lambda
    ((bv endian)
     (utf16->string bv endian #f))
    ((bv endian endianness-mandatory?)
     (call-with-string-output-port
       (lambda (p)
         (define (put c)
           (put-char p c))
         (let* ((BOM (if (fx<? (bytevector-length bv) 2)
                         #f
                         (let ((bom (bytevector-u16-ref bv 0 (endianness big))))
                           (cond ((fx=? bom #xFEFF) (endianness big))
                                 ((fx=? bom #xFFFE) (endianness little))
                                 (else #f)))))
                (endian (if endianness-mandatory? endian (or BOM endian))))
           (let lp ((i (if BOM 2 0))
                    (rem (if BOM (fx- (bytevector-length bv) 2) (bytevector-length bv))))
             (cond
               ((eqv? rem 0))
               ((eqv? rem 1) (put #\xFFFD))
               (else
                (let ((w0 (bytevector-u16-ref bv i endian))
                      (i^ (fx+ i 2))
                      (rem^ (fx- rem 2)))
                  (cond
                    ((fx<=? #xD800 w0 #xDFFF)
                     ;; Surrogate pair
                     (cond
                       ((fx<? rem^ 2)
                        (put #\xFFFD)
                        (lp i^ rem^))
                       (else
                        ;; Interesting: the ordering of the
                        ;; surrogate pairs forms a second level of
                        ;; endianness, which thankfully is fixed as
                        ;; big endian.
                        (let ((w1 (bytevector-u16-ref bv i^ endian))
                              (i^^ (fx+ i^ 2))
                              (rem^^ (fx- rem^ 2)))
                          (cond ((fx<=? #xD800 w1 #xDFFF)
                                 (let ((w (fxior (fxarithmetic-shift-left (fx- w0 #xD800) 10)
                                                 (fxbit-field (fx- w1 #xDC00) 0 10)
                                                 #x10000)))
                                   (cond ((fx>? w #x10FFFF)
                                          (put #\xFFFD)
                                          (lp i^ rem^))
                                         (else
                                          (put (integer->char w))
                                          (lp i^^ rem^^)))))
                                (else
                                 (put #\xFFFD)
                                 (lp i^ rem^)))))))
                    (else
                     (put (integer->char w0))
                     (lp i^ rem^)))))))))))))

(define utf32->string
  (case-lambda
    ((bv endian)
     (utf32->string bv endian #f))
    ((bv endian endianness-mandatory?)
     (call-with-string-output-port
       (lambda (p)
         (define (put c)
           (put-char p c))
         (let* ((BOM (if (fx<? (bytevector-length bv) 4)
                         #f
                         (let ((bom (bytevector-u32-ref bv 0 (endianness big))))
                           (cond ((eqv? bom #x0000FEFF) (endianness big))
                                 ((eqv? bom #xFFFE0000) (endianness little))
                                 (else #f)))))
                (endian (if endianness-mandatory? endian (or BOM endian))))
           (let lp ((i (if BOM 4 0))
                    (rem (if BOM (fx- (bytevector-length bv) 4) (bytevector-length bv))))
             (cond
               ((eqv? rem 0))
               ((fx<? rem 4)
                (put #\xFFFD))
               (else
                (let ((w0 (bytevector-u32-ref bv i endian))
                      (i^ (fx+ i 4))
                      (rem^ (fx- rem 4)))
                  (cond
                    ((fx<=? #xD800 w0 #xDFFF)
                     ;; Surrogate pair
                     (put #\xFFFD)
                     (lp i^ rem^))
                    (else
                     (put (integer->char w0))
                     (lp i^ rem^))))))))))))))


