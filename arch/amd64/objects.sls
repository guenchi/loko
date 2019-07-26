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

;;; amd64 object serialization, tagging, data layout, etc.

(library (loko arch amd64 objects)
  (export
    tag mask shift btag
    immediate
    encode-object
    amd64-fixnum-width
    amd64-least-fixnum
    amd64-greatest-fixnum)
  (import
    (only (loko compiler recordize) const?
          const-value const-ref set-const-ref!)
    (only (loko compat) gensym? gensym->unique-string gensym-prefix)
    (rnrs))

(define-syntax define-inlined
  (lambda (x)
    (syntax-case x ()
      ((_ (name f* ...) . body)
       (with-syntax (((fun) (generate-temporaries #'(name))))
         #'(begin
             (define (fun f* ...)
               . body)
             (define-syntax name
               (lambda (x)
                 (syntax-case x ()
                   ((_ arg* (... ...))
                    (if (exists (lambda (c)
                                  (or (pair? c) (identifier? c)))
                                #'(arg* (... ...)))
                        #f #t)
                    #'((lambda (f* ...) . body) arg* (... ...)))
                   ((_ arg* (... ...))
                    #'(fun arg* (... ...))))))))))))

(define-inlined (amd64-fixnum-width)
  (- 64 (shift 'fixnum)))

(define-inlined (amd64-least-fixnum)
  (- (bitwise-arithmetic-shift 1 (- (amd64-fixnum-width) 1))))

(define-inlined (amd64-greatest-fixnum)
  (- (bitwise-arithmetic-shift 1 (- (amd64-fixnum-width) 1)) 1))

;; bignum
(define-inlined (amd64-int-width) 30)
(define-inlined (amd64-int^-width)
  ;; must not be larger than (- (fixnum-width) 1)
  (fx+ (fx* (amd64-int-width) 2) 1))
(define-inlined (amd64-int-mask)
  (define (amd64-int-radix)
    (fxarithmetic-shift-left 1 (amd64-int-width)))
  (fx- (amd64-int-radix) 1))

(define-inlined (tag type)
  (case type
    ((fixnum)         #b000)          ;special under bitwise-ior
    ((box)            #b001)
    ((pair)           #b010)
    ((procedure)      #b011)
    ((string)         #b100)
    ((vector)         #b110)
    ((bytevector)     #b101)
    ((immsym)        #b0111)
    ;; Objects with 8-bit tags
    ((char)       #b00011111)
    ((boolean)    #b10001111)
    ((flonum)     #b01001111)
    ((null)
     (bitwise-ior #b00101111 (* 0 (expt 2 (shift 'singular)))))
    ((eof-object)
     (bitwise-ior #b00101111 (* 1 (expt 2 (shift 'singular)))))
    ((moved-mark)
     (bitwise-ior #b00101111 (* 2 (expt 2 (shift 'singular)))))
    ;; 12-bit tags
    ((kill-mark)  #b100000001111)
    ;; 16-bit tags
    ((void)       #b0000000000001111)
    ((seek-mark)
     ;; XXX: This is handled specially because of the high bit. The
     ;; bit is set so that no canonical address (in particular a
     ;; return address) can be mistaken for a seek-mark.
     (bitwise-ior #b1000000000001111 (bitwise-arithmetic-shift-left 1 63)))
    ((box-header) #b0100000000001111)
    (else
     (error 'shift "Unknown type for tag on amd64" type))))

(define-inlined (mask type)
  (case type
    ((fixnum box pair procedure string vector bytevector)
     #b111)
    ((immsym)
     #b1111)
    ((char boolean flonum)
     #b11111111)
    ((kill-mark void)
     #xfff)
    ((seek-mark)
     (bitwise-ior #xffff (bitwise-arithmetic-shift-left 1 63)))
    ((null eof-object moved-mark box-header)
     #xffff)
    ;; These are different from the ones above. A box may start with a
    ;; box header. The length field is the length of the rest of the
    ;; box, in qwords.
    ((box-header:length) #xffffffff)
    ((box-header:refs?) #b1)
    ((box-header:type) #x7f)
    ((box-header:value) #xff)
    (else
     (error 'mask "Unknown type for tag mask on amd64" type))))

(define-inlined (shift type)
  (case type
    ((fixnum) 3)
    ((immsym) 4)
    ((char boolean) 8)
    ((void) 12)
    ((flonum) 32)
    ((kill-mark) 12)
    ((seek-mark) 16)
    ((singular) 8)
    ((box-header:length) 32)
    ((box-header:refs?) 31)
    ((box-header:type) 24)
    ((box-header:value) 16)
    (else
     (error 'shift
            "Unknown type for immediate tag shift amount on amd64" type))))

(define-inlined (btag type)
  (case type
    ((bignum) #x01)
    ((ratnum) #x02)
    ((symbol) #x03)
    (else
     (error 'btag "Unknown type for box header type on amd64" type))))

(define (box-header type refs? value length)
  (assert (= length (bitwise-and length (mask 'box-header:length))))
  (assert (boolean? refs?))
  (assert (= value (bitwise-and value (mask 'box-header:value))))
  (bitwise-ior (bitwise-arithmetic-shift-left length (shift 'box-header:length))
               (if refs? (bitwise-arithmetic-shift-left 1 (shift 'box-header:refs?)) 0)
               (bitwise-arithmetic-shift-left value (shift 'box-header:value))
               (bitwise-arithmetic-shift-left (btag type) (shift 'box-header:type))
               (tag 'box-header)))

(define (seek-mark bytes)
  (define (fxalign i alignment)
    (fxand (fx+ i (fx- alignment 1))
           (fx- alignment)))
  ;; The mark is used like this: shift it to the right, then add the
  ;; result to the scan pointer. The scan pointer is pointing at the
  ;; seek mark, so the mark is included in the length.
  (let ((a (fxalign (fx+ bytes 8) 8)))
    (bitwise-ior (tag 'seek-mark)
                 (fxarithmetic-shift-left a (shift 'seek-mark)))))

;;; Object serialization.

;; This encodes an immediate object as a bitpattern that can be
;; stored in a register or in memory. It is also OK to use this as
;; an immediate operand in an assembler instruction, but beware that
;; it might need to be loaded with mov if it's large.
(define (immediate x)
  (cond ((number? x)
         (cond ((and (exact? x) (integer? x)
                     (<= (amd64-least-fixnum) x (amd64-greatest-fixnum)))
                (bitwise-arithmetic-shift-left x (shift 'fixnum)))
               ((and (inexact? x) (real? x))
                (let ((bv (make-bytevector 4)))
                  (bytevector-ieee-single-set! bv 0 x (endianness little))
                  (let ((b (bytevector-u32-ref bv 0 (endianness little))))
                    (bitwise-ior (tag 'flonum)
                                 (bitwise-arithmetic-shift-left b (shift 'flonum))))))
               (else #f)))
        ((and (symbol? x) (not (gensym? x)))
         (let ((str (symbol->string x)))
           (encode-immsym str)))
        ((char? x)
         (bitwise-ior (tag 'char)
                      (bitwise-arithmetic-shift-left (char->integer x)
                                                     (shift 'char))))
        ((boolean? x)
         (bitwise-ior (tag 'boolean)
                      (bitwise-arithmetic-shift-left (if x 1 0)
                                                     (shift 'boolean))))
        ((null? x) (tag 'null))
        ((eof-object? x) (tag 'eof-object))
        ((eqv? x (if #f #f)) (tag 'void))
        (else #f)))

(define (encode-object x objs strings symbols gensyms bytevectors
                       gensym-idx-locations emit)
  (define-syntax with-interning-table
    (lambda (x)
      (syntax-case x ()
        ((_ (table value) body ...)
         #'(cond ((hashtable-ref table value #f))
                 (else
                  (let ((ref (begin body ...)))
                    (hashtable-set! table value ref)
                    ref)))))))

  (define (genref v type)
    (let ((label (vector 'const v)))  ;assembler label
      `(+ ,label ,(tag type))))       ;assembler operand

  (define (allocate! x type)          ;allocate label
    (when (and x (not (const-ref x)))
      (let ((ref (genref (const-value x) type)))
        (set-const-ref! x ref)
        ref)))

  (define (generate x type v . xs)      ;generate assembler
    (define (wrap f)
      (if (bytevector? f)
          (emit `(%vu8 ,f))
          (emit `(%u64 ,f))))
    (let ((ref (if (const? x) (const-ref x) (genref v type))))
      (emit `(%align 16 0))
      (emit `(%label ,(cadr ref)))
      (for-each wrap xs)
      ref))

  ;;(define objs (make-eq-hashtable))

  (define (encode* v)
    (cond ((immediate v))
          (else
           (with-interning-table (objs v)
                                 (encode #f v)))))

  (define (encode x v)
    (define (align i alignment)
      (fxand (fx+ i (fx- alignment 1))
             (fx- alignment)))
    (cond
      ((and x (const-ref x)))
      ((immediate v))
      ((and (bytevector? v) (< (bytevector-length v) (amd64-greatest-fixnum)))
       ;; XXX: this limit is too nice. the length must be
       ;; representable by a seek-mark.
       (with-interning-table (bytevectors v)
                             (allocate! x 'bytevector)
                             ;; TODO: Can the seek-mark be replaced by a box header?
                             (generate x 'bytevector v (immediate (bytevector-length v))
                                       (seek-mark (bytevector-length v))
                                       v)))
      ((string? v)
       (with-interning-table (strings v)
                             (allocate! x 'string)
                             (generate x 'string v (immediate (string-length v))
                                       (uint-list->bytevector
                                        (map immediate (string->list v))
                                        (endianness little) 4))))
      ((gensym? v)
       ;; Gensyms retain their identity and their unique string.
       (with-interning-table (gensyms v)
                             (allocate! x 'box)
                             (let ((ret
                                    (generate x 'box v (box-header 'symbol #t 1 3)
                                              (encode* (string->utf8 (symbol->string v)))
                                              (encode* (string->utf8 (gensym->unique-string v))))))
                               ;; This makes the assembler line for
                               ;; the gensym index fields available.
                               ;; Updated by the code generator.
                               (let ((gensym-env-index `(%u64 ,(immediate #f))))
                                 (hashtable-set! gensym-idx-locations v gensym-env-index)
                                 (emit gensym-env-index))
                               ret)))
      ((symbol? v)
       (with-interning-table (symbols v)
                             (allocate! x 'box)
                             (generate x 'box v (box-header 'symbol #t 0 1)
                                       (encode* (string->utf8 (symbol->string v))))))
      ((and (integer? v) (exact? v)
            ;; XXX: is this just too large?
            (< (bitwise-length v) (fxarithmetic-shift-left 1 20)))
       (do ((sign (if (negative? v) -1 1))
            (v (abs v) (bitwise-arithmetic-shift-right v (amd64-int-width)))
            (digits '() (cons (bitwise-and v (amd64-int-mask)) digits)))
           ((zero? v)
            (allocate! x 'box)
            (if #t
                (generate x 'box v (box-header 'bignum #t 0 3)
                          (immediate (length digits))
                          (immediate sign)
                          (encode* (list->vector (reverse digits))))
                (apply generate x 'box v
                       (box-header 'bignum #t (if (eqv? sign 1) 0 1)
                                   (fx+ 1 (length digits)))
                       (immediate (length digits))
                       (map immediate (reverse digits)))))))

      ((and (rational? v) (exact? v))
       ;; Stupid workaround for Ikarus bug #831582
       (let ((v (/ (+ v v) 2)))
         (cond ((= (denominator v) 1)
                (encode x (numerator v)))
               ((and (rational? v) (exact? v))
                (allocate! x 'box)
                (generate x 'box v
                          (box-header 'ratnum #t 0 2)
                          (encode* (numerator v))
                          (encode* (denominator v))))
               (else
                (encode x v)))))

      ((complex? v)
       ;; TODO: when under self-hosting, make it check for complex
       ;; numbers in polar representation.
       (allocate! x 'box)
       (generate x 'box v (encode* 'rcompnum) (immediate 2)
                 (encode* (real-part v))
                 (encode* (imag-part v))))

      ;; Pairs and vectors can contain shared structure, so they are
      ;; special. They go through the objs hashtable. TODO: check
      ;; that shared structures work.
      ((pair? v)
       (hashtable-set! objs x (allocate! x 'pair))
       (generate x 'pair v (encode* (car v)) (encode* (cdr v))))
      ((vector? v)
       (hashtable-set! objs x (allocate! x 'vector))
       (apply generate x 'vector v (immediate (vector-length v))
              (map encode* (vector->list v))))
      (else
       (error 'encode-object
              "This object can not be encoded on amd64" v))))

  (encode x (const-value x)))

;;; 5-bit immediate symbols

(define alphabet     "abcdefghijklmnopqrstuvwxyz-/<=>")
(define end-alphabet "acdefghklmnopqrstvxy!*+-/08<=>?")

;; TODO: looks slow
(define (encode-immsym s)
  (define (string-index s c)
    (let lp ((i 0))
      (and (not (fx=? i (string-length s)))
           (let ((c* (string-ref s i)))
             (if (eq? c c*)
                 i
                 (lp (fx+ i 1)))))))
  (define (list->immsym l)
    ;; Takes a list of up to 12 5-bit integers and encodes it as a
    ;; 64-bit integer. The lower four bits are left as zero. If the list
    ;; is shorter than 12 it is padded with zeros.
    (do ((l (reverse l) (cdr l))
         (c 0 (bitwise-ior (car l) (bitwise-arithmetic-shift-left c 5))))
        ((null? l)
         (bitwise-ior (tag 'immsym)
                      (bitwise-and #xffffffffffffffff
                                   (bitwise-arithmetic-shift-left c (shift 'immsym)))))))
  (let ((l (reverse (string->list s))))
    (and (<= 1 (string-length s) 12)
         (string-index end-alphabet (car l))
         (for-all (lambda (c) (string-index alphabet c))
                  (cdr l))
         (list->immsym
          (reverse
           (cons 0 (cons (+ 1 (string-index end-alphabet (car l)))
                         (map (lambda (c) (+ 1 (string-index alphabet c)))
                              (cdr l)))))))))

#;(define (decode-immsym i)
    (define (immsym->list c)
      (do ((c (bitwise-arithmetic-shift-right c (shift 'immsym))
              (bitwise-arithmetic-shift-right c 5))
           (l '() (cons (bitwise-and c 31) l)))
          ((zero? c) (reverse l))))
    (define (symbol x)
      (let ((points (if (memv 0 x) (cdr (memv 0 x)) x)))
        (list->string
         (reverse
          (cons (string-ref end-alphabet (- (car points) 1))
                (map (lambda (p) (string-ref alphabet (- p 1)))
                     (cdr points)))))))
    (define (gensym id)
      (string-append "$gensym$" (string-reverse (number->string id 16))))
    (let ((x (immsym->list i)))
      (display x) (newline)
      (if (zero? (car x))
          (gensym (bitwise-arithmetic-shift-right i (+ 5 (shift 'immsym))))
          (symbol (reverse x)))))

;; (immsym->list (list->immsym '(1 2 3 4 5 6 7 8 0 2)))
;; => (1 2 3 4 5 6 7 8 0 2)
;; (immsym->list (list->immsym '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
;; => (1 2 3 4 5 6 7 8 9 10 11 12)

#;(define (encode-gensym id)
    ;; This is an immsym of length zero.
    (assert (<= 0 id (- (bitwise-arithmetic-shift-left 1 (- 64 (shift 'immsym) 5)) 1)))
    (bitwise-and #xffffffffffffffff
                 (bitwise-ior (tag 'immsym)
                              (bitwise-arithmetic-shift-left id (+ 5 (shift 'immsym))))))

;; (decode-immsym (encode-gensym #xffffffffffff))
;; (decode-immsym (encode-gensym #x3ffffffffffff))
;; (decode-immsym (encode-gensym #x7fffffffffffff))
;; (decode-immsym (encode-gensym #xCBA))
;; (decode-immsym (encode-immsym "abc?"))

)
