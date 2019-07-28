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

;;; Standard library for symbols

(library (loko libs symbols)
  (export
    symbol? symbol->string #;symbol=? string->symbol

    gensym gensym? gensym->unique-string gensym-prefix
    *unbound-hack*
    symbol-value set-symbol-value!
    $gensym-generate-names!)
  (import
    (except (rnrs) symbol? symbol->string symbol=? string->symbol)
    (prefix (rnrs) sys:)
    (loko libs context)
    (loko system $primitives))

(define *unbound-hack* (vector 'unbound))

(define (symbol? x)
  (or ($immsym? x)
      (and ($box? x)
           (let ((t ($box-type x)))
             (and ($box-header? t)
                  (eqv? ($box-header-type t) #x03))))))

(define (symbol->string v)
  (unless (symbol? v)
    (assertion-violation 'symbol->string "This procedure needs a symbol." v))
  ;; TODO: This is really silly. The returned string can actually be
  ;; immutable. These output ports cons an awful lot.
  (call-with-string-output-port
    (lambda (p)
      (display v p))))

;; FNV-1 hash
(define bytevector-hash
  (case-lambda
    ((bv)
     (define offset_basis 2166136261)
     (bytevector-hash bv offset_basis))
    ((bv d)
     (define FNV_prime 16777619)
     (do ((i 0 (fx+ i 1))
          (d d
             (fxand (fxxor (fx* d FNV_prime)
                           (bytevector-u8-ref bv i))
                    #xffffffff)))
         ((fx=? i (bytevector-length bv)) d)))))

(define *interned* (make-hashtable bytevector-hash bytevector=?))

(define (string->symbol s)
  (define alphabet     "abcdefghijklmnopqrstuvwxyz-/<=>")
  (define end-alphabet "acdefghklmnopqrstvxy!*+-/08<=>?")
  (define (string-index s c)
    (let lp ((i 0))
      (and (not (fx=? i (string-length s)))
           (let ((c* (string-ref s i)))
             (if (eq? c c*)
                 i
                 (lp (fx+ i 1)))))))
  (define (immsym-encode s)
    (let ((len (string-length s)))
      (and (fx<=? 1 len 12)
           (let ((ret (string-index end-alphabet (string-ref s (fx- len 1)))))
             (and ret
                  (let lp ((ret (fx+ ret 1)) (i (fx- len 2)))
                    (if (eq? i -1)
                        ($fixnum->immsym ret)
                        (let ((j (string-index alphabet (string-ref s i))))
                          (and j
                               (lp (fxior (fxarithmetic-shift-left ret 5)
                                          (fx+ j 1))
                                   (fx- i 1)))))))))))
  (define (find-bootstrap-symbol bv)
    (let ((G/V ($bootstrap-symbols)))
      (let ((G (car G/V)) (V (cdr G/V)))
        (let* ((d (vector-ref G (fxmod (bytevector-hash bv) (vector-length G))))
               (idx (if (negative? d)
                        (fx- -1 d)
                        (fxmod (bytevector-hash bv d)
                               (vector-length V))))
               (sym (vector-ref V idx)))
          (and (bytevector=? ($box-ref sym 0) bv)
               sym)))))
  (unless (string? s)
    (assertion-violation 'string->symbol "Expected a string" s))
  (or (immsym-encode s)
      ;; TODO: it might be a good idea to not use string->utf8 here.
      ;; less consing.
      (let ((bv (string->utf8 s)))
        (or
          (find-bootstrap-symbol bv)
          (hashtable-ref *interned* bv #f)
          (let ((sym ($make-box ($make-box-header 'symbol #t 0 1) 1)))
            ($box-set! sym 0 bv)
            (hashtable-set! *interned* bv sym)
            sym)))))

;;; gensyms
;; TODO: collect all the gensym related stuff here

(define (gensym? x)
  ;; If the size field of the box is not two, then there's a
  ;; unique-string there. And maybe a value? And maybe an index into
  ;; the top level environment?
  (and ($box? x)
       (let ((t ($box-type x)))
         (and ($box-header? t)
              (eqv? ($box-header-type t) #x03)
              (not (eqv? ($box-header-length t) 1))))))

(define (gensym->unique-string v)
  (assert (gensym? v))
  (when (not ($box-ref v 1))
    ($gensym-generate-names! v))
  (utf8->string ($box-ref v 1)))

(define (gensym-prefix v)
  (assert (gensym? v))
  (when (not ($box-ref v 0))
    ($gensym-generate-names! v))
  (utf8->string ($box-ref v 0)))

(define gensym
  (case-lambda
    (()
     (gensym *unbound-hack*))
    ((prefix)
     (let ((p (cond ((eq? prefix *unbound-hack*) #f)
                    ((symbol? prefix) (string->utf8 (symbol->string prefix)))
                    ((string? prefix) (string->utf8 prefix))
                    (else
                     (error 'gensym "This procedure needs a string or a symbol"
                            prefix)))))
       (let ((ret ($make-box ($make-box-header 'symbol #t 0 3) 3)))
         ($box-set! ret 0 p)
         ($box-set! ret 1 #f)         ;generated later
         ($box-set! ret 2 *unbound-hack*)
         ret)))))

(define (gensym-from-bootstrap? symbol)
  (eqv? ($box-header-value ($box-type symbol)) 1))

;; Unique strings for gensyms generated lazily.
(define $gensym-generate-names!
  (let ((g-count -1)
        (id-count -1))            ;TODO: should be some kind of UUID
    (lambda (g)
      (assert (gensym? g))
      (unless ($box-ref g 0)
        (set! g-count (+ g-count 1))
        ($box-set! g 0 (string->utf8 (string-append "g" (number->string g-count)))))
      (unless ($box-ref g 1)
        (set! id-count (+ id-count 1))
        ($box-set! g 1 (string->utf8 (string-append "bs-" (number->string id-count))))))))

(define (set-symbol-value! symbol value)
  (cond ((and (gensym? symbol) (not (gensym-from-bootstrap? symbol)))
         ($box-set! symbol 2 value))
        (else
         (error 'set-symbol-value! "TODO: Set a bootstrap symbol" symbol value))))

(define (symbol-value symbol)
  (cond ((gensym? symbol)
         (cond ((gensym-from-bootstrap? symbol)
                ;; A bootstrap gensym contains an index into the
                ;; process vector where the symbol's value is
                ;; stored. This is because they are shared between
                ;; all processes.
                (let ((idx ($box-ref symbol 2)))
                  (if (fixnum? idx)
                      (vector-ref ($processor-data-ref CPU-VECTOR:PROCESS-VECTOR) idx)
                      (error 'symbol-value "This symbol has no value" symbol))))
               (else
                ;; A non-bootstrap gensym just contains the value.
                (let ((x ($box-ref symbol 2)))
                  (if (eq? x *unbound-hack*)
                      (error 'eval "This variable has no value" symbol)
                      x)))))
        (else
         (error 'symbol-value "Expected a gensym" symbol)))))
