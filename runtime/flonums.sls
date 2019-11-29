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

;;; Primitives for flonums

(library (loko runtime flonums)
  (export
    flonum?
    fl=? fl<? fl<=? fl>? fl>=?
    flinteger? flzero? flpositive? flnegative?
    flodd? fleven? flfinite? flinfinite? flnan?
    flmax flmin
    fl+ fl* fl- fl/
    flabs
    fldiv-and-mod fldiv flmod
    fldiv0-and-mod0 fldiv0 flmod0
    flnumerator fldenominator
    flfloor flceiling fltruncate flround
    flexp fllog flsin flcos fltan
    flasin flacos flatan flsqrt flexpt
    fixnum->flonum)
  (import
    (rnrs base)
    (rnrs control)
    (rnrs lists)
    (rnrs syntax-case)
    (prefix (rnrs) sys:)
    (rnrs arithmetic fixnums)
    (loko system $primitives))

(define e 2.7182818284590452353602874713526625)
(define pi 3.1415926535897932384626433832795029)
(define pi/2 1.5707963267948966192313216916397514)
(define pi/4 0.78539816339744830961566084581987572)
(define ln2 0.69314718055994530941723212145817657)
(define ln10 2.3025850929940456840179914546843642)

(define (flonum? obj) (sys:flonum? obj))

;; real->flonum is in arithmetic

(define-syntax define-comparator
  (lambda (x)
    (syntax-case x ()
      ((_ (CMP args ...) body ...)
       #'(define CMP
           (case-lambda
             ((args ...)
              body ...)
             ((a b c)
              (and (CMP a b) (CMP b c)))
             ((a b c d)
              (and (CMP a b) (CMP b c) (CMP c d)))
             ((a b c d . e)
              (and (CMP a b c d)
                   (let lp ((d d) (e e))
                     (or (null? e)
                         (and (CMP d (car e))
                              (lp (car e) (cdr e)))))))))))))

(define-comparator (fl=? a b) (sys:fl=? a b))

(define-comparator (fl<? a b) (sys:fl>? b a))      ;TODO: native

(define-comparator (fl<=? a b) (or (fl<? a b) (fl=? a b)))  ;TODO: native

(define-comparator (fl>? a b) (sys:fl>? a b))

(define-comparator (fl>=? a b) (or (fl>? a b) (fl=? a b)))  ;TODO: native

(define (flinteger? a)
  (and (flfinite? a)
       (fl=? a (fltruncate a))))

(define (flzero? a)
  (fl=? a 0.0))

(define (flpositive? a)
  (fl>? a 0.0))

(define (flnegative? a)
  (fl<? a 0.0))

(define (flodd? a)
  (if (flinteger? a)
      (let ((x (fl/ a 2.0)))
        (not (fl=? x (fltruncate x))))
      (assertion-violation 'flodd? "Expected an integer" a)))

(define (fleven? a)
  (if (flinteger? a)
      (let ((x (fl/ a 2.0)))
        (fl=? x (fltruncate x)))
      (assertion-violation 'fleven? "Expected an integer" a)))

(define (flfinite? a)
  (and (fl=? a a)
       (not (flinfinite? a))))

(define (flinfinite? a)
  (and (fl=? a (fl+ a a))
       (not (fl=? a 0.0))))

(define (flnan? a)
  (not (fl=? a a)))

(define flmax
  (case-lambda
    ((a b)
     ;; Assumes that (sys:flmax a +nan.0) => +nan.0
     (if (flnan? a)
         a
         (sys:flmax a b)))
    ((a)
     (sys:flmax a a))
    ((a b c)
     (sys:flmax (sys:flmax a b) c))
    ((a b c . x*)
     (fold-left flmax (flmax a b c) x*))))

(define flmin
  (case-lambda
    ((a b)
     (if (flnan? a)
         a
         (sys:flmin a b)))
    ((a)
     (sys:flmin a a))
    ((a b c)
     (sys:flmin (sys:flmin a b) c))
    ((a b c . x*)
     (fold-left flmin (flmin a b c) x*))))

(define fl+
  (case-lambda
    ((a b)
     (sys:fl+ a b))
    ((a)
     (sys:fl+ a 0.0))
    ((a b c)
     (sys:fl+ (sys:fl+ a b) c))
    ((a b c . x*)
     (fold-left fl+ (fl+ a b c) x*))))

(define fl*
  (case-lambda
    ((a b)
     (sys:fl* a b))
    ((a)
     (sys:fl* a 1.0))
    ((a b c)
     (sys:fl* (sys:fl* a b) c))
    ((a b c . x*)
     (fold-left fl* (fl* a b c) x*))))

(define fl-
  (case-lambda
    ((a b)
     (sys:fl- a b))
    ((a)
     (sys:fl* a -1.0))
    ((a b c)
     (sys:fl- (sys:fl- a b) c))
    ((a b c . x*)
     (fold-left fl- (fl- a b c) x*))))

(define fl/
  (case-lambda
    ((a b)
     (sys:fl/ a b))
    ((a)
     (sys:fl/ 1.0 a))
    ((a b c)
     (sys:fl/ (sys:fl/ a b) c))
    ((a b c . x*)
     (fold-left fl/ (fl/ a b c) x*))))

(define (flabs a)
  (sys:flabs a))

(define (fldiv-and-mod a b)
  (error 'fldiv-and-mod "TODO: Not yet implemented"))

(define (fldiv a b)
  (let-values (((q _) (fldiv-and-mod a b)))
    q))

(define (flmod a b)
  (let-values (((_ r) (fldiv-and-mod a b)))
    r))

(define (fldiv0-and-mod0 a b)
  (error 'fldiv0-and-mod0 "TODO: Not yet implemented"))

(define (fldiv0 a b)
  (let-values (((q _) (fldiv0-and-mod0 a b)))
    q))

(define (flmod0 a b)
  (let-values (((_ r) (fldiv0-and-mod0 a b)))
    r))

(define (flnumerator a)
  (assert (flonum? a))
  (if (flfinite? a)
      (inexact (numerator (exact a)))
      a))

(define (fldenominator a)
  (assert (flonum? a))
  (if (flfinite? a)
      (inexact (denominator (exact a)))
      (if (flnan? a)
          a
          1.0)))

(define (flfloor fl) (sys:flfloor fl))

(define (flceiling fl) (sys:flceiling fl))

(define (fltruncate fl) (sys:fltruncate fl))

(define (flround fl) (sys:flround fl))

(define (flexp fl)
  (error 'flexp "TODO: Not yet implemented"))

(define fllog
  (case-lambda
    ((a)
     (cond
       ((not (flonum? a))
        (assertion-violation 'fllog "Expected a flonum" a))
       ((fl=? a +inf.0) +inf.0)
       ((fl=? a 0.0) -inf.0)
       ((flnegative? a) +nan.0)
       ((flnan? a) a)
       ((fl=? a 2.0) ln2)
       ((fl=? a 10.0) ln10)
       ((fl<? 0.0 a 2.0)
        (let ((x (fl- a 1.0)))
          (do ((x^n x (fl* x^n x))
               (sign 1.0 (fl- sign))
               (n 1.0 (fl+ n 1.0))
               (ret 0.0 (fl+ ret (fl/ (fl* sign x^n) n))))
              ((fl>=? n 30.0) ret))))
       (else
        (fl+ ln2 (fllog (fl/ a 2.0))))))
    ((a b)
     (fl/ (fllog a) (fllog b)))))

(define (flsin fl)
  (error 'flsin "TODO: Not yet implemented"))

(define (flcos fl)
  (error 'flcos "TODO: Not yet implemented"))

(define (fltan fl)
  (error 'fltan "TODO: Not yet implemented"))

(define (flasin fl)
  (error 'flasin "TODO: Not yet implemented"))

(define (flacos fl)
  (error 'flacos "TODO: Not yet implemented"))

(define flatan
  (case-lambda
    ((a)
     (error 'flatan "TODO: Not yet implemented" a))
    ((a b)
     (error 'flatan "TODO: Not yet implemented" a b))))

(define (flsqrt a)
  (sys:flsqrt a))

(define (flexpt a b)
  (error 'flexpt "TODO: Not yet implemented"))

(define (fixnum->flonum x) (sys:fixnum->flonum x)))
