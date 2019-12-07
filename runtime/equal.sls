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

;;; Equality

;; The CASE syntax doesn't work until eqv? is defined.

(library (loko runtime equal)
  (export
    eq? eqv? equal?
    equal-hash)
  (import
    (except (rnrs) eq? eqv? equal? equal-hash)
    (prefix (rnrs) sys:)
    (rnrs mutable-pairs)
    (only (loko runtime arithmetic) int? ratnum? compnum?))

(define (eq? x y) (sys:eq? x y))

(define (flsign n)
  (define (sign bits)
    (fxbit-field bits 31 32))
  (let ((bits (let ((bv (make-bytevector 4)))
                (bytevector-ieee-single-native-set! bv 0 n)
                (bytevector-u32-native-ref bv 0))))
    (sign bits)))

(define (eqv? x y)
  (cond ((eq? x y))
        ;; Exact numbers
        ((fixnum? x) (and (fixnum? y) (fx=? x y)))
        ((int? x) (and (int? y) (= x y)))
        ;; Sometimes exact numbers
        ((ratnum? x) (and (ratnum? y) (eq? (exact? x) (exact? y)) (= x y)))
        ((compnum? x) (and (compnum? y) (eq? (exact? x) (exact? y)) (= x y)))
        ;; Inexact numbers
        ((flonum? x)
         (and (flonum? y)
              (fx=? (flsign x) (flsign y))
              (fl=? x y)))
        (else #f)))

(define (make-xorshift32 seed)
  ;; http://www.jstatsoft.org/v08/i14/paper
  (let ((state seed))
    (lambda ()
      (let* ((y state)
             (y (fxxor y (fxarithmetic-shift y 13)))
             (y (fxxor y (fxarithmetic-shift y -17)))
             (y (fxxor y (fxarithmetic-shift y 5)))
             (y (fxand y #xffffffff)))
        (set! state y)
        y))))

;; This code is from the paper "Efficient Nondestructive Equality
;; Checking for Trees and Graphs" by Michael D. Adams and R. Kent
;; Dybvig (ICFP '08). It could call r5rs-equal? directly if there were
;; no cyclic structures on the lexical level, no set-cxr! and no
;; vector-set! in the whole program.

(define (equal? x y)
  (define xorshift32 (make-xorshift32 2463534242))
  (define (random x) (fxmod (xorshift32) x))
  (define box? pair?)
  (define box list)
  (define unbox car)
  (define set-box! set-car!)

  (define (union-find ht x y)
    (define (find b)
      (let ([n (unbox b)])
        (if (box? n)
            (let loop ([b b] [n n])
              (let ([nn (unbox n)])
                (if (box? nn)
                    (begin
                      (set-box! b nn)
                      (loop n nn))
                    n)))
            b)))
    (let ([bx (hashtable-ref ht x #f)]
          [by (hashtable-ref ht y #f)])
      (if (not bx)
          (if (not by)
              (let ([b (box 1)])
                (hashtable-set! ht x b)
                (hashtable-set! ht y b)
                #f)
              (let ([ry (find by)])
                (hashtable-set! ht x ry)
                #f))
          (if (not by)
              (let ([rx (find bx)])
                (hashtable-set! ht y rx)
                #f)
              (let ([rx (find bx)] [ry (find by)])
                (or (eq? rx ry)
                    (let ([nx (unbox rx)]
                          [ny (unbox ry)])
                      (if (fx>? nx ny)
                          (begin
                            (set-box! ry rx)
                            (set-box! rx (fx+ nx ny))
                            #f)
                          (begin
                            (set-box! rx ry)
                            (set-box! ry (fx+ ny nx))
                            #f)))))))))

  (define (interleave? x y k)
    (let ([ht (make-eq-hashtable)])
      (define (call-union-find x y)
        (union-find ht x y))
      (define (e? x y k)
        (if (fx<=? k 0)
            (if (fx=? k kb)
                (fast? x y (random (fx* 2 k0)))
                (slow? x y k))
            (fast? x y k)))
      (define (slow? x y k)
        (cond
          [(eq? x y) k]
          [(pair? x)
           (and (pair? y)
                (if (call-union-find x y)
                    0
                    (let ([k (e? (car x) (car y) (fx- k 1))])
                      (and k (e? (cdr x) (cdr y) k)))))]
          [(vector? x)
           (and (vector? y)
                (let ([n (vector-length x)])
                  (and (fx=? (vector-length y) n)
                       (if (call-union-find x y)
                           0
                           (let f ([i 0] [k (fx- k 1)])
                             (if (fx=? i n)
                                 k
                                 (let ([k (e? (vector-ref x i)
                                              (vector-ref y i) k)])
                                   (and k (f (fx+ i 1) k)))))))))]
          [(string? x) (and (string? y) (string=? x y) k)]
          [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
          [else (and (eqv? x y) k)]))
      (define (fast? x y k)
        (let ([k (fx- k 1)])
          (cond
            [(eq? x y) k]
            [(pair? x)
             (and (pair? y)
                  (let ([k (e? (car x) (car y) k)])
                    (and k (e? (cdr x) (cdr y) k))))]
            [(vector? x)
             (and (vector? y)
                  (let ([n (vector-length x)])
                    (and (fx=? (vector-length y) n)
                         (let f ([i 0] [k k])
                           (if (fx=? i n)
                               k
                               (let ([k (e? (vector-ref x i)
                                            (vector-ref y i) k)])
                                 (and k (f (fx+ i 1) k))))))))]
            [(string? x) (and (string? y) (string=? x y) k)]
            [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
            [else (and (eqv? x y) k)])))
      (and (e? x y k) #t)))

  (define (pre? x y k)
    (cond
      [(eq? x y) k]
      [(pair? x)
       (and (pair? y)
            (if (fx<=? k 0)
                k
                (let ([k (pre? (car x) (car y) (fx- k 1))])
                  (and k (pre? (cdr x) (cdr y) k)))))]
      [(vector? x)
       (and (vector? y)
            (let ([n (vector-length x)])
              (and (fx=? (vector-length y) n)
                   (let f ([i 0] [k k])
                     (if (or (fx=? i n) (fx<=? k 0))
                         k
                         (let ([k (pre? (vector-ref x i)
                                        (vector-ref y i)
                                        (fx- k 1))])
                           (and k (f (fx+ i 1) k))))))))]
      [(string? x)
       (and (string? y) (string=? x y) k)]
      [(bytevector? x)
       (and (bytevector? y) (bytevector=? x y) k)]
      [else (and (eqv? x y) k)]))

  (define (r5rs-equal? x y)
    (cond ((eq? x y))
          ((pair? x)
           (and (pair? y)
                (equal? (car x) (car y))
                (equal? (cdr x) (cdr y))))
          ((vector? x)
           (and (vector? y)
                (fx=? (vector-length x)
                      (vector-length y))
                (let lp ((i 0))
                  (or (fx=? (vector-length x) i)
                      (and (equal? (vector-ref x i)
                                   (vector-ref y i))
                           (lp (fx+ i 1)))))))
          ((string? x)
           (and (string? y) (string=? x y)))
          ((bytevector? x)
           (and (bytevector? y) (bytevector=? x y)))
          (else (eqv? x y))))

  (define k0 400)
  (define kb -40)

  (let ([k (pre? x y k0)])
    (and k (or (fx>? k 0) (interleave? x y 0)))))

(define (equal-hash x)
  (cond
    ((fixnum? x) x)
    ((char? x) (char->integer x))
    ((symbol? x) (symbol-hash x))
    ((string? x) (string-hash x))
    (else
     ;; TODO: Do something better. Much better.
     (string-hash (call-with-string-output-port
                    (lambda (p)
                      (write x p))))))))
