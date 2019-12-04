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

;;; Enumeration sets

(library (loko runtime enums)
  (export
    enum-set?
    make-enumeration
    enum-set-universe enum-set-indexer enum-set-constructor
    enum-set->list enum-set-member? enum-set-subset?
    enum-set=? enum-set-union enum-set-intersection
    enum-set-difference enum-set-complement
    enum-set-projection)
  (import
    (rnrs arithmetic bitwise)
    (rnrs arithmetic fixnums)
    (rnrs base)
    (rnrs control)
    (rnrs hashtables)
    (rnrs lists)
    (rnrs records syntactic))

(define-record-type enum-type
  (opaque #t) (sealed #t)
  (fields symbols ht))

(define-record-type enum-set
  (fields type v))

(define (make-enumeration symbol-list)
  (unless (for-all symbol? symbol-list)
    (assertion-violation 'make-enumeration
                         "Expected a list of symbols" symbol-list))
  (let ((ht (make-eq-hashtable)))
    (do ((s* symbol-list (cdr s*))
         (i 0 (fx+ i 1)))
        ((null? s*)
         (let ((t (make-enum-type (apply list symbol-list) ht)))
           (make-enum-set t (- (expt 2 i) 1))))
      (hashtable-set! ht (car s*) i))))

(define (enum-set-universe enum-set)
  (let* ((t (enum-set-type enum-set))
         (s* (enum-type-symbols t)))
    (make-enum-set t (- (expt 2 (length s*)) 1))))

(define (enum-set-indexer enum-set)
  (let ((ht (enum-type-ht (enum-set-type enum-set))))
    (lambda (s) (hashtable-ref ht s #f))))

(define (enum-set-constructor enum-set)
  (let* ((t (enum-set-type enum-set))
         (ht (enum-type-ht t)))
    (lambda (s*)
      (unless (for-all symbol? s*)
        (assertion-violation #f "Expected a list of symbols" s*))
      (do ((s* s* (cdr s*))
           (v 0 (cond ((hashtable-ref ht (car s*) #f)
                       => (lambda (b) (bitwise-ior v (expt 2 b))))
                      (else
                       (assertion-violation #f "Expected symbols in the enum-set universe"
                                            (car s*) (apply list (enum-type-symbols t)))))))
          ((null? s*)
           (make-enum-set t v))))))

(define (enum-set->list enum-set)
  (let ((t (enum-set-type enum-set))
        (v (enum-set-v enum-set)))
    (do ((s* (enum-type-symbols t) (cdr s*))
         (b 1 (bitwise-arithmetic-shift-left b 1))
         (ret '() (if (eqv? 0 (bitwise-and b v))
                      ret
                      (cons (car s*) ret))))
        ((null? s*) (reverse ret)))))

(define (enum-set-member? symbol enum-set)
  (let ((t (enum-set-type enum-set))
        (v (enum-set-v enum-set)))
    (let ((ht (enum-type-ht t)))
      (cond ((hashtable-ref ht symbol #f) =>
             (lambda (b) (bitwise-bit-set? v b)))
            (else #f)))))

(define (enum-set-subset? enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (eqv? 0 (bitwise-and v1 (bitwise-not v2)))
        (and (for-all (lambda (sym) (memq sym (enum-type-symbols t2)))
                      (enum-type-symbols t1))
             (for-all (lambda (sym) (enum-set-member? sym enum-set2))
                      (enum-set->list enum-set1))))))

(define (enum-set=? enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (= v1 v2)
        ;; lazy
        (and (enum-set-subset? enum-set1 enum-set2)
             (enum-set-subset? enum-set2 enum-set1)))))

(define (enum-set-union enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (make-enum-set t1 (bitwise-ior v1 v2))
        (error 'enum-set-union "Expected enum-sets of the same type"
               enum-set1 enum-set2))))

(define (enum-set-intersection enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (make-enum-set t1 (bitwise-and v1 v2))
        (error 'enum-set-intersection "Expected enum-sets of the same type"
               enum-set1 enum-set2))))

(define (enum-set-difference enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2))
        (v2 (enum-set-v enum-set2)))
    (if (eq? t1 t2)
        (make-enum-set t1 (bitwise-and v1 (bitwise-not v2)))
        (error 'enum-set-difference "Expected enum-sets of the same type"
               enum-set1 enum-set2))))

(define (enum-set-complement enum-set)
  (let ((t (enum-set-type enum-set))
        (v (enum-set-v enum-set)))
    (let ((s* (enum-type-symbols t)))
      (make-enum-set t (bitwise-xor v (- (expt 2 (length s*)) 1))))))

(define (enum-set-projection enum-set1 enum-set2)
  (let ((t1 (enum-set-type enum-set1))
        (v1 (enum-set-v enum-set1))
        (t2 (enum-set-type enum-set2)))
    (if (eq? t1 t2)
        enum-set1
        (let ((ht (enum-type-ht t2)))
          (do ((s* (enum-type-symbols t1) (cdr s*))
               (b 1 (bitwise-arithmetic-shift-left b 1))
               (v 0 (cond ((eqv? 0 (bitwise-and v1 b)) v)
                          ((hashtable-ref ht (car s*) #f)
                           => (lambda (b) (bitwise-ior v (expt 2 b))))
                          (else v))))
              ((null? s*)
               (make-enum-set t2 v))))))))
