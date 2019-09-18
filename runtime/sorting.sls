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

;;; List and vector sorting

;; TODO: "If multiple returns occur from list-sort or vector-sort, the
;; return values returned by earlier returns are not mutated."

(library (loko runtime sorting)
  (export list-sort vector-sort vector-sort!)
  (import (except (rnrs)
                  list-sort vector-sort vector-sort!)
          (rnrs mutable-pairs)
          (prefix (rnrs) sys:))

(define (vector-swap! v i1 i2)
  (let ((t1 (vector-ref v i1))
        (t2 (vector-ref v i2)))
    (vector-set! v i1 t2)
    (vector-set! v i2 t1)))

;; TODO: Merge sort?
(define (vector-sort less? v)
  (list->vector (list-sort less? (vector->list v))))

;; Quicksort. TODO: heap sort and/or introsort.
(define (partition! less? v left right pivot-idx)
  (let ((pivot-value (vector-ref v pivot-idx)))
    (vector-swap! v pivot-idx right)
    (let lp ((i left) (store-idx left))
      (cond ((fx=? i right)
             (vector-swap! v store-idx right)
             store-idx)
            ((less? (vector-ref v i) pivot-value)
             (vector-swap! v i store-idx)
             (lp (fx+ i 1) (fx+ store-idx 1)))
            (else
             (lp (fx+ i 1) store-idx))))))

(define (vector-sort! less? v)
  (let quicksort ((left 0)
                  (right (fx- (vector-length v) 1)))
    (when (fx<? left right)
      ;; TODO: random pivot element.
      (let ((pivot-idx (partition! less? v left right
                                   (div (+ left right) 2))))
        (quicksort left (fx- pivot-idx 1))
        (quicksort (fx+ pivot-idx 1) right)))))

(define (list-sort less? seq)
  ;; "sort.scm" from SLIB
  ;; Author: Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
  ;; This code is in the public domain.
  (define (sort:merge! a b less?)
    (define (key x) x)
    (define (loop r a kcara b kcarb)
      (cond ((less? kcarb kcara)
             (set-cdr! r b)
             (if (null? (cdr b))
                 (set-cdr! b a)
                 (loop b a kcara (cdr b) (key (cadr b)))))
            (else                     ; (car a) <= (car b)
             (set-cdr! r a)
             (if (null? (cdr a))
                 (set-cdr! a b)
                 (loop a (cdr a) (key (cadr a)) b kcarb)))))
    (cond ((null? a) b)
          ((null? b) a)
          (else
           (let ((kcara (key (car a)))
                 (kcarb (key (car b))))
             (cond
               ((less? kcarb kcara)
                (if (null? (cdr b))
                    (set-cdr! b a)
                    (loop b a kcara (cdr b) (key (cadr b))))
                b)
               (else			; (car a) <= (car b)
                (if (null? (cdr a))
                    (set-cdr! a b)
                    (loop a (cdr a) (key (cadr a)) b kcarb))
                a))))))

  ;; takes two sorted lists a and b and smashes their cdr fields to form a
  ;; single sorted list including the elements of both.
  ;; Note:  this does _not_ accept arrays.
  (define (merge! a b less?)
    (sort:merge! a b less?))

  (define (sort:sort-list! seq less?)
    (define keyer (lambda (x) x))
    (define (step n)
      (cond ((> n 2) (let* ((j (div n 2))
                            (a (step j))
                            (k (- n j))
                            (b (step k)))
                       (merge! a b less?)))
            ((= n 2) (let ((x (car seq))
                           (y (cadr seq))
                           (p seq))
                       (set! seq (cddr seq))
                       (cond ((less? (keyer y) (keyer x))
                              (set-car! p y)
                              (set-car! (cdr p) x)))
                       (set-cdr! (cdr p) '())
                       p))
            ((= n 1) (let ((p seq))
                       (set! seq (cdr seq))
                       (set-cdr! p '())
                       p))
            (else '())))
    (step (length seq)))

  (define (list-sort! less? seq)
    (let ((ret (sort:sort-list! seq less?)))
      (if (not (eq? ret seq))
          (do ((crt ret (cdr crt)))
              ((eq? (cdr crt) seq)
               (set-cdr! crt ret)
               (let ((scar (car seq)) (scdr (cdr seq)))
                 (set-car! seq (car ret)) (set-cdr! seq (cdr ret))
                 (set-car! ret scar) (set-cdr! ret scdr)))))
      seq))

  (list-sort! less? (append seq '()))))
