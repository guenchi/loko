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

;;; Vector primitives

(library (loko runtime vectors)
  (export
    vector? make-vector vector vector-length vector-ref
    vector-set! vector->list list->vector vector-fill!
    vector-map vector-for-each)
  (import
    (except (rnrs)
            vector? make-vector vector vector-length vector-ref
            vector-set! vector->list list->vector vector-fill!
            vector-map vector-for-each)
    (prefix (only (rnrs) vector? vector-length vector-ref vector-set!)
            sys:)
    (loko system $primitives))

(define (vector? x) (sys:vector? x))

(define make-vector
  (case-lambda
    ((n)
     (make-vector n 0))
    ((n fill)
     (if (eqv? n 0)
         '#()
         (let ((v ($make-vector n)))
           (unless (eqv? fill 0)
             (vector-fill! v fill))
           v)))))

(define vector
  (case-lambda
    (() '#())
    ((a)
     (let ((v ($make-vector 1)))
       (vector-set! v 0 a)
       v))
    ((a b)
     (let ((v ($make-vector 2)))
       (vector-set! v 0 a)
       (vector-set! v 1 b)
       v))
    (x
     (list->vector x))))

(define (vector-length x) (sys:vector-length x))

(define (vector-ref v i) (sys:vector-ref v i))

(define (vector-set! v i o) (sys:vector-set! v i o))

(define (vector->list v)
  (do ((i (fx- (vector-length v) 1) (fx- i 1))
       (ret '() (cons (vector-ref v i) ret)))
      ((fx=? i -1) ret)))

(define (list->vector list)
  ;; XXX: finds cycles using length
  (let* ((len (length list))
         (v (make-vector len)))
    (do ((i 0 (fx+ i 1))
         (list list (cdr list)))
        ((fx=? i len) v)
      (vector-set! v i (car list)))))

(define (vector-fill! v fill)
  (do ((i 0 (fx+ i 1)))
      ((fx=? i (vector-length v)))
    (vector-set! v i fill)))

(define vector-map
  (case-lambda
    ((proc v)
     ;; TODO: multiple returns of proc
     (let ((ret (make-vector (vector-length v))))
       (do ((i 0 (fx+ i 1)))
           ((fx=? i (vector-length ret)) ret)
         (vector-set! ret i (proc (vector-ref v i))))))
    ((proc v1 v2)
     (assert (fx=? (vector-length v1) (vector-length v2)))
     (do ((ret (make-vector (vector-length v1)))
          (i 0 (fx+ i 1)))
         ((fx=? i (vector-length ret)) ret)
       (vector-set! ret i (proc (vector-ref v1 i) (vector-ref v2 i)))))
    ((proc v0 . v*)
     (for-each (lambda (v)
                 (unless (fx=? (vector-length v0)
                               (vector-length v))
                   (apply assertion-violation 'vector-map
                          "Expected vectors of the same length"
                          v0 v*)))
               v*)
     (do ((ret (make-vector (vector-length v0)))
          (i 0 (fx+ i 1)))
         ((fx=? i (vector-length ret)) ret)
       (vector-set! ret i
                    (apply proc (vector-ref v0 i)
                           (map (lambda (v) (vector-ref v i)) v*)))))))

(define vector-for-each
  (case-lambda
    ((proc v)
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (vector-length v)))
       (proc (vector-ref v i))))
    ((proc v1 v2)
     (assert (fx=? (vector-length v1) (vector-length v2)))
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (vector-length v1)))
       (proc (vector-ref v1 i)
             (vector-ref v2 i))))
    ;; TODO: Proc is always called in the same dynamic environment as
    ;; vector-for-each itself.
    ((proc v0 . v*)
     (for-each (lambda (v)
                 (unless (fx=? (vector-length v0)
                               (vector-length v))
                   (apply assertion-violation 'vector-for-each
                          "Expected vectors of the same length"
                          v0 v*)))
               v*)
     (do ((i 0 (fx+ i 1)))
         ((fx=? i (vector-length v0)))
       (apply proc (vector-ref v0 i)
              (map (lambda (v) (vector-ref v i)) v*)))))))
