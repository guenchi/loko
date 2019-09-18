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

;;; A place for various utilities

;; This should be portable.

(library (loko runtime utils)
  (export
    map-in-order)
  (import
    (rnrs))

(define map-in-order
  (case-lambda
    ((f x*)
     (let lp ((ret '()) (x* x*))
       (if (null? x*)
           (reverse ret)
           (lp (cons (f (car x*)) ret)
               (cdr x*)))))
    ((f x* y*)
     (let lp ((ret '()) (x* x*) (y* y*))
       (if (null? x*)
           (reverse ret)
           (lp (cons (f (car x*) (car y*)) ret)
               (cdr x*) (cdr y*)))))
    ((f x* y* z*)
     (let lp ((ret '()) (x* x*) (y* y*) (z* z*))
       (if (null? x*)
           (reverse ret)
           (lp (cons (f (car x*) (car y*) (car z*)) ret)
               (cdr x*) (cdr y*) (cdr z*)))))
    ((f x* y* . z**)
     (let lp ((ret '()) (x* x*) (y* y*) (z** z**))
       (if (null? x*)
           (reverse ret)
           (lp (cons (apply f (car x*) (car y*) (map car z**)) ret)
               (cdr x*) (cdr y*) (map cdr z**))))))))
