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

;;; Memory-related constants and such

(library (loko arch amd64 memory)
  (export
    stack-area heap-area PAGE-SIZE STACK-SIZE HEAP-SIZE)
  (import
    (rnrs (6)))

(define K 1024)
(define M (* 1024 K))
(define G (* 1024 M))
(define T (* 1024 G))
(define PAGE-SIZE (* 4 K))
(define CODE-TOP (* 4 G))
(define MARK-AND-SWEEP-TOP (* 1 T))
(define STACK-SIZE (* 2 M))
(define HEAP-SIZE (* 16 M))
(define VIRTUAL-ADDRESS-TOP (bitwise-arithmetic-shift-left 1 48))

;; (/ (- VIRTUAL-ADDRESS-TOP MARK-AND-SWEEP-TOP)
;;    (+ HEAP-SIZE STACK-SIZE))

(define (stack-area n)
  (assert (and (integer? n) (not (negative? n))))
  (let ((start (+ MARK-AND-SWEEP-TOP (* n (+ HEAP-SIZE STACK-SIZE)))))
    (when (> (+ start HEAP-SIZE STACK-SIZE) VIRTUAL-ADDRESS-TOP)
      (error 'stack-area "There is no stack area with this number" n))
    start))

(define (heap-area n)
  (assert (and (integer? n) (not (negative? n))))
  (let ((start (+ MARK-AND-SWEEP-TOP (* n (+ HEAP-SIZE STACK-SIZE)) STACK-SIZE)))
    (when (> (+ start HEAP-SIZE STACK-SIZE) VIRTUAL-ADDRESS-TOP)
      (error 'heap-area "There is no heap area with this number" n))
    start)))
