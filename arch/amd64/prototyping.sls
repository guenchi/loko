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

;;; Stuff for prototyping on the amd64 port (in the REPL).

;; This is maybe temporary until the compiler is up and running.

(library (loko arch amd64 prototyping)
  (export
    cpuid rdtsc

    $bytevector-location
    $switch-stack
    $processor-data-ref $processor-data-set!
    $object->fixnum

    $make-box
    $box-ref
    $box-set!)
  (import
    (rnrs)
    (prefix (loko system $asm-amd64) sys:)
    (prefix (loko system $bytevectors) sys:)
    (prefix (loko system $host) sys:)
    (prefix (loko system $boxes) sys:))

(define cpuid
  (case-lambda
    ((eax)
     (cpuid eax 0))
    ((eax ecx)
     (let ((ret (make-vector 4)))
       (sys:$cpuid! eax ecx ret)
       (values (vector-ref ret 0)
               (vector-ref ret 1)
               (vector-ref ret 2)
               (vector-ref ret 3))))))

(define (rdtsc)
  (sys:rdtsc))

;; maybe not strictly part of this library...?
(define ($bytevector-location bv)
  (bytevector-length bv)
  (sys:$bytevector-location bv))

;; this should absolutely not be here
(define ($switch-stack rsp ret)
  (sys:$switch-stack rsp ret))
(define ($processor-data-ref idx)
  (sys:$processor-data-ref idx))
(define ($processor-data-set! idx v)
  (sys:$processor-data-set! idx v))

(define ($object->fixnum x)
  (sys:$object->fixnum x))

;; The boxes!! Should probably not be here either.
(define ($make-box type len) (sys:$make-box type len))

(define ($box-ref v i) (sys:$box-ref v i))

(define ($box-set! v i x) (sys:$box-set! v i x)))
