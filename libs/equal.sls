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

(library (loko libs equal)
  (export
    eq? eqv? equal?
    equal-hash)
  (import
    (only (loko system $host) $void? #;eq-hash)
    (except (rnrs) eq? eqv? equal? equal-hash)
    (prefix (rnrs) sys:))

(define (eq? x y) (sys:eq? x y))

(define (eqv? x y)
  (cond ((eq? x y))
        ((fixnum? x) (and (fixnum? y) (fx=? x y)))
        ;; TODO: matching exactness when doing flonums
        ((number? x) (and (number? y) (= x y)))
        ;; XXX: perhaps this is dumb. the only place that really
        ;; needs to check for voids, but that might not have access
        ;; to $void?, is the object serializer.
        (($void? x) ($void? y))
        (else #f)))

(define (equal? x y)
  ;; FIXME: This doesn't handle cycles correctly. See Dybvig et al.
  (cond ((pair? x)
         (and (pair? y)
              (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((number? x)
         (and (number? y) (= x y)))
        ((string? x)
         (and (string? y) (string=? x y)))
        ((bytevector? x)
         (and (bytevector? y) (bytevector=? x y)))
        ((vector? x)
         (and (vector? y)
              (fx=? (vector-length x)
                    (vector-length y))
              (let lp ((i 0))
                (or (fx=? (vector-length x) i)
                    (and (equal? (vector-ref x i)
                                 (vector-ref y i))
                         (lp (fx+ i 1)))))))
        (else
         (eq? x y))))

(define (equal-hash x)
  ;; TODO: Do something better. Much better.
  (string-hash (call-with-string-output-port
                 (lambda (p)
                   (write x p))))))
