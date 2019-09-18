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

;;; Booleans

(library (loko runtime booleans)
  (export
    not boolean? boolean=?)
  (import
    (only (rnrs (6))
          define case-lambda let cond else and unless quote
          eq? car cdr null? assertion-violation apply)
    (prefix (only (rnrs) not boolean?)
            sys:))

(define (not x) (sys:not x))

(define (boolean? x) (sys:boolean? x))

(define boolean=?
  (case-lambda
    ((x y)
     (unless (and (boolean? x) (boolean? y))
       (assertion-violation 'boolean=? "Expected booleans" x y))
     (eq? x y))
    ((x y . rest*)
     (unless (and (boolean? x) (boolean? y))
       (apply assertion-violation 'boolean=? "Expected booleans" x y rest*))
     (let lp ((x* rest*) (ret (eq? x y)))
       (cond ((null? x*) ret)
             (else
              (unless (boolean? (car x*))
                (apply assertion-violation 'boolean=? "Expected booleans" x y rest*))
              (lp (cdr x*) (and ret (eq? x (car x*)))))))))))
