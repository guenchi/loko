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

;;; Parameters

(library (loko runtime parameters)
  (export
    ;;parameterize   ; defined in (psyntax expander)
    make-parameter)
  (import
    (only (rnrs) define case-lambda set! unless procedure?
          assertion-violation quote let))

(define make-parameter
  (case-lambda
    ((x)
     (case-lambda
       (() x)
       ((v) (set! x v))))
    ((x fender)
     (unless (procedure? fender)
       (assertion-violation 'make-parameter "Expected a procedure" x fender))
     (let ((x (fender x)))
       (case-lambda
         (() x)
         ((v) (set! x (fender v)))))))))
