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

;;; Compat lib for bootstrapping from Chez Scheme

(library (loko compat)
  (export gensym? gensym->unique-string gensym-prefix call/1cc)
  (import
    (rnrs arithmetic bitwise)
    (except (rnrs base) map)
    (rnrs bytevectors)
    (rnrs control)
    (rnrs hashtables)
    (rnrs programs)
    (rnrs io simple)
    (rnrs io ports)
    (rnrs lists)
    (rnrs files)
    (rnrs unicode)
    (rename (only (chezscheme) gensym? gensym->unique-string gensym-prefix call/1cc)
            (gensym->unique-string chez:gensym->unique-string))
    (loko config)
    (rename (loko utils) (map-in-order map))
    (psyntax expander)
    (psyntax internal)
    (psyntax compat))

;; For reproducible builds
(define (gensym->unique-string s)
  (let ((p (open-string-input-port (chez:gensym->unique-string s))))
    (let lp ()
      (unless (eqv? (get-char p) #\-)
        (lp)))
    (string-append "bs-" (get-string-all p))))

(let ()
  ;; This defines the primitives needed to run all the macros in the
  ;; bootstrap.
  (define-syntax define-prims
    (syntax-rules ()
      ((_ name* ...)
       (let ((g* (map (lambda (x) (gensym)) '(name* ...)))
             (v* (list name* ...)))
         (for-each set-symbol-value! g* v*)
         (let ((alist (map cons '(name* ...) g*)))
           (current-primitive-locations
            (lambda (x)
              (cond
                ((assq x alist) => cdr)
                (else (error 'expand-files "Undefined prim" x))))))))))
  (define-prims
      syntax-dispatch apply cons append map list for-each syntax-error
      generate-temporaries = + - * mod datum->syntax string->symbol
      string-append symbol->string syntax->datum gensym length
      open-string-output-port identifier? free-identifier=? exists
      values call-with-values for-all ellipsis-map assertion-violation
      assertion-error null? car cdr pair? bound-identifier=? vector
      not eq? eqv? reverse list->vector memq memv
      syntax-violation string->utf8 string? integer?
      char->integer char-whitespace? char<=? string-ref string-length bitwise-and
      make-variable-transformer
      void
      config-target-kernel
      make-eq-hashtable hashtable-ref hashtable-set! hashtable-delete!)))
