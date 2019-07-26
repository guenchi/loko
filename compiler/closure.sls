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

;;; Closure conversion

(library (loko compiler closure)
  (export pass-closure)
  (import
    (loko compiler recordize)
    (rename (loko utils) (map-in-order map))
    (except (rnrs) map)
    (only (psyntax compat) gensym))

(define (pass-closure top-level-name x)
  (define who 'pass-closure)
  (define labels '())
  (define (pass x)
    ;; (display x)
    ;; (newline)
    (cond ((bind? x)
           (make-bind (bind-lhs* x)
                      (map pass (bind-rhs* x))
                      (pass (bind-body x))))
          ((fix? x)
           (make-fix (fix-lhs* x)
                     (map pass (fix-rhs* x))
                     (pass (fix-body x))))
          ((proc? x)
           (let ((proc (make-proc (proc-label x)
                                  (proc-end-label x)
                                  (map (lambda (x)
                                         (make-proccase (proccase-info x)
                                                        (pass (proccase-body x))))
                                       (proc-cases x))
                                  (proc-free x)
                                  (proc-name x)
                                  (proc-source x))))
             (set! labels (cons proc labels))
             (make-closure proc (proc-free x))))

          ((seq? x)
           (make-seq (pass (seq-e0 x))
                     (pass (seq-e1 x))))
          ((mutate? x)
           (make-mutate (mutate-name x)
                        (pass (mutate-expr x))))
          ((test? x)
           (make-test (pass (test-expr x))
                      (pass (test-then x))
                      (pass (test-else x))))
          ((funcall? x)
           (let ((op (funcall-operator x))
                 (operands (funcall-operand* x)))
             (make-funcall (pass op)
                           (map pass operands)
                           (funcall-label x)
                           (funcall-source x))))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          ((goto? x) x)
          ((tagbody? x)
           (make-tagbody (tagbody-label x)
                         (pass (tagbody-body x))
                         (tagbody-source x)))
          ((infer? x)
           (make-infer (pass (infer-expr x))
                       (infer-facts x)))
          (else
           (error who "Unknown type" x))))
  (let ((body (pass x)))
    (make-labels top-level-name labels body))))
