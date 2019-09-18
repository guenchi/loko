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

;;; Free variable analysis

(library (loko compiler freevar)
  (export
    pass-freevar)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map))

(define (pass-freevar x)
  (define who 'pass-freevar)
  ;; TODO: these are a bit dumb, but they are here so there will not
  ;; be a dependency on srfi-1.
  (define (append-map f x) (apply append (map f x)))
  (define (delete-duplicates x)
    (cond ((null? x)
           '())
          ((memq (car x) (cdr x))
           (delete-duplicates (cdr x)))
          (else
           (cons (car x) (delete-duplicates (cdr x))))))

  (define (find x to-find)
    (define (find x)
      (cond ((ref? x)
             (if (memq (ref-name x) to-find) (list (ref-name x)) '()))
            ((bind? x)
             (append (append-map find (bind-rhs* x))
                     (find (bind-body x))))
            ((fix? x)
             (append (append-map find (fix-rhs* x))
                     (find (fix-body x))))
            ((proc? x)
             (append-map (lambda (x) (find (proccase-body x)))
                         (proc-cases x)))
            ((seq? x)
             (append (find (seq-e0 x))
                     (find (seq-e1 x))))
            ((test? x)
             (append (find (test-expr x))
                     (find (test-then x))
                     (find (test-else x))))
            ((funcall? x)
             (append (find (funcall-operator x))
                     (append-map find (funcall-operand* x))))
            ((const? x) '())
            ((primref? x) '())
            (else
             (error who "Unknown type" x))))
    (find x))
  ;; XXX: perhaps this pass could just as well mutate existing
  ;; procs?
  (define (pass x env)
    ;; (write x) (display " - ") (write env)
    ;; (newline)
    (cond ((bind? x)
           (make-bind (bind-lhs* x)
                      (map (lambda (x) (pass x env)) (bind-rhs* x))
                      (pass (bind-body x)
                            (append (bind-lhs* x) env))))
          ((fix? x)
           (let ((env (append (fix-lhs* x) env)))
             (make-fix (fix-lhs* x)
                       (map (lambda (x) (pass x env)) (fix-rhs* x))
                       (pass (fix-body x) env))))
          ((proc? x)
           (let* ((free '())
                  (cases
                   (map (lambda (x)
                          (let* ((new-env (append (caseinfo-formals
                                                   (proccase-info x))
                                                  env))
                                 (new-body (pass (proccase-body x) new-env))
                                 (new-free (find (proccase-body x) env)))
                            (set! free (append new-free free))
                            (make-proccase (proccase-info x) new-body)))
                        (proc-cases x))))
             (make-proc (proc-label x)
                        (proc-end-label x)
                        cases
                        (delete-duplicates free)
                        (proc-name x)
                        (proc-source x))))
          ((seq? x)
           (make-seq (pass (seq-e0 x) env)
                     (pass (seq-e1 x) env)))
          ((test? x)
           (make-test (pass (test-expr x) env)
                      (pass (test-then x) env)
                      (pass (test-else x) env)))
          ((funcall? x)
           (let ((op (funcall-operator x))
                 (operands (funcall-operand* x)))
             (make-funcall (pass op env)
                           (map (lambda (x) (pass x env)) operands)
                           (funcall-label x)
                           (funcall-source x))))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          (else
           (error who "Unknown type" x))))
  (pass x '())))
