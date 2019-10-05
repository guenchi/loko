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

;;; Wrapper around the pluggable architecture-dependent assembler libraries

(library (loko arch asm)
  (export
    register-target
    assembler-library
    assemble
    code-generator
    instruction-analyzer
    target-convention
    generate-tables)
  (import
    (rnrs (6)))

(define (notice)
  '((%utf8z "Built with Loko Scheme <https://scheme.fail/>")))

(define-record-type target
  (fields cpu-id kernel-id
          assembler
          code-generator
          instruction-analyzer
          convention-proc
          table-generator
          assembler-library-proc))

;; Target registry
(define targets
  '())

(define (register-target cpu-id kernel-id assembler code-generator instruction-analyzer
                         convention-proc table-generator assembler-library-proc)
  (set! targets
        (cons (make-target cpu-id kernel-id
                           assembler code-generator instruction-analyzer
                           convention-proc table-generator assembler-library-proc)
              targets)))

(define find-target
  (case-lambda
    ((cpu-id)
     (find (lambda (target) (eq? (target-cpu-id target) cpu-id))
           targets))
    ((cpu-id kernel-id)
     (find (lambda (target)
             (and (eq? (target-cpu-id target) cpu-id)
                  (eq? (target-kernel-id target) kernel-id)))
           targets))))

;; Returns a bytevector of assembled code and a hashtable with symbols.
(define (assemble cpu-id code)
  (cond ((find-target cpu-id) =>
         (lambda (target)
           ((target-assembler target) code)))
        (else
         (assertion-violation 'assemble "Unsupported CPU architecture" cpu-id))))

;; Takes records from (loko compiler recordize) and returns a list of
;; instructions.
(define (code-generator cpu-id code primlocs make-init-code?)
  (cond ((find-target cpu-id) =>
         (lambda (target)
           ((target-code-generator target) code primlocs make-init-code?)))
        (else
         (assertion-violation 'code-generator "Unsupported CPU architecture" cpu-id))))

;; Returns a procedure that performs instruction analysis for the
;; given target.
(define (instruction-analyzer cpu-id)
  (cond ((find-target cpu-id) => target-instruction-analyzer)
        (else #f)))

;; Returns a procedure that can answer questions about the target
;; architecture.
(define (target-convention cpu-id)
  (cond ((find-target cpu-id) => target-convention-proc)
        (else
         (assertion-violation 'target-convention
                              "Unsupported CPU architecture" cpu-id))))

;; If the architecture has need of some particular tables, e.g.
;; stack unwinding tables, these can be generated here.
(define (generate-tables cpu-id kernel-id text data)
  (cond ((find-target cpu-id kernel-id) =>
         (lambda (target)
           ((target-table-generator target) text data)))
        (else
         (values text data))))

;; Returns assembler code for the Loko runtime. This code starts
;; with a header (e.g. ELF or multiboot), contains a start label
;; that calls scheme-start, and then exits (or reboots). The
;; standard procedures are also defined here.
(define (assembler-library cpu-id kernel-id text data)
  (cond ((find-target cpu-id kernel-id) =>
         (lambda (target)
           ((target-assembler-library-proc target) cpu-id kernel-id (notice) text data)))
        (else
         (assertion-violation 'assembler-library
                              "Unsupported target" cpu-id kernel-id)))))
