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

(library (loko compiler)
  (export
    compiler-passes assemble-text-file
    (rename (record->sexpr compiler-code->sexpr)))
  (import
    (loko arch asm)
    (loko config)
    (loko compiler recordize)
    (loko compiler let)
    (loko compiler letrec)
    (loko compiler cp0)
    (loko compiler mutation)
    (loko compiler freevar)
    (loko compiler loops)
    (loko compiler infer)
    (loko compiler closure)
    (loko compiler optimize)
    (only (psyntax compat) pretty-print)
    (rnrs))

(define (assemble-text-file filename code primlocs)
  (define (print . x) (for-each display x) (newline))
  (define (generate-assembler code)
    ;; Returns text and data.
    (print "Generating code...")
    ;; (for-each (lambda (x) (pretty-print x) (newline)) (map record->sexpr code)) (newline)
    (let-values ([(text data) (code-generator (config-target-cpu) code primlocs
                                              'make-init-code)])
      (print "Optimizing code...")
      (cond ((instruction-analyzer (config-target-cpu))
             => (lambda (a) (pass-optimize text data a
                                           (target-convention (config-target-cpu)))))
            (else
             ;; No instruction analyzer? No optimizations for you.
             (values text data)))))
  (let*-values ([(text data) (generate-assembler code)]
                [(text data) (generate-tables (config-target-cpu) text data)])
    (print "Assembling code...")
    ;; (for-each (lambda (x) (write x) (newline)) text) (newline)
    (call-with-port (open-file-output-port filename (file-options no-fail))
      (lambda (p)
        (let ((asm (assembler-library (config-target-cpu)
                                      (config-target-kernel)
                                      text data)))
          ;; (for-each (lambda (x) (write x) (newline)) asm)
          (let-values ([(binary symbols)
                        (assemble (config-target-cpu) asm)])
            (print "Writing " filename)
            (put-bytevector p binary)
            (display "Done.\n")
            #;
            (let-values (((syms addrs) (hashtable-entries symbols)))
              (display "Symbol table:\n")
              (vector-for-each
               (lambda (label addr)
                 (print (number->string addr 16) " - " label))
               syms addrs)
              (newline))))))))

;; The input to compiler-passes comes from psyntax. The output goes
;; to a code generator.
(define (compiler-passes name code)
  (define (progress step)
    ;; (display (list step) (current-error-port))
    #f)
  (let* ((_ (progress 'recordize))
         (c (pass-recordize name code))
         (_ (progress 'let))
         (c (pass-let c))
         (_ (progress 'letrec))
         (c (pass-letrec c))
         (_ (progress 'cp0))
         (c (pass-cp0 c))
         (_ (progress 'let))
         (c (pass-let c))
         (_ (progress 'mutation))
         (c (pass-mutation c))
         (_ (progress 'freevar))
         (c (pass-freevar c))
         (_ (progress 'loops))
         (c (pass-loops c))
         (_ (progress 'infer))
         (c (pass-infer c))
         (_ (progress 'closure))
         ;; (_ (pretty-print (record->sexpr c)))
         (c (pass-closure name c)))
    c)))
