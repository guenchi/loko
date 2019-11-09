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

;;; Compilation with static linking

(library (loko compiler static)
  (export
    compile-program)
  (import
    (rnrs (6))
    (srfi :98 os-environment-variables)
    (loko runtime utils)
    (psyntax library-manager)
    (loko compiler expander)
    (loko compiler main)
    (loko config)
    (only (loko compiler cp0) cp0-effort-limit))

(define (get-scheme-libraries target-cpu target-kernel options)
  (if (memq 'freestanding options)
      '()
      `("akku/metadata"
        "loko/runtime/parameters"
        "loko/config"
        "loko/runtime/booleans"
        "loko/runtime/fixnums"
        "loko/runtime/flonums"
        "loko/runtime/chars"
        "loko/runtime/pairs"
        "loko/runtime/bytevectors"
        "loko/runtime/arithmetic"
        "loko/runtime/strings"
        "loko/runtime/vectors"
        "loko/runtime/equal"     ;eqv? -> memv -> case
        "loko/runtime/context"
        "loko/runtime/records"
        "loko/runtime/reader"
        "loko/runtime/control"
        "loko/runtime/utils"
        "loko/match"
        "loko/runtime/init"
        "loko/runtime/hashtables"
        "loko/runtime/symbols"
        "loko/runtime/conditions"
        "loko/runtime/enums"
        "loko/runtime/time"
        "pfds/heaps"
        "loko/runtime/fibers"
        "loko/runtime/io"
        "loko/runtime/sorting"
        ,@(if (memq 'eval options)
              '("psyntax/compat"
                "psyntax/internal"
                "psyntax/config"
                "psyntax/library-manager"
                "psyntax/builders"
                "psyntax/expander"  ;assert starts working: assertion-error
                "loko/compiler/recordize"
                "loko/compiler/let"
                "loko/compiler/letrec"
                "loko/compiler/cp0"
                "loko/compiler/mutation"
                "loko/compiler/freevar"
                "loko/compiler/loops"
                "loko/compiler/infer"
                "loko/compiler/closure"
                "loko/compiler/optimize"
                "loko/compiler/expander"
                "loko/runtime/repl"
                "loko/arch/asm")
              '())
        "struct/pack"
        ,@(if (memq 'main options)
              '("loko/runtime/main")
              '())
        "loko/runtime/unsafe"
        ;; Target-specific stuff. Only code in (loko arch ...) should
        ;; import these.
        ,@(case target-cpu
            ((amd64)
             `("machine-code/disassembler/x86-opcodes"
               ;; FIXME: use instead of (loko arch amd64 disassembler)
               ;; "machine-code/disassembler/private"
               ;; "machine-code/disassembler/x86"
               "loko/arch/amd64/disassembler"
               "loko/arch/amd64/traps"
               "loko/arch/amd64/memory"
               ,@(if (memq 'eval options)
                     '("loko/arch/amd64/objects"
                       "loko/arch/amd64/codegen"
                       "loko/arch/amd64/analyzer")
                     '())
               "loko/arch/amd64/processes"
               "loko/arch/amd64/playground"
               "loko/arch/amd64/prototyping"
               "loko/arch/amd64/registers"
               ,@(if (or (memq target-kernel '(pc pc+linux))
                         (memq 'eval options))
                     '("machine-code/assembler/x86-misc"
                       "machine-code/assembler/x86-operands"
                       ;;FIXME: runs some code on startup
                       "machine-code/assembler/x86")
                     '())
               ,@(if (memq 'eval options)
                     '("machine-code/format/elf"
                       "machine-code/assembler/elf"
                       "loko/arch/amd64/tables"
                       "loko/arch/amd64/lib-gc"
                       "loko/arch/amd64/lib-printer"
                       "loko/arch/amd64/lib-stacks"
                       "loko/arch/amd64/lib-traps"
                       "loko/arch/amd64/lib-valgrind"
                       "loko/arch/amd64/lib")
                     '())
               ,@(if (memq target-kernel '(pc pc+linux))
                     `(,@(if (memq 'eval options)
                             '("loko/arch/amd64/pc-segments"
                               "loko/arch/amd64/pc-interrupts"
                               "loko/arch/amd64/pc-paging"
                               "loko/arch/amd64/pc-syscalls"
                               "loko/arch/amd64/pc-start")
                             '())
                       "loko/arch/amd64/pc-ap-boot"
                       "loko/runtime/buddy"
                       "loko/arch/amd64/pc-init")
                     '())
               ,@(if (memq target-kernel '(linux pc+linux))
                     `("loko/arch/amd64/linux-numbers"
                       "loko/arch/amd64/linux-syscalls"
                       ,@(if (memq 'eval options)
                             '("loko/arch/amd64/linux-start")
                             '())
                       "loko/arch/amd64/linux-init")
                     '())
               ,@(if (memq 'eval options)
                     ;; These native and cross compiler targets will
                     ;; be supported. They should also be in
                     ;; compile-loko.sps if you want to build Loko.
                     '("loko/arch/amd64/pc-asm"
                       "loko/arch/amd64/linux-asm"
                       "loko/arch/amd64/pc-and-linux-asm")
                     '())
               "loko/u8rings"
               "loko/drivers/uart/ns8250" ;FIXME: exclude from linux builds
               "loko/arch/amd64/process-init"))
            (else '()))
        ,@(if (memq 'eval options)
              '("loko/runtime/eval"
                "loko/compiler/main"
                "loko/compiler/static")
              '())
        "loko/runtime/start"
        ,@(if (memq 'eval options)
              '("loko/runtime/start-libman")
              '()))))

;; Get filenames from a subset of the libraries above here.
(define (get-scheme-library-files library-dir target-cpu target-kernel options)
  (map (lambda (fn)
         (let ((base (string-append library-dir "/" fn)))
           (if (file-exists? (string-append base ".loko.sls"))
               (string-append base ".loko.sls")
               (string-append base ".sls"))))
       (get-scheme-libraries target-cpu target-kernel options)))

(define (compile-program out-fn sps-fn options)
  (define library-files
    (get-scheme-library-files (or (get-environment-variable "LOKO_SOURCE")
                                  (config-source-path))
                              (config-target-cpu)
                              (config-target-kernel)
                              options))
  (let-values ([(name* core* locs)
                (expand-files library-files sps-fn (memq 'use-primlocs options)
                              (memq 'freestanding options))])
    (let* ((primlocs (lambda (x)
                       (cond ((assq x locs) => cdr)
                             ((memq x '(eval environment))
                              (error 'compile-loko "Eval needs -feval"))
                             (else
                              (error 'compile-loko "No location for primitive" x)))))
           (codes
            (map-in-order (lambda (name core)
                            (display name) (newline)
                            (let ((code (compiler-passes name core)))
                              ;; (pretty-print (compiler-code->sexpr code))
                              code))
                          name* core*)))
      (assemble-text-file out-fn codes primlocs)))))
