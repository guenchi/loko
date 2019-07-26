#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Loko Scheme - an R6RS Scheme compiler

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

;;; Program that builds the loko binary

(import
  (rnrs (6))
  (loko utils)
  (psyntax library-manager)
  (loko compiler expander)
  (loko compiler)
  (loko config)
  (only (loko compiler cp0) cp0-effort-limit))

(define library-dir ".akku/lib")

(define scheme-library-files
  `("akku/metadata"
    "loko/config"
    "loko/libs/booleans"
    "loko/libs/fixnums"
    "loko/libs/flonums"
    "loko/libs/chars"
    "loko/libs/pairs"
    "loko/libs/bytevectors"
    "loko/libs/arithmetic"
    "loko/libs/strings"
    "loko/libs/vectors"
    "loko/libs/equal"     ;eqv? -> memv -> case
    "loko/libs/context"
    "loko/libs/control"
    "loko/utils"
    "loko/match"
    "loko/libs/records"
    "loko/libs/hashtables"
    "loko/libs/symbols"
    "loko/libs/conditions"
    "loko/libs/enums"
    "loko/libs/io"
    "loko/libs/reader"
    "loko/libs/sorting"
    "psyntax/compat"
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
    "loko/init"
    "struct/pack"
    "text-mode/console/events"
    "text-mode/console/model"
    "text-mode/terminfo/builtins"
    "text-mode/terminfo/constants"
    "text-mode/terminfo/parser"
    "text-mode/termios"
    "text-mode/terminfo"
    "text-mode/platform"
    "text-mode/unicode"
    "text-mode/console"
    "loko/tui"
    "loko/repl"
    "loko/main"
    ;; Target-specific stuff. Only code in (loko arch ...) should
    ;; import these.
    ,@(case (config-target-cpu)
        ((amd64)
         `("machine-code/disassembler/x86-opcodes"
           ;; FIXME: use instead of (loko arch amd64 disassembler)
           ;; "machine-code/disassembler/private"
           ;; "machine-code/disassembler/x86"
           "loko/arch/amd64/traps"
           "loko/arch/amd64/memory"
           "loko/arch/amd64/objects"
           "loko/arch/amd64/codegen"
           "loko/arch/amd64/analyzer"
           "loko/arch/amd64/disassembler"
           "loko/arch/amd64/processes"
           "loko/arch/amd64/playground"
           "loko/arch/amd64/prototyping"
           "loko/arch/amd64/tables"
           "machine-code/format/elf"
           "machine-code/assembler/elf"
           "machine-code/assembler/x86-misc"
           "machine-code/assembler/x86-operands"
           "machine-code/assembler/x86" ;FIXME: runs some code on startup
           "loko/arch/amd64/registers"
           "loko/arch/amd64/lib-gc"
           "loko/arch/amd64/lib-printer"
           "loko/arch/amd64/lib-stacks"
           "loko/arch/amd64/lib-traps"
           "loko/arch/amd64/lib-valgrind"
           "loko/arch/amd64/lib"
           ;; These need to be last
           ,@(if (memq (config-target-kernel) '(loko loko+linux))
                 '("loko/arch/amd64/pc-segments"
                   "loko/arch/amd64/pc-interrupts"
                   "loko/arch/amd64/pc-paging"
                   "loko/arch/amd64/pc-syscalls"
                   "loko/arch/amd64/pc-start"
                   "loko/arch/amd64/pc-ap-boot"
                   "loko/arch/amd64/pc-init")
                 '())
           ,@(if (memq (config-target-kernel) '(linux loko+linux))
                 '("loko/arch/amd64/linux-numbers"
                   "loko/arch/amd64/linux-syscalls"
                   "loko/arch/amd64/linux-start"
                   "loko/arch/amd64/linux-init")
                 '())
           "loko/arch/amd64/process-init"))
        (else '()))
    "loko/arch/asm"
    "loko/libs/eval"
    "loko/compiler"
    "loko/start"))

(cp0-effort-limit 1000)
(library-directories '())
(library-extensions '())

(let-values ([(name* core* locs)
              (expand-files (map (lambda (fn)
                                   (let ((base (string-append library-dir "/" fn)))
                                     (if (file-exists? (string-append base ".loko.sls"))
                                         (string-append base ".loko.sls")
                                         (string-append base ".sls"))))
                                 scheme-library-files)
                            'use-primlocs)])
  (let ((filename "loko.out")
        (primlocs (lambda (x)
                    (cond ((assq x locs) => cdr)
                          (else
                           (error 'compile-loko "No location for primitive" x))))))
    (let* ((codes
            (map-in-order (lambda (name core)
                            (display name) (newline)
                            (let ((code (compiler-passes name core)))
                              ;; (pretty-print (compiler-code->sexpr code))
                              code))
                          name* core*)))
      (assemble-text-file filename codes primlocs))

    (display "Build finished and written to ")
    (display filename)
    (newline)))
