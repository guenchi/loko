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

;;; Wrapper around the architecture-dependent assembler libraries.

#|

The following gives a list of assembler labels that are present for
all architectures. Their arguments are passed according to the
standard calling convention for the architecture. These are unsafe
primitives that can't be called by user code. The ones that can
be different depending on the kernel Loko is running under are
referenced via a function pointer stored in bss.

  ((mem+ *debug-put-u8) <unscaled-byte>)
    Outputs a byte to the architecture's debug port.

  ((mem+ *panic))
    Halts the system and indicates a panic to the user.

|#

(library (loko arch asm)
  (export
    assembler-library
    assemble
    code-generator
    instruction-analyzer
    target-convention
    generate-tables)
  (import
    (prefix (loko arch amd64 pc-start) amd64-pc-start:)
    (prefix (loko arch amd64 linux-start) amd64-linux-start:)
    (prefix (loko arch amd64 lib) amd64:)
    (prefix (loko arch amd64 codegen) amd64:)
    (prefix (loko arch amd64 analyzer) amd64:)
    (prefix (loko arch amd64 tables) amd64:)
    (rnrs))

(define (copyright)
  '((%utf8z "Copyright © 2019 Göran Weinholt")))

;; Returns a bytevector of assembled code and a hashtable with symbols.
(define (assemble target-cpu code)
  (case target-cpu
    ((amd64)
     (amd64:assemble code))
    (else
     (error 'assemble "Unsupported CPU architecture" target-cpu))))

(define (code-generator target-cpu code primlocs make-init-code?)
  (case target-cpu
    ((amd64)
     (amd64:codegen code primlocs make-init-code?))
    (else
     (error 'code-generator "Unsupported CPU architecture" target-cpu))))

;; Returns a procedure that performs instruction analysis for the
;; given target.
(define (instruction-analyzer target-cpu)
  (case target-cpu
    ((amd64)
     amd64:instruction-analyzer)
    (else
     #f)))

;; Returns a procedure that can answer questions about the target
;; architecture.
(define (target-convention target-cpu)
  (case target-cpu
    ((amd64) amd64:target-convention)
    (else
     (error 'target-convention
            "Unsupported CPU architecture" target-cpu))))

;; If the architecture has need of some particular tables, e.g.
;; stack unwinding tables, these can be generated here.
(define (generate-tables target-cpu text data)
  (case target-cpu
    ((amd64) (amd64:table-generator text data))
    (else
     (values text data))))

;; Returns assembler code for the Loko runtime. This code starts
;; with a header (e.g. ELF or multiboot), contains a start label
;; that calls scheme-start, and then exits (or reboots). The
;; standard procedures are also defined here.
(define (assembler-library target-cpu target-kernel text data)
  (define originate
    '((%origin #x200000)
      (%label image-address-zero)))
  (define target (vector target-cpu target-kernel))
  (cond ((equal? target '#(amd64 linux))
         (append originate
                 (amd64-linux-start:image-header)
                 (copyright)
                 (amd64:text-start)
                 (amd64-linux-start:text-start)
                 (amd64:text)
                 (amd64-linux-start:text)
                 text
                 (amd64:text-end)
                 (amd64:data-start)
                 (amd64:data)
                 (amd64-linux-start:data)
                 data
                 (amd64:data-end)
                 (amd64-linux-start:image-footer)))
        ((equal? target '#(amd64 loko))
         (append originate
                 (amd64-pc-start:image-header 'image-address-zero)
                 (copyright)
                 (amd64:text-start)
                 (amd64-pc-start:text-start)
                 (amd64:text)
                 (amd64-pc-start:text)
                 text
                 (amd64:text-end)
                 (amd64:data-start)
                 (amd64:data)
                 (amd64-pc-start:data)
                 data
                 (amd64:data-end)))
        ((equal? target '#(amd64 loko+linux))
         (append originate
                 (amd64-linux-start:image-header)
                 (copyright)
                 (amd64-pc-start:image-header 'image-address-zero)
                 (amd64:text-start)
                 (amd64-pc-start:text-start)
                 (amd64-linux-start:text-start)
                 (amd64:text)
                 (amd64-pc-start:text)
                 (amd64-linux-start:text)
                 text
                 (amd64:text-end)
                 (amd64:data-start)
                 (amd64:data)
                 (amd64-linux-start:data)
                 (amd64-pc-start:data)
                 data
                 (amd64:data-end)
                 (amd64-linux-start:image-footer)))
        (else
         (error 'assembler-library
                "Unsupported target" target)))))
