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

;;; Register the target for Loko on Linux on AMD64

(library (loko arch amd64 linux-asm)
  (export visit)
  (import
    (rnrs (6))
    (loko arch asm)
    (prefix (loko arch amd64 linux-start) amd64-linux-start:)
    (prefix (loko arch amd64 lib) amd64:)
    (prefix (loko arch amd64 codegen) amd64:)
    (prefix (loko arch amd64 analyzer) amd64:)
    (prefix (loko arch amd64 tables) amd64:))

(define (visit) #f)

(define (linux-assembler-library target-cpu target-kernel notice text data)
  (append '((%origin #x200000)
            (%label image-address-zero))
          (amd64-linux-start:image-header)
          notice
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

(register-target 'amd64 'linux
                 amd64:assemble
                 amd64:codegen
                 amd64:instruction-analyzer
                 amd64:target-convention
                 amd64:table-generator
                 linux-assembler-library))
