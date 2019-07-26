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

;;; Valgrind client requests for when running under Valgrind.

;; If not under Valgrind then these calls are NOPs. Valgrind believes
;; in the red zone, which Loko does not care about, so it needs
;; patching to disable those checks. In memcheck/mc_main.c the
;; helperc_MAKE_STACK_UNINIT_* procedures should be disabled.

(library (loko arch amd64 lib-valgrind)
  (export
    lib-valgrind:text lib-valgrind:data)
  (import
    (loko arch amd64 objects)
    (rnrs))

(define (lib-valgrind:text)
;;;
;;; Issues a Valgrind client request
;;;
  `((%label $valgrind)
    ;; Function signature.
    ;; Input:   rdi    fixnum pointer to request data
    ;; Output:  rax    fixnum result
    ;;
    ;; Valgrind magic sequence.
    ;; Input:   rax    pointer to the request
    ;; Output:  rdx    result
    (sar rdi ,(shift 'fixnum))
    ;; This code is here because the optimizer would remove the magic
    ;; sequence.
    (xor edx edx)
    (rol rdi 3)
    (rol rdi 13)
    (rol rdi 61)
    (rol rdi 51)
    (xchg rbx rbx)
    (mov rax rdx)
    (shl rax ,(shift 'fixnum))
    (ret)))

(define (lib-valgrind:data)
  `()))
