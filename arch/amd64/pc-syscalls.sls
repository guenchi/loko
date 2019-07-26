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

;;; System call interface using SYSCALL/SYSRET

;; These syscalls are only present to work around the fact that #AC
;; only works at CPL=3 and some instructions only work at CPL=0.

;; The return value should be in RAX and should fit in a fixnum, but
;; it should not actually be a fixnum. This is to make it compatible
;; with the Linux syscall interface which obviously does not return
;; fixnums.

(library (loko arch amd64 pc-syscalls)
  (export
    text data
    text-initialize-syscalls
    ;; syscall numbers
    NR-hlt NR-rdmsr NR-wrmsr)
  (import
    (rnrs)
    (prefix (loko arch amd64 pc-segments) pc-segments:)
    (loko arch amd64 registers))

;; The IA32_STAR register works like this:
;; On SYSRET: IA32_STAR[63:48]+16->CS(sel), IA32_STAR[63:48]+8->SS(sel).
;; On SYSCALL: IA32_STAR[47:32]->CS(sel), IA32_STAR[47:32]+8->SS(sel).
;; If the selectors are wrong then things will seem to work, until
;; they are reloaded.

;; Loko syscall numbers
(define NR-hlt -1)
(define NR-rdmsr -2)
(define NR-wrmsr -3)

(define (text-initialize-syscalls)
  `((mov ecx ,(MSR IA32_EFER))
    (rdmsr)
    (or eax ,(EFER SCE))
    (wrmsr)                     ;enable SYSCALL/SYSRET
    ;; Negated RFLAGS mask. Bits to clear.
    (mov ecx ,(MSR IA32_FMASK))
    (rdmsr)
    (xor edx edx)
    (xor eax eax)
    (wrmsr)
    ;; Target RIP
    (mov ecx ,(MSR IA32_LSTAR))
    (mov eax syscall-vector)
    (xor edx edx)
    ;; (mov edx (>> syscall-vector 32))
    (wrmsr)
    ;; Segment selectors
    (mov ecx ,(MSR IA32_STAR))
    ;; (rdmsr)
    (xor eax eax)
    (mov edx ,(fxior (fxarithmetic-shift-left (- pc-segments:data-PL3 8) 16)
                     pc-segments:code-PL0))
    (wrmsr)))

(define (text)
  `((%align 8)
    (%label syscall-vector)
    ;; Preserve RCX and R11 in order to do SYSRET.
    ;; RAX is used to pass in a syscall number.
    ;; RAX is also the return value.
    ;; Arguments passed in RDI RSI RDX R10 R8 R9.
    ;; There are no fixnums here, it's all integers.
    ;; Do NOT clobber the stack, it's a Scheme stack!

    (cmp rax ,NR-hlt)
    (je syscall-hlt)
    (cmp rax ,NR-rdmsr)
    (je syscall-rdmsr)
    (cmp rax ,NR-wrmsr)
    (je syscall-wrmsr)

    ;; RCX->RIP, R11->RFLAGS, IA32_STAR[63:48]+16->CS,
    ;; IA32_STAR[63:48]+8->SS.

    ;; Returns to 64-bit CPL=3 code.
    (xor eax eax)
    (dec rax)
    (sysretq)

;;; Enable interrupts and wait for an interrupt
    ;; should possibly be monitor/mwait instead
    (%align 8)
    (%label syscall-hlt)
    (sti)
    (hlt)
    (cli)
    (xor eax eax)
    (sysretq)

;;; Read an MSR
    (%align 8)
    (%label syscall-rdmsr)
    (mov r10 rcx)
    (mov ecx edi)
    (rdmsr)
    (mov (mem32+ rsi) eax)
    (mov (mem32+ rsi 4) edx)
    (xor eax eax)
    (mov rcx r10)
    (sysretq)

;;; Write an MSR
    (%align 8)
    (%label syscall-wrmsr)
    (mov r10 rcx)
    (mov ecx edi)
    (mov eax esi)
    ;;(mov edx edx)
    (wrmsr)
    (xor eax eax)
    (mov rcx r10)
    (sysretq)))

(define (data)
  '()))
