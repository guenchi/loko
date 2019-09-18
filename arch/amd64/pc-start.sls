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

;;; Loko multiboot image support

;; The plan is: This brings up Loko in long mode on a bare PC. The
;; image is mapped to address #x200000 just like when running with an
;; OS. The first 4GB of memory is identity mapped. The code in here
;; can only fail in two ways: Either the bootloader is not multiboot
;; compliant, or the CPU does not support long mode. Errors are
;; reported via the port 80h debug port.

(library (loko arch amd64 pc-start)
  (export
    image-header
    text-start text
    data)
  (import
    (loko config)
    (prefix (loko arch amd64 pc-interrupts) pc-interrupts:)
    (prefix (loko arch amd64 pc-paging) pc-paging:)
    (loko arch amd64 registers)
    (prefix (loko arch amd64 pc-segments) pc-segments:)
    (prefix (loko arch amd64 pc-syscalls) pc-syscalls:)
    (prefix (loko arch amd64 lib) lib:)
    (only (loko arch amd64 objects) immediate shift)
    (only (loko runtime context) CPU-VECTOR:CPU-NUMBER)
    (rnrs (6)))

;;; .text

;; This header does not need to be at the exact beginning of the file.
(define (image-header load-addr)
  (define header-magic #x1BADB002)
  (define flags (bitwise-ior (bitwise-arithmetic-shift-left 1 16) ;non-ELF binary
                             (bitwise-arithmetic-shift-left 1 1) ;memory map
                             (bitwise-arithmetic-shift-left 1 0))) ;page-align modules
  (define checksum (bitwise-and (- (+ header-magic flags)) #xffffffff))
  `((%mode 32)
    (jmp multiboot:start)
    ;; Multiboot version 1 header. It will probably not be difficult
    ;; to add a Multiboot v2 header as well, for twice the fun.
    (%align 4)
    (%label multiboot-header)
    (%u32 ,header-magic ,flags ,checksum)
    (%u32 multiboot-header ,load-addr bss bss-end multiboot:start)))

;; This is at the very start of the binary image. Its job is to make
;; a multiboot image, and before entering scheme-start, set up the
;; basic environment. Runs without a stack.
(define (text-start)
  (define error-bootloader (vector 'multiboot 'bad-bootloader))
  (define error-cpu (vector 'multiboot 'bad-cpu))
  (define stop (vector 'multiboot 'stop))
  (define stop-end (vector 'multiboot 'stop-end))
  (define reload-cs32 (vector 'multiboot 'reload-cs32))
  `((%mode 32)
    (%label multiboot:start multiboot:start64)
    ;; eax: multiboot magic, ebx: multiboot information structure
    (cmp eax #x2BADB002)
    (jne ,error-bootloader)
    (mov al #x00) (out #x80 al)         ;PORT80h

    ;; Set a temporary GDT with a 32/64-bit code segment
    (lgdt (mem+ gdtr32))
    (jmpf (far #x18 ,reload-cs32))      ;load cs
    (%label ,reload-cs32)
    (mov eax #x10)
    (mov ds ax)
    (mov es ax)
    (mov fs ax)
    (mov gs ax)
    (mov ss ax)
    (%comm idtr-dummy 6 2)
    (lidt (mem+ idtr-dummy))            ;interrupt -> triple fault

    ;; Switch to long mode
    (mov eax ,(CR4 PAE #|DE MCE PCE|# OSFXSR #|OSXMMEXCPT|#))
    (mov cr4 eax)      ;activate PAE and SSE

    ,@(pc-paging:text)                  ;load IA32_PAT

    (mov eax pml4)
    (mov cr3 eax)                       ;level 4 page map table

    (mov ecx ,(MSR IA32_EFER))
    (rdmsr)
    (or eax ,(EFER LME NXE))
    (wrmsr)                             ;enable long mode, etc

    (mov eax cr0)
    (or eax ,(CR0 PG WP AM MP NE))
    (and eax ,(bitwise-not (CR0 CD NW)))
    (mov cr0 eax)                       ;enable paging, etc

    ;; The next instruction must be a branch, AMD says. The previous
    ;; instruction enabled long mode. The CPU is still in a 32-bit
    ;; code segment, so now do a far jump to 64-bit mode. Uses the
    ;; 32-bit GDT.
    (jmpf (far ,pc-segments:code-PL0 multiboot:start64))

;;; 64-bit start up
    (%mode 64)
    (%align 8)
    (%label multiboot:start64 ,stop)
    (mov al #x03) (out #x80 al)         ;PORT80h

    ;; Initialize architectural procedures
    (mov (mem64+ *debug-put-u8) multiboot:debug-put-u8)
    (mov (mem64+ *panic) multiboot:panic)

    ;; Save multiboot information structure and set the boot loader
    ;; type. RBX is live until here.
    (sal rbx ,(shift 'fixnum))
    (mov (mem64+ boot-loader-data) rbx)
    (mov rax ,(immediate 'multiboot))
    (mov (mem64+ boot-loader-type) rax)

    ;; Get a per-CPU vector as FS and RAX, CPU number as RBX
    ,@(lib:text-allocate-per-cpu-vector stop)
    (mov ecx ,(MSR IA32_FS_BASE))
    (xor edx edx)
    (wrmsr)

    ;; Setup/reload all other segments
    (lgdt (mem+ gdtr-table (* rbx 2)))
    (mov ax ,pc-segments:data-PL0)
    (mov ss ax)
    (xor eax eax)                       ;null selector
    (mov ds ax)
    (mov es ax)
    (mov gs ax)
    (lldt ax)                           ;no LDT
    (mov eax ,pc-segments:task)
    (ltr ax)                            ;load TSS via GDT
    (lidt (mem+ idtr))
    (fninit)

    ;; Enable the syscall interface
    ,@(pc-syscalls:text-initialize-syscalls)

    ;; RCX->RIP, R11->RFLAGS, IA32_STAR[63:48]+16->CS,
    ;; IA32_STAR[63:48]+8->SS. Go to CPL=3.
    (mov ecx scheme-init)
    (mov r11d ,(bitwise-ior (RFLAGS-IOPL 3)
                            (RFLAGS #;IF AC)))
    (sysretq)

;;; Error handling

    ;; Error handlers for when it's not possible to get to
    ;; scheme-init. Can run from 32-bit or 64-bit modes.
    (%label ,error-bootloader)
    (mov al #x01) (out #x80 al)         ;PORT80h
    (jmp ,stop)
    (%label ,error-cpu)
    (mov al #x02) (out #x80 al)         ;PORT80h
    (jmp ,stop)

    (%label ,stop)
    (hlt)
    (jmp ,stop)))

;;; Standard library

(define (text)
  ;; Early debugging output. Input in edi.
  (define return (vector 'return))
  (define wait-thr (vector 'wait-thr))
  `((%mode 64)
    (%align 8)
    (%label multiboot:debug-put-u8)
    ;; serial output
    (mov dx ,(+ #x3f8 5))
    (%label ,wait-thr)
    (in al dx)                          ;LSR
    (test al #b100000)
    (jz ,wait-thr)
    (mov dx #x3f8)
    (mov eax edi)
    (out dx al)                         ;THB
    (cmp al ,(char->integer #\linefeed))
    (jne ,return)
    (mov al ,(char->integer #\return))
    (out dx al)
    (%label ,return)
    (ret)

    (%align 8)
    (%label multiboot:panic)
    (mov al #x50) (out #x80 al)         ;PORT80h
    (jmp multiboot:panic)

    ,@(pc-interrupts:text)
    ,@(pc-syscalls:text)))

;;; .data

(define (data)
  `((%utf8z "This Scheme image has multiboot amd64 support")
    ,@(pc-interrupts:data)
    ,@(pc-segments:data (config-max-cpus))
    ,@(pc-paging:data))))
