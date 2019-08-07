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

;;; Stack handling

(library (loko arch amd64 lib-stacks)
  (export
    lib-stacks:text lib-stacks:data)
  (import
    (only (loko arch amd64 registers) RFLAGS)
    (loko arch amd64 objects)
    (loko libs context)
    (rnrs (6)))

(define (lib-stacks:text)
  (define %heap-rem 'r13)
  (define %alloc 'r14)
  (define (pvec i) `(,(+ 8 (* 8 i) (- (tag 'vector)))))

  `((%align 8)
    ;;;
    ;;; Copy the current stack into a newly allocated box object.
    ;;;
    (%label copy-stack restore-stack)
    ;; This returns a boxed stack object. The first field is the
    ;; length of the stack. This is followed by the values in the
    ;; stack. The first field following the length field must be the
    ;; return address for this subroutine.

    ;; Remove anything on the stack that isn't live. TODO: this
    ;; would not be necessary if the GC could recognize that it is
    ;; starting to trace inside a copied stack.
    (mov r8 (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (mov rbx rsp)
    (mov rbp (mem64+ r8 ,@(pvec PROCESS-VECTOR:STACK-TOP)))
    (call cleanup-stack)

    ;; Find the length and do the heap overflow check.
    (mov rcx (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR))) ;get process-vector
    (mov rcx (mem64+ rcx 8 ,(* 8 PROCESS-VECTOR:STACK-TOP) ,(- (tag 'vector))))
    (sub rcx rsp)
    (lea rdx (mem+ 8 8 rcx 15))
    (and rdx -16)
    (sub ,%heap-rem rdx)
    ,@(let ((skip-gc (vector 'copy-stack-skip-gc)))
        `((jns ,skip-gc)
          (shl rcx 3)
          (mov rdi (mem64+ r8 ,@(pvec PROCESS-VECTOR:STACK-TOP))) ;don't clean again
          (add rdi 8)                 ;readjusted in stop-and-copy
          (push rdx)
          (push rcx)
          (call stop-and-copy)        ;XXX: cleans the stack again!
          (pop rcx)
          (pop rdx)
          (sar rcx 3)
          (%label ,skip-gc)))
    ;; Initialize the box header
    (mov (mem64+ ,%alloc) ,(immediate 'stack)) ;store type
    (mov (mem64+ ,%alloc 8) rcx)      ;store length
    ;; Allocate the box
    (lea rax (mem+ ,%alloc ,(tag 'box))) ;return value
    (lea ,%alloc (mem64+ ,%alloc 8 8 rcx 15))
    (and ,%alloc -16)
    ;; Copy the stack into the box
    (shr rcx 3)                       ;copy 64 bits at a time
    (mov rsi rsp)
    (lea rdi (mem+ rax 8 8 ,(- (tag 'box))))
    (rep.movs (mem64+ rdi) (mem64+ rsi))
    (ret)

    ;;;
    ;;; Restore a boxed stack.
    ;;;
    (%align 8)
    (%label restore-stack switch-stack)
    ;; rdx is a stack box (see above) that contains the stack to be
    ;; restored. rax is a value to return, so don't touch it. TODO:
    ;; this should be multiple values.

    ;; Find the top of the stack
    (mov rdi (mem+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (mov rdi (mem64+ rdi 8 ,(* 8 PROCESS-VECTOR:STACK-TOP) ,(- (tag 'vector))))

    ;; Restore the stack
    (mov rcx (mem+ rdx 8 ,(- (tag 'box)))) ;length of saved stack
    (sub rdi rcx)
    (shr rcx 3)
    (lea rsi (mem+ rdx 8 8 ,(- (tag 'box))))
    (mov rsp rdi)
    (rep.movs (mem64+ rdi) (mem64+ rsi))
    (ret)                             ;rax is set by $restore-stack

    ;;;
    ;;; Switch and save stack pointers.
    ;;;
    (%align 8)
    (%label switch-stack cleanup-stack)
    ;; Switches stack pointers, saving the old one. The new stack is
    ;; in RDI and the return value is already in RAX.
    ;; TODO: CLI/STI should be done by $process-yield. Right now
    ;; there is nothing that does STI again after this
    ;; (pushfq)
    ;; (and (mem64+ rsp) ,(fxnot (RFLAGS IF))) ;attempt to do CLI
    ;; (popfq)
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-SP)) rsp)
    (sal (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-SP)) ,(shift 'fixnum))
    (mov rsp rdi)
    (ret)

    ;;;
    ;;; Clear non-live objects from the stack.
    ;;;
    (%align 8)
    (%label cleanup-stack)
    ,@(let ((next-return (vector 'next-return))
            (size-read (vector 'size-read))
            (size-read2 (vector 'size-read2))
            (next-slot (vector 'next-slot))
            (do-not-kill (vector 'do-not-kill))
            (no-more-masks (vector 'no-more-masks))
            (exit (vector 'exit)))
        (define (lodsb dst)
          ;; Could be replaced by lods if dst == eax, but lods is
          ;; microcoded and slow. TODO: The NOPs are (currently)
          ;; eight bytes long, so it is possible to read them in one
          ;; fell swoop.
          `((movzx ,dst (mem8+ rsi)) (inc rsi)))
        ;; rsi points to the first return address slot that should
        ;; be inspected. rbp points to the top of the stack. See
        ;; (loko arch amd64 analyzer) for a description of the live
        ;; masks. Must preserve r8, r13 and r14.
        `((%label ,next-return)
          (cmp rbx rbp)
          (je ,exit)                  ;XXX: jea would mask bugs
          (mov rsi (mem64+ rbx))      ;Read the return address
          (add rbx 8)
          ;; Check that this is a long NOP
          ,@(lodsb 'eax) (cmp al #x0F) (jne ,next-return)
          ,@(lodsb 'eax) (cmp al #x1F) (jne ,next-return)
          ;; Check how many bytes are used for the frame size
          ,@(lodsb 'eax)
          (test al #b00111000)
          (jz ,next-return)           ;0 = assembly generated NOP
          (shr eax 3)
          (and eax #b11)              ;eax = 1, 2 or 3 bytes
          ;; Decode the frame size (written to edx)
          (xor edx edx)
          (mov edi eax)

          ;; Check the size of the frame size encoding.
          (cmp eax 2)
          (je ,size-read2)

          ;; The frame size is one byte.
          (%label ,size-read)
          (shl edx 8)
          ,@(lodsb 'ecx)
          (or edx ecx)
          (dec edi)
          (jnz ,size-read)            ;get another byte XXX: never taken!
          ;; rdi points to the next return address
          (lea rdi (mem+ rbx (* 8 rdx) 8))
          ;; Read the liveness mask (written to edx). The first
          ;; liveness mask is 32 bits wide. Afterwards ecx will
          ;; contain the number of valid bits in edx.
          ,@(lodsb 'edx)
          ,@(lodsb 'ecx) (shl ecx 8)  (or edx ecx)
          ,@(lodsb 'ecx) (shl ecx 16) (or edx ecx)
          ,@(lodsb 'ecx) (shl ecx 24) (or edx ecx)
          (mov ecx 32)                ;32 valid bits in edx
          (jmp ,next-slot)

          ;; The frame size is two bytes.
          (%label ,size-read2)
          ,@(lodsb 'edx)
          ,@(lodsb 'ecx) (shl ecx 8) (or edx ecx)
          ;; rdi points to the next return address
          (lea rdi (mem+ rbx (* 8 rdx) 8))
          ;; Read the liveness mask.
          ,@(lodsb 'edx)
          ,@(lodsb 'ecx) (shl ecx 8)  (or edx ecx)
          ,@(lodsb 'ecx) (shl ecx 16) (or edx ecx)
          (mov ecx 24)                ;24 valid bits in edx

          ;; Now go through each of the slots in the frame. If a
          ;; slot is not live then clobber it. TODO: it is possible
          ;; to skip over sequences of clear bits in the livemask if
          ;; this is integrated with the GC stack tracer
          (%label ,next-slot)
          (cmp rbx rdi)
          (je ,next-return)           ;finished with this frame?
          (add rbx 8)
          (dec ecx)                   ;one bit less
          (shr rdx 1)                 ;put liveness into CF
          (jc ,do-not-kill)           ;CF = 1 is slot is live
          ;; Replace the reference with an object that shows where
          ;; it was stored. If this kills something that is live,
          ;; that will make it easier to track where the value came
          ;; from.
          (lea rax (mem64+ rbx -8))
          (sal rax ,(shift 'kill-mark))
          (or rax ,(tag 'kill-mark))
          ;; (mov r9 (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
          ;; (mov r9 (mem64+ r8 ,@(pvec PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT)))
          ;; (sal r9 54)
          ;; (or rax r9)
          (mov (mem64+ rbx -8) rax)   ;kill the reference
          (%label ,do-not-kill)
          (test ecx ecx)
          (jnz ,next-slot)
          ;; Read the next liveness mask. The output is rdx and ecx.
          ;; If the following instruction is not a continuation-NOP,
          ;; then there are no more live variables in this frame.
          (xor edx edx)               ;nothing is live right now
          (xor ecx ecx)
          (dec ecx)                   ;"infinite" liveness mask
          (cmp (mem8+ rsi) #x0F) (jne ,next-slot)
          (cmp (mem8+ rsi 1) #x1F) (jne ,next-slot)
          (cmp (mem8+ rsi 2) #xA4) (jne ,next-slot)
          (add rsi 3)                 ;this is a good NOP
          ;; Liveness in SIB + displacement of the NOP
          ,@(lodsb 'edx)
          ,@(lodsb 'ecx) (shl ecx 8)  (or edx ecx)
          ,@(lodsb 'ecx) (shl ecx 16) (or edx ecx)
          ,@(lodsb 'ecx) (shl ecx 24) (or edx ecx)
          ,@(lodsb 'ecx) (shl rcx 32) (or rdx rcx)
          (mov ecx 40)
          (jmp ,next-slot)

          (%label ,exit)
          (ret)))))

(define (lib-stacks:data)
  '()))
