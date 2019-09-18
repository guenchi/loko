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

;;; Interrupt descriptors and entry points

;; Note: The stack should always be 16-byte aligned in interrupt
;; handlers. The CPU takes care of that itself when there is no
;; automatic stack switch.

;; TODO: #MC indicates that there is no reliable way to restart the program!

(library (loko arch amd64 pc-interrupts)
  (export text data)
  (import
    (rnrs)
    (loko arch amd64 registers)
    (loko arch amd64 objects)
    (loko runtime context)
    (prefix (loko arch amd64 pc-segments) pc-segments:))

;; These are programmed into the PICs and designate offsets into the
;; IDT. Must be aligned to 8.
;; (define PIC1-offset 32)
;; (define PIC2-offset (+ PIC1-offset 8))
;; (define APIC-offset (+ PIC2-offset 8))

;; (define irq-APIC-timer (+ APIC-offset 0))
;; (define irq-APIC-spurious (+ APIC-offset 1))
;; (define irq-APIC-lint0 (+ APIC-offset 2))
;; (define irq-APIC-lint1 (+ APIC-offset 3))
;; (define irq-APIC-error (+ APIC-offset 4))
;; (define irq-APIC-perfcnt (+ APIC-offset 5))
;; (define irq-APIC-thermal (+ APIC-offset 6))

(define (sysdesc dpl type offset selector ist)
  ;; Encodes a 64-bit system descriptor. Interrupt and trap gates
  ;; only right now. The DPL controls which privilege levels can
  ;; manually trigger an interrupt. Set dpl=3 to allow the int NN
  ;; instruction for CPL=3.
  `(bitwise-ior (bitwise-bit-field ,offset 0 16)
                (ash (bitwise-bit-field ,offset 16 64) ,(+ 32 16))
                (ash ,selector 16)    ;cs
                (ash ,ist 32)         ;Interrupt Stack Table
                (ash ,type ,(+ 32 8))
                (ash ,dpl ,(+ 32 13)) ;Descriptor Privilege Level
                (ash 1 ,(+ 32 15))))  ;Segment Present

(define idt-entries
  (let ((idt (make-vector 256 0)))
    ;; Code segment selector to use for the IDT (this is an index into
    ;; the GDT). Because HLT only runs in CPL=0, this cs must be used
    ;; for everything that can interrupt HLT.
    (define cs pc-segments:code-PL0-non-conforming)
    ;; Code segment selector for exceptions that run in CPL=3. XXX:
    ;; the interrupt handler for these will overwrite the Scheme
    ;; stack, but that might be OK, just as long as enough debugging
    ;; info is available elsewhere. Anything here with IST=0 must
    ;; accept that the Scheme stack is gone.
    (define cs-cpl3 pc-segments:code-PL3)
    (define interrupt-gate #b1110)    ;disables interrupts
    (define trap-gate #b1111)         ;does not disable interrupts
    ;; 0-31: Exceptions. The * suffix means an error code is pushed
    ;; on the stack.
    (vector-set! idt 0 (sysdesc 0 trap-gate 'fault-DE cs-cpl3 0))
    (vector-set! idt 1 (sysdesc 0 interrupt-gate 'trap/fault-DB cs 7))
    (vector-set! idt 2 (sysdesc 0 interrupt-gate 'int-NMI cs 1))
    (vector-set! idt 3 (sysdesc 0 interrupt-gate 'trap-BP cs 7))
    ;;4 #OF INTO (impossible)
    ;;5 #BR BOUND (impossible)
    (vector-set! idt 6 (sysdesc 0 trap-gate 'fault-UD cs-cpl3 0))
    (vector-set! idt 7 (sysdesc 0 interrupt-gate 'fault-NM cs 7))
    (vector-set! idt 8 (sysdesc 0 interrupt-gate 'abort-DF* cs 7))
    ;;9 Not used anymore
    (vector-set! idt 10 (sysdesc 0 interrupt-gate 'fault-TS* cs 7))
    (vector-set! idt 11 (sysdesc 0 interrupt-gate 'fault-NP* cs 7))
    (vector-set! idt 12 (sysdesc 0 trap-gate 'fault-SS* cs-cpl3 0))
    (vector-set! idt 13 (sysdesc 0 trap-gate 'fault-GP* cs-cpl3 0))
    (vector-set! idt 14 (sysdesc 0 interrupt-gate 'fault-PF* cs 2))
    ;;15 Intel reserved
    (vector-set! idt 16 (sysdesc 0 interrupt-gate 'fault-MF cs 7))
    (vector-set! idt 17 (sysdesc 0 trap-gate 'fault-AC* cs-cpl3 0))
    (vector-set! idt 18 (sysdesc 0 interrupt-gate 'abort-MC cs 7))
    (vector-set! idt 19 (sysdesc 0 interrupt-gate 'fault-XM cs 7)) ;aka #XF?
    ;;30 Security Exception #SX
    ;;31 Reserved
    ;; Fill in every entry so there is never a #GP due to a missing
    ;; IDT entry. N.B.! This can not handle pushed error codes. It
    ;; is not possible to know if new vectors in the 0-31 range will
    ;; have error codes.
    (letrec ((int-generic-n
              (lambda (n name)
                ;; TODO: align these?
                `((%label ,name)
                  (push ,(immediate n))
                  (jmp int-generic)))))
      (let lp ((i 0) (handlers '()))
        (cond ((= i (vector-length idt))
               (cons idt (reverse handlers)))
              ((and (eqv? (vector-ref idt i) 0)
                    (>= i 32))
               (let ((name (string->symbol
                            (string-append "int-" (number->string i 16)))))
                 (vector-set! idt i (sysdesc 0 interrupt-gate name cs 6))
                 (lp (+ i 1) (append (reverse (int-generic-n i name)) handlers))))
              (else
               (lp (+ i 1) handlers)))))))

(define (text)
  (define (pvec i) `(,(+ 8 (* 8 i) (- (tag 'vector)))))
  (define all-registers '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

  `(
;;; Serious exceptions

    ,@(letrec ((gen (lambda (label code)
                      `((%label ,label)
                        (mov al ,code) (out #x80 al)
                        (hlt)
                        (jmp ,label)))))
        `(,@(gen 'fault-TS* #xf0)     ;Invalid TSS
          ,@(gen 'fault-NP* #xf1)     ;Segment Not Present
          ,@(gen 'trap/fault-DB #xf3) ;Debug Exception
          ,@(gen 'fault-MF #xf4)      ;x87 FPU Floating-Point Error
          ,@(gen 'fault-XM #xf5)      ;SSE exception
          ,@(gen 'trap-BP #xf6)       ;Breakpoint
          ,@(gen 'abort-MC #xf7)      ;Machine Check
          ,@(gen 'fault-NM #xf8)      ;Device not available
          ,@(gen 'abort-DF* #xf9)     ;Double Fault
          ))

    ;; (%label trap/fault-DB)            ;Debug Exception
    ;; (push 0)                          ;code
    ;; (push 3)                          ;vector
    ;; (jmp gdb-attach)

    ;; (%label trap-BP)                  ;Breakpoint
    ;; (push 0)                          ;code
    ;; (push 1)                          ;vector
    ;; (jmp gdb-attach)

;;; Page fault
    (%label fault-PF*)                ;Page Fault
    ;; RSP layout: code RIP CS RFLAGS RSP SS. PF executes on IST 2
    ;; to allow dynamic extension of stack space.

    ;; Check if this is a reserved bit violation or if the CPU was
    ;; executing in a data or non-present page.
    (test (mem32+ rsp) #b11000)
    (jnz PF-serious-error)
    ;; TODO: handle non-present pages for the growable stacks

    ;; Return to Scheme. XXX: should pass CR2
    ;;(mov rbx (mem+ rsp #x08))         ;rbx=saved rip

    ;; RCX->RIP, R11->RFLAGS, IA32_STAR[63:48]+16->CS,
    ;; IA32_STAR[63:48]+8->SS.
    ;;(mov rcx PF-raise)
    (mov ecx invalid-address)
    (mov r11 (mem+ rsp #x18))         ;saved rflags
    (and r11d ,(bitwise-ior (RFLAGS IF)))
    (or r11d ,(bitwise-ior (RFLAGS-IOPL 3)
                           (RFLAGS AC)))
    (mov rsp (mem+ rsp #x20))         ;switch stack
    (sysretq)                         ;return to CPL=3

    (%label PF-serious-error)
    ;; The operating system has fallen and it can't get up.
    ;; FIXME: system-wide panic. or maybe something less dramatic.
    (mov al #xf8) (out #x80 al)
    (%label PF-stop)
    (hlt)
    (jmp PF-stop)

;;; Stack-Segment Fault
    (%label fault-SS*)
    ;; As far as I can tell, this can only be the result of
    ;; referencing a non-canonical pointer with RBP as base
    ;; register.
    (mov rax (mem64+ rsp 8))
    (mov rsp (mem64+ rsp ,(* 8 4)))
    (push rax)
    (jmp noncanonical-address)

;;; General protection
    (%label fault-GP*)
    ;; Non-canonical address violation is the only one we're
    ;; interested in here. Anything else is likely a system failure.
    ;; FIXME: check what happened... check CPL of the pushed CS?
    (mov rax (mem64+ rsp 8))
    (mov rsp (mem64+ rsp ,(* 8 4)))
    (push rax)
    (jmp noncanonical-address)

;;; Alignment check
    ;; CPL=3, interrupts enabled.
    (%label fault-AC*)
    (mov rax (mem64+ rsp 8))
    (mov rsp (mem64+ rsp ,(* 8 4)))
    (push rax)
    (jmp alignment-check)

;;; Divide error
    ;; CPL=3, interrupts enabled.
    (%label fault-DE)
    (mov rax (mem64+ rsp))
    (mov rsp (mem64+ rsp ,(* 8 3)))
    (push rax)
    (jmp divide-error)

;;; Undefined opcode
    ;; CPL=3, interrupts enabled.
    (%label fault-UD)
    (mov rax (mem64+ rsp))
    (mov rsp (mem64+ rsp ,(* 8 3)))
    (push rax)
    (jmp undefined-opcode)

;;; IRQs and interrupts with no other handler

    (%label int-NMI)                  ;Non-Maskable Interrupt
    (iretq)                           ;iret enables NMI again

    ;; All other interrupts go here. CPL=0, interrupts disabled,
    ;; interrupt stack table 6.

    ;; It would be nice if this could be CPL=3, but unfortunately
    ;; that seems to raise #DF since HLT runs at CPL=0. Using AMD's
    ;; MWAIT this could be done at CPL=3.

    ;; RSP layout: RIP CS RFLAGS RSP SS

    ,@(cdr idt-entries)
    (%align 16)
    (%label int-generic)
    ,@(let ((iret (vector 'iret)))
        `((pop (mem64+ fs ,(* 8 CPU-VECTOR:LAST-INTERRUPT-VECTOR))) ;save vector number
          (cmp (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-SP)) 0)
          (je ,iret)                    ;scheduler running, don't preempt
          (test (mem64+ rsp 16) ,(RFLAGS-IOPL 3))
          (jz ,iret)                    ;CPL=0, don't preempt

          ;; Preempt the current process.
          (push rbp)
          (mov rbp rsp)      ;remember rsp
          ;; RBP layout: RBP RIP CS RFLAGS RSP SS
          ;; Find the process save area. There is no safety check
          ;; on the vector-ref here.
          (mov rsp (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
          (mov rsp (mem64+ rsp ,@(pvec PROCESS-VECTOR:SAVE-AREA)))
          (add rsp ,PROCESS-SAVE-SIZE)

          ;; Push the IRETQ return frame, which is consumed by
          ;; interrupts:resume (and nothing else).
          (push (mem64+ rbp ,(* 8 5))) ;SS
          (push (mem64+ rbp ,(* 8 4))) ;RSP
          (push (mem64+ rbp ,(* 8 3))) ;RFLAGS
          (push (mem64+ rbp ,(* 8 2))) ;CS
          (push (mem64+ rbp ,(* 8 1))) ;RIP
          (push (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR))) ;save process-vector
          (sub rsp ,(* 16 3))         ;(need 16-byte alignment)
          (movdqa (mem128+ rsp ,(* 16 0)) xmm0)
          (movdqa (mem128+ rsp ,(* 16 1)) xmm1)
          (movdqa (mem128+ rsp ,(* 16 2)) xmm2)
          ,@(map (lambda (reg)
                   `(push ,(if (eq? reg 'rbp)
                               '(mem64+ rbp) ;fixup RBP
                               reg)))
                 all-registers)
          (push interrupts:resume)
          ;; (mov al ,(char->integer #\p))
          ;; (out #xe9 al)

          ;; RCX->RIP, R11->RFLAGS, IA32_STAR[63:48]+16->CS,
          ;; IA32_STAR[63:48]+8->SS.
          (mov rdi (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-SP)))
          (mov ecx switch-stack)
          (mov r11d ,(bitwise-ior (RFLAGS-IOPL 3)
                                  (RFLAGS AC)))
          (sar rdi ,(shift 'fixnum))
          (mov rax ,(immediate 'preempted))
          (sysretq)

          ;; It was decided against preempting. This ought to mean
          ;; that the scheduler is running and that it has issued a
          ;; HLT instruction (see syscall-hlt). The instruction
          ;; following the HLT is a CLI, but another interrupt can
          ;; happen immediately after IRETQ and before CLI if RFLAGS
          ;; is not modified here. TODO: use sysretq instead?
          (%label ,iret)
          ;; (push rax)
          ;; (mov al ,(char->integer #\h))
          ;; (out #xe9 al)
          ;; (pop rax)
          (and (mem64+ rsp 16) ,(fxnot (RFLAGS IF))) ;CLI
          (iretq)))

    ;; This is the reverse of the preempting in int-generic. It runs
    ;; in CPL=3 and is returned into from switch-stack.
    (%align 16)
    (%label interrupts:resume)
    ,@(map (lambda (reg) `(pop ,reg))
           (reverse all-registers))
    (movdqa xmm2 (mem128+ rsp ,(* 16 2)))
    (movdqa xmm1 (mem128+ rsp ,(* 16 1)))
    (movdqa xmm0 (mem128+ rsp ,(* 16 0)))
    (add rsp ,(* 16 3))
    (pop (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR))) ;restore process-vector
    (iretq)))

(define (data)
  `((%align 8 0)
    (%label idtr)
    (%u16 (- idt-end idt 1))
    (%u64 idt)

    (%align 8 0)
    (%label idt)                      ;Interrupt Descriptor Table
    (%u128 ,@(vector->list (car idt-entries)))
    (%label idt-end))))
