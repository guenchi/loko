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

;;; Boot loader-independent amd64 assembler runtime.

(library (loko arch amd64 lib)
  (export
    assemble
    text-start text text-end
    text-allocate-per-cpu-vector
    text-allocate-per-cpu-stack
    per-cpu-stack-size
    data-start data data-end)
  (import
    (only (loko arch amd64 memory) heap-area stack-area
          STACK-SIZE HEAP-SIZE)
    (only (loko arch amd64 registers) RFLAGS)
    (loko arch amd64 lib-gc)
    (loko arch amd64 lib-printer)
    (loko arch amd64 lib-stacks)
    (loko arch amd64 lib-traps)
    (loko arch amd64 lib-valgrind)
    (loko arch amd64 objects)
    (loko libs context)
    (loko config)
    (rnrs (6))
    (only (machine-code assembler x86) assemble)) ;reexported

(define NR-hlt -1)                      ;XXX: just here temporarily

(define (text-start)
  `((%section text)
    (%label text)
    (%mode 64)))

;; This stack is used to handle interrupts/signals that can happen
;; any time but should not ruin the process currently running. XXX:
;; The stack here is only for Linux? Should be 4096 in that case so
;; it can be munmap'd.
(define per-cpu-stack-size 2048)      ;at least MINSIGSTKSZ
(define per-cpu-vector-size 128)      ;positive multiple of 64

(define (text-allocate-per-cpu-vector error-label)
  ;; This code is used in the bootloader-specific startup routine.
  ;; It returns a value in rax which should be loaded into the fs
  ;; segment base. The error-label is jumped to if the maximum
  ;; number of supported CPU's has been exceeded.
  `((mov ebx ,(immediate 1))
    (lock.xadd (mem+ cpu-counter) ebx) ;ebx = cpu number
    (cmp rbx ,(immediate (config-max-cpus)))
    (jnb ,error-label)                ;too many CPUs?
    (imul eax ebx ,(/ per-cpu-vector-size (immediate 1)))
    (add rax per-cpu-vector)
    (mov (mem64+ rax ,(immediate CPU-VECTOR:CPU-NUMBER)) rbx)))

(define (text-allocate-per-cpu-stack)
  ;; This code is used in the bootloader-specific startup routine.
  ;; It returns a stack pointer base in rax. It must be used after
  ;; text-allocate-per-cpu-vector.
  `((mov eax ,per-cpu-stack-size)
    (lock.xadd (mem64+ per-cpu-stack-offset) rax)
    (add rax per-cpu-stacks)))

(define (text)
  ;; TODO: Should be in one common place.
  (define %heap-rem 'r13)
  (define %alloc 'r14)
  (define %arg-reg* '(rdi rsi rdx rcx r8 r9))

  (define (pvec i) `(,(+ 8 (* 8 i) (- (tag 'vector)))))

  `((%comm *debug-put-u8 16 16)        ;routine
    (%comm *panic 16 16)               ;routine
    (%comm boot-loader-type 16 16)     ;immsym
    (%comm boot-loader-data 16 16)     ;fixnum
    (%comm cpu-counter 8 8)

    ;; Per-CPU data. This is aligned to cache line size (assumed to
    ;; be 64) and each CPU owns a cache line. At start time the fs
    ;; segment base points to the start of a 64-bit area in this
    ;; vector.
    (%comm per-cpu-vector ,(* (config-max-cpus) per-cpu-vector-size) 64)
    ;; Stacks for e.g. timer interrupt/signal. XXX: not used by pc-*.
    (%comm per-cpu-stacks ,(* (config-max-cpus) per-cpu-stack-size) 16)
    (%comm per-cpu-stack-offset 8 8)

    ;; Should be removed when the runtime doesn't try to use them
    ;; under Linux.
    (%align 8)
    (%label $tmp-cli)
    (pushfq)
    (and (mem64+ rsp) ,(fxnot (RFLAGS IF)))
    (popfq)
    (ret)
    (%label $tmp-sti)
    (pushfq)
    (or (mem64+ rsp) ,(RFLAGS IF))
    (popfq)
    (ret)

    ;;;
    ;;; Allocates lists for optional arguments
    ;;;

    (%align 8)
    (%label consargs scheme-init)
    ,@(let ((gc (vector 'gc))
            (gc-now (vector 'gc-now))
            (gc-done (vector 'gc-done))
            (cons-regs (vector 'consargs 'regs))
            (loop (vector 'consargs 'loop))
            (exit (vector 'consargs 'exit))
            (reg-label* (map (lambda (reg) (vector 'consargs reg))
                             %arg-reg*))
            (clear-label* (map (lambda (reg) (vector 'consargs-gc reg))
                               %arg-reg*)))
        ;; This is the code that implements rest argument lists.
        ;; Input:     rax      Number of arguments passed (negative)
        ;;            r10      Number of fixed arguments (positive) that
        ;;                       will not be consed
        ;; Output:    r12      Newly allocated list
        ;; Variable:  rbx      Pointer to the next item from the stack
        ;;            r10      Temporary register
        ;;            r11      Arguments left to cons up
        ;;            r12      The list being constructed
        ;; Preserved: rax rdi rsi rdx rcx r8 r9 r15 rbx rbp
        `((lea r11 (mem+ rax r10))    ;=-argcount+fixedargs
          (add ,%heap-rem r11)
          (add ,%heap-rem r11)
          (js ,gc)                    ;get more heap

          ;; There is now space for the list.
          (%label ,gc-done)
          (push rbx)                  ;get another temporary register
          (neg r11)                   ;=remaining conses
          (mov r12d ,(immediate '()))
          (lea rbx (mem+ rsp 8 8 ,(immediate (length %arg-reg*))))
          (push r10)                  ;TODO: this is silly
          (test r11 r11)
          (jz ,exit)

          ;; Cons arguments from the stack.
          (%label ,loop)
          (mov r10 (mem64+ rsp))      ;fixed
          (add r10 r11)               ;fixed+remaining
          (cmp r10 ,(immediate (length %arg-reg*)))
          (jle ,cons-regs)            ;nothing left on the stack?
          (mov r10 (mem64+ rbx))
          (mov (mem64+ ,%alloc) r10)
          (mov (mem64+ ,%alloc 8) r12)
          (lea r12 (mem+ ,%alloc ,(tag 'pair))) ;set! r12 (cons r10 r12)
          (add ,%alloc 16)
          (add rbx 8)
          (sub r11 8)
          (jnz ,loop)
          (jmp ,exit)

          ;; And now cons the register-passed arguments. Some kind
          ;; of Duff's device. KISS for now. This code finds which
          ;; register to start the consing from.
          (%label ,cons-regs)
          (test r11 r11)
          (jz ,exit)
          (mov r10 (mem64+ rsp))
          (add r10 r11)               ;fixed+remaining
          ,@(let lp ((label* reg-label*) (i 1))
              (if (null? label*)
                  '()
                  `((cmp r10 ,(immediate i))
                    (je ,(car label*))
                    ,@(lp (cdr label*) (fx+ i 1)))))
          (jmp ,exit)
          ;; The generated code conses up to "r11" arguments from
          ;; the registers.
          ,@(let lp ((reg* (reverse %arg-reg*))
                     (label* (reverse reg-label*)))
              (if (null? reg*)
                  '()
                  `((%label ,(car label*))
                    (mov (mem64+ ,%alloc) ,(car reg*))
                    (mov (mem64+ ,%alloc 8) r12)
                    (lea r12 (mem+ ,%alloc ,(tag 'pair)))
                    (add ,%alloc 16)
                    ,@(if (null? (cdr reg*))
                          '()
                          `((sub r11 8)
                            (jz ,exit)
                            ,@(lp (cdr reg*) (cdr label*)))))))

          (%label ,exit)
          (pop r10)
          (pop rbx)
          (ret)

          ;; Prepare to call the GC. First clear the dead argument
          ;; registers.
          (%label ,gc)
          ,@(let lp ((label* clear-label*) (i 0))
              (if (null? label*)
                  '()
                  `((cmp rax ,(immediate (fx- i))) ;rax negative
                    (je ,(car label*))
                    ,@(lp (cdr label*) (fx+ i 1)))))
          (jmp ,gc-now)
          ,@(let lp ((reg* %arg-reg*) (label* clear-label*))
              (if (null? reg*)
                  '()
                  `((%label ,(car label*))
                    (xor ,(car reg*) ,(car reg*))
                    ,@(if (null? (cdr reg*))
                          '()
                          (lp (cdr reg*) (cdr label*))))))

          ;; Here rdi will point to the return address in the
          ;; caller's frame, not the return address of the caller.
          ;; This is because everything in the caller's frame
          ;; currently is live.
          (%label ,gc-now)
          ,@(let ((saved (append '(rax r10 r11) %arg-reg*)))
              `(,@(map (lambda (reg) `(push ,reg)) saved)
                (lea rdi (mem+ rsp 8 8 ,(* 8 (length saved)))) ;adjusted in stop-and-copy
                (sub rdi rax)
                (call stop-and-copy)
                ,@(map (lambda (reg) `(pop ,reg)) (reverse saved))))
          (jmp ,gc-done)))

    ;;;
    ;;; Setup the process environment and start Scheme.
    ;;;
    (%align 8)
    (%label scheme-init process-init)
    ;; Jumped to by linux:start or multiboot:start. At this point
    ;; heap and stack area 0 are mapped into VM. Switch to area #0.

    ,@(let ((stop (vector 'stop))
            (cont (vector 'cont)))
        ;; FIXME: Only CPU 0 has a stack and heap...
        `((mov r10 (mem64+ fs ,(* 8 CPU-VECTOR:CPU-NUMBER)))
          (test r10 r10)
          (jz ,cont)
          (%label ,stop)
          (mov rax ,NR-hlt)
          (syscall)
          (jmp ,stop)
          (%label ,cont)))

    (mov rsp ,(+ (stack-area 0) STACK-SIZE (- PROCESS-SAVE-SIZE)))
    (mov ,%alloc ,(heap-area 0))
    (mov ,%heap-rem ,(div HEAP-SIZE 2))

    ;; Initialize the per-cpu-vector. CPU-VECTOR:CPU-NUMBER is
    ;; already set.
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:LENGTH)) ,(immediate (/ (- per-cpu-vector-size 8) 8)))
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:LAST-INTERRUPT-VECTOR)) ,(immediate #f))
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-RUNNING?)) ,(immediate #t)) ;scheduler running
    (%comm global-environment 8 16)
    (call init-global-environment)    ;XXX: for lack of EQU
    (call allocate-process-vector)

    ;; Set memory management parameters.
    (mov rax (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (mov rdx ,(heap-area 0))
    (mov (mem64+ rax ,@(pvec PROCESS-VECTOR:START-CURRENT-HEAP)) rdx)
    (mov ecx ,(div HEAP-SIZE 2))
    (mov (mem64+ rax ,@(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)) rcx)
    (mov (mem64+ rax ,@(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)) rcx)
    (add rdx rcx)
    (mov (mem64+ rax ,@(pvec PROCESS-VECTOR:START-OTHER-HEAP)) rdx)
    (mov (mem64+ rax ,@(pvec PROCESS-VECTOR:STACK-TOP)) rsp)
    (mov edx ,(- (- STACK-SIZE PROCESS-SAVE-SIZE) 4096))
    (mov (mem64+ rax ,@(pvec PROCESS-VECTOR:STACK-SIZE)) rdx)

    ;; Start with zeroed registers
    ,@(map (lambda (r) `(xor ,r ,r))
           '(eax ecx edx ebx ebp esi edi r8d r9d r10d r11d r12d #;r13d #;r14d r15d))
    (call scheme-start)
    (mov rdi ,(immediate 'start))
    (jmp (mem64+ *panic))

    ;;;
    ;;; The entry point for newly created processes.
    ;;;
    (%align 8)
    (%label process-init allocate-process-vector)
    ;; This routine is called from a scheduler process that is
    ;; starting up a new process. RSP is already set. XXX: It is
    ;; stupid to update boot-loader-type every time. It is needed
    ;; once. TODO: this routine actually needs to allocate a vector
    ;; to store per-process info the runtime needs. XXX: this could
    ;; be scheme-init instead, if the bootloader code sets things up
    ;; like this.
    (mov rax ,(immediate 'scheme))
    (mov (mem64+ boot-loader-type) rax)
    (mov (mem64+ boot-loader-data) 0)
    (pop r14)
    (pop r13)
    (call allocate-process-vector)
    (mov rax (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (pop (mem64+ rax ,@(pvec PROCESS-VECTOR:START-CURRENT-HEAP)))
    (pop (mem64+ rax ,@(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)))
    (pop (mem64+ rax ,@(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    (pop (mem64+ rax ,@(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    (pop (mem64+ rax ,@(pvec PROCESS-VECTOR:STACK-TOP)))
    (pop (mem64+ rax ,@(pvec PROCESS-VECTOR:STACK-SIZE)))
    ,@(map (lambda (r) `(xor ,r ,r))
           '(eax ecx edx ebx ebp esi edi r8d r9d r10d r11d r12d #;r13d #;r14d r15d))
    (popfq)                           ;also enables interrupts
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-RUNNING?)) ,(immediate #f)) ;not currently yielding
    (ret)

    ;;;
    ;;; Allocate a process vector.
    ;;;
    (%align 8)
    (%label allocate-process-vector)
    ;; Build the process vector. Assumes that the heap can fit the
    ;; vector...
    (mov rdx (mem64+ global-environment)) ;XXX:size of process-vector. implement EQU?
    (mov (mem64+ ,%alloc) rdx)
    (lea rax (mem64+ ,%alloc ,(tag 'vector)))
    (add rdx 15)
    (and rdx -16)
    (add ,%alloc rdx)
    (sub ,%heap-rem rdx)
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)) rax)
    ;; Set default error invoker.
    (mov (mem64+ rax ,@(pvec PROCESS-VECTOR:ERROR-INVOKER))
         (+ *panic ,(tag 'procedure)))
    (ret)

;;; Other libraries

    ,@(lib-gc:text)
    ,@(lib-printer:text)
    ,@(lib-stacks:text)
    ,@(lib-traps:text)
    ,@(lib-valgrind:text)))

(define (text-end)
  `((%label text-end)))

(define (data-start)
  `((%align 4096 0)
    (%label data)
    (%section data)))

(define (data)
  `(,@(lib-gc:data)
    ,@(lib-printer:data)
    ,@(lib-stacks:data)
    ,@(lib-traps:data)
    ,@(lib-valgrind:data)))

(define (data-end)
  `((%label data-end)
    ;; Put uninitialized data here
    (%align 4096 0)
    (%label bss)
    (%section bss)
    (%align 4 0)
    (%label bss-end))))
