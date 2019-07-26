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

;;; Loko runtime for Linux on amd64

(library (loko arch amd64 linux-start)
  (export
    image-header image-footer
    text-start text
    data)
  (import
    (loko arch amd64 linux-numbers)
    (only (loko arch amd64 memory)
          stack-area heap-area PAGE-SIZE STACK-SIZE HEAP-SIZE)
    (loko arch amd64 registers)
    (loko arch amd64 objects)
    (loko libs context)
    (prefix (loko arch amd64 lib) lib:)
    (loko config)
    (rnrs)
    (machine-code assembler elf)
    (machine-code format elf))

;;; ELF

;; Linux Standard Base
(define PT_GNU_EH_FRAME #x6474e550)
(define PT_GNU_STACK #x6474e551)
(define PT_GNU_RELRO #x6474e552)

(define elf:start (vector 'elf 'start))
(define elf:segments (vector 'elf 'segments))
(define elf:sections (vector 'elf 'sections))

(define segments '(text data bss stack0 heap0 gnu-stack))

(define sections
  (make-string-table '("" ".text" ".data" ".bss"
                       ".shstrtab" ".note.ABI-tag" ".strtab" ".symtab"
                       ".debug_gdb_scripts")))

(define (elf64-header entry)
  ;; This header must be at the very start of the image.
  `((%label ,elf:start)
    ,@(elf-64-assembler (make-elf-image
                         #f ELFCLASS64 ELFDATA2LSB ELFOSABI-SYSV 0
                         ET-EXEC EM-X86-64 EV-CURRENT
                         entry
                         (if (null? segments) 0 `(- ,elf:segments ,elf:start))
                         (if (string-table-empty? sections) 0 `(- ,elf:sections ,elf:start))
                         0 #f #f (length segments) #f (string-table-size sections)
                         (or (string-table-list-index sections ".shstrtab")
                             SHN-UNDEF)))))

(define (image-header)
  (elf64-header 'linux:start))

(define (mangle str)
  ;; gdb can't handle all the symbols that Loko uses
  (call-with-string-output-port
    (lambda (p)
      (string-for-each (lambda (c)
                         (case c
                           ((#\-) (display #\_ p))
                           ((#\:) (display "_c_" p))
                           ((#\/) (display "_s_" p))
                           ((#\!) (display "_ex" p))
                           ((#\?) (display "_p" p))
                           ((#\*) (display "_star" p))
                           (else (put-char p c))))
                       str))))

(define (image-footer)
  (define sym-labels (make-eq-hashtable))
  (define :shstrtab (vector 'elf 'shstrtab))
  (define :shstrtab-end (vector 'elf 'shstrtab-end))
  (define :strtab (vector 'elf 'strtab))
  (define :strtab-end (vector 'elf 'strtab-end))
  (define :symtab (vector 'elf 'symtab))
  (define :symtab-end (vector 'elf 'symtab-end))
  (define :debug_gdb_scripts (vector 'elf 'debug_gdb_scripts))
  (define :debug_gdb_scripts-end (vector 'elf 'debug_gdb_scripts_end))
  (define :abi (vector 'elf 'abi))
  (define :abi-end (vector 'elf 'abi-end))
  ;; The ELF header contains pointers to everything here
  `((%call ,(lambda (assemble! port ip symbols bss _end)
              ;; Rewind to before the BSS was inserted
              (- (- _end bss)))
           bss bss-end)
    (%section elf)
    (%label ,elf:segments)
    ;; Text, data and bss as separate segments.
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-X)
                                          0 elf:start elf:start
                                          `(- text-end ,elf:start)
                                          `(- text-end ,elf:start)
                                          (expt 2 12)))
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R)
                                          `(- data ,elf:start) 'data 'data
                                          `(- data-end data)
                                          `(- data-end data)
                                          (expt 2 12)))
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-W)
                                          0 'bss 'bss
                                          0 `(- bss-end bss)
                                          (expt 2 12)))
    ;; Stack and heap for thread area #0.
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-W)
                                          0 (fx+ (stack-area 0) PAGE-SIZE)
                                          (fx+ (stack-area 0) PAGE-SIZE)
                                          0 (fx- STACK-SIZE PAGE-SIZE) (expt 2 12)))
    ,@(elf-64-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-W)
                                          0 (heap-area 0) (heap-area 0)
                                          0 HEAP-SIZE (expt 2 12)))
    ;; The initial stack should not be executable. The lack of PF-X
    ;; here also makes Linux turn off READ_IMPLIES_EXEC, which affects
    ;; the segments above.
    ,@(elf-64-assembler (make-elf-segment PT_GNU_STACK (bitwise-ior PF-R PF-W)
                                          0 0 0 0 0 16))

    ;; section headers
    (%label ,elf:sections)
    ,@(elf-64-assembler (make-elf-section 0 SHT-NULL 0 0 0 0 0 0 0 0))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".text")
                         SHT-PROGBITS
                         (bitwise-ior SHF-EXECINSTR SHF-ALLOC)
                         'text `(- text ,elf:start) '(- text-end text)
                         0 0 0 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".data")
                         SHT-PROGBITS (bitwise-ior SHF-ALLOC)
                         'data
                         `(- data ,elf:start) '(- data-end data)
                         0 0 (expt 2 12) #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".bss")
                         SHT-NULL (bitwise-ior SHF-WRITE SHF-ALLOC)
                         'bss
                         0 '(- bss-end bss)
                         0 0 (expt 2 12) #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".shstrtab")
                         SHT-STRTAB 0 0
                         `(- ,:shstrtab ,elf:start)
                         `(- ,:shstrtab-end ,:shstrtab)
                         0 0 0 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".note.ABI-tag")
                         SHT-NOTE 0 0
                         `(- ,:abi ,elf:start)
                         `(- ,:abi-end ,:abi)
                         0 0 8 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".strtab")
                         SHT-STRTAB 0 0
                         `(- ,:strtab ,elf:start)
                         `(- ,:strtab-end ,:strtab)
                         0 0 0 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".symtab")
                         SHT-SYMTAB 0 0
                         `(- ,:symtab ,elf:start)
                         `(- ,:symtab-end ,:symtab)
                         (string-table-list-index sections ".strtab")
                         0 0 #f))
    ,@(elf-64-assembler (make-elf-section
                         (string-table-byte-index sections ".debug_gdb_scripts")
                         SHT-PROGBITS 0 0
                         `(- ,:debug_gdb_scripts ,elf:start)
                         `(- ,:debug_gdb_scripts-end ,:debug_gdb_scripts)
                         0 0 0 #f))

    ;; Section name table.
    (%label ,:shstrtab)
    (%vu8 ,(string-table-bytes sections))
    (%label ,:shstrtab-end)

    ;; ABI tag. This should be using u64's according to "ELF-64
    ;; object file format", but "Linux Standard Base" disagrees.
    (%align 4 0)
    (%label ,:abi)
    (%u32 ,(bytevector-length (string->utf8 "GNU\x0;")))
    (%u32 ,(* 4 (length '(0 2 6 32))))
    (%u32 1)                          ;GNU/Linux
    (%align 4 0)
    (%vu8 ,(string->utf8 "GNU\x0;"))
    (%align 4 0)
    (%u32 ,@'(0 2 6 18))              ;needs at least 2.6.18
    (%label ,:abi-end)

    ;; The strings for the symbol table
    (%label ,:strtab)
    (%utf8z "")
    (%call ,(lambda (assemble! port ip symbols)
              ;; TODO: this prints some strings that will not be used
              (for-each
               (lambda (sym)
                 (let ((name (car sym)))
                   (when (symbol? name)
                     (cond ((hashtable-ref sym-labels name #f) =>
                            (lambda (label)
                              (assemble! `(%label ,label))))
                           (else
                            (let ((label (vector 'strtab name)))
                              (hashtable-set! sym-labels name label)
                              (assemble! `(%label ,label)))))
                     (assemble! `(%utf8z ,(mangle (symbol->string name)))))))
               symbols)
              0))
    (%label ,:strtab-end)

    ;; The very symbol table itself
    ;;(%align 8 0)
    (%label ,:symtab)
    ,@(elf-64-assembler (make-elf-symbol 0 0 0 0 0 0 0))
    (%call ,(lambda (assemble! port ip symbols)
              (define :TEXT (string-table-list-index sections ".text"))
              (define :DATA (string-table-list-index sections ".data"))
              (define (elf-symbol sym str-label)
                (let* ((start (car sym))
                       (end (cadr sym))
                       (section (caddr sym))
                       (misc (cdddr sym)))
                  (if (or (not section)
                          (eq? start 'text))
                      '()
                      (elf-64-assembler
                       (make-elf-symbol `(- ,str-label ,:strtab)
                                        STB-GLOBAL
                                        (case section
                                          ((text) STT-FUNC)
                                          ((bss data) STT-OBJECT)
                                          (else STT-NOTYPE))
                                        0
                                        (case section
                                          ((text) :TEXT)
                                          ((bss) SHN-COMMON)
                                          ((data) :DATA)
                                          ;; SHN-ABS for anything?
                                          (else SHN-UNDEF))
                                        ;; TODO: these look weird in
                                        ;; objdump for the bss
                                        start
                                        `(- ,end ,start))))))
              (for-each (lambda (symbol)
                          (when (symbol? (car symbol))
                            (for-each assemble!
                                      (elf-symbol
                                       symbol
                                       (hashtable-ref sym-labels (car symbol) 0)))))
                        symbols)
              0))
    (%label ,:symtab-end)

    ;; Debug scripts for GDB
    (%label ,:debug_gdb_scripts)
    (%u8 1)                           ;Python
    (%utf8z "src/loko/arch/amd64/loko-gdb.py")
    (%label ,:debug_gdb_scripts-end)))

;;; .text

;; This is at the very start of the binary image. Its job is to make
;; an ELF image, and before entering scheme-start, set up the basic
;; environment.
(define (text-start)
  (define (pvec i) `(,(+ 8 (* 8 i) (- (tag 'vector)))))
  `((%align 8)
    (%mode 64)
    ;;;
    ;;; ELF entry point.
    ;;;
    (%label linux:start signal-return)
    ;; Enable alignment checking.
    (pushfq)
    (or (mem32+ rsp) ,(RFLAGS AC))
    (popfq)
    ;; Set up boot-loader specific subroutines.
    (mov (mem64+ *debug-put-u8) linux:debug-put-u8)
    (mov (mem64+ *panic) linux:panic)

    ;; On the stack now: argc, argv, envp, auxv (AT_* from
    ;; /usr/include/elf.h), padding, argument strings, environment
    ;; strings, end marker
    (mov rax rsp)
    (sal rax 3)
    (mov (mem64+ boot-loader-data) rax)
    (mov rax ,(immediate 'linux))
    (mov (mem64+ boot-loader-type) rax)

    ;; Setup FS and allocate an alternate signal stack
    ,@(lib:text-allocate-per-cpu-vector 'linux:panic)
    ;;(mov (mem64+ rax ,(+ 8 (* 8 4))) rax)
    ,@(syscall __NR_arch_prctl ARCH_SET_FS 'rax)
    ,@(lib:text-allocate-per-cpu-stack)
    (sal rax ,(shift 'fixnum))
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:ALTSIGSTK-BASE)) rax)
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:ALTSIGSTK-SIZE)) ,(immediate lib:per-cpu-stack-size))

    (jmp scheme-init)

    ;;;
    ;;; Returns from a signal handler (given in rt_sigaction). This
    ;;; becomes the return address in signal handler stack frames.
    ;;;
    ;; The arguments here are as follows: int signo, siginfo_t *,
    ;; ucontext_t *. They are passed in rdi, rsi and rdx.
    (%align 8)
    (%label signal-return signal-handler)
    ;; This can only be done when ONSTACK has been used, because
    ;; otherwise the Scheme stack has been smashed and returning is
    ;; meaningless.
    ,@(syscall __NR_rt_sigreturn 0)

    ;;;
    ;;; Scheme code makes this the signal handler for everything
    ;;; that is a runtime error (type errors mainly).
    ;;;
    (%align 8)
    (%label signal-handler)
    ;; This needs to restore the original rsp because otherwise
    ;; there will be junk on the stack which the GC will hate.
    ;; Besides, the program will probably be executing on the
    ;; alternate signal stack. TODO: It should probably provide the
    ;; error invokers with the registers so that it can give more
    ;; informative error messages. It should look at (mem32+ rsi
    ;; ,(format-size "2L")) to see if there's a SI_USER code.
    (mov r15 (mem+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-r15)))
    (mov r14 (mem+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-r14)))
    (mov r13 (mem+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-r13)))
    (mov rbx (mem+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-bx)))
    (mov rbp (mem+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-bp)))
    (mov eax (mem+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-trapno)))
    (mov rcx (mem+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-ip)))
    (mov rsp (mem+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-sp)))
    (push (mem64+ rdx ,(+ offsetof-ucontext-uc_mcontext offsetof-sigcontext_64-flags)))
    (popfq)
    (push rcx)                        ;RIP
    ,@(map (lambda (r) `(xor ,r ,r))  ;who knows what Linux put here
           '(ecx edx esi edi r8d r9d r10d r11d r12d))
    (test eax eax)                    ;#DE
    (je divide-error)
    (cmp eax 12)                      ;#SS
    (je noncanonical-address)
    (cmp eax 13)                      ;#GP
    (je noncanonical-address)
    ;; TODO: #PF has several types of causes
    (cmp eax 14)                      ;#PF
    (je invalid-address)
    (cmp eax 17)                      ;#AC
    (je alignment-check)
    (cmp eax 1)                       ;#DB
    (je debug-exception)
    (cmp eax 3)                       ;#BP
    (je breakpoint)
    (cmp eax 6)                       ;#UD
    (je undefined-opcode)
    ;; OK... what to do...? Some more generic error invoker is
    ;; needed.
    (jmp (mem64+ *panic))

    ;;;
    ;;; The preemption timer has fired. It's time for the current
    ;;; process to scurry away and let the scheduler run. SIGURG is
    ;;; masked when the scheduler runs.
    ;;;
    (%align 8)
    (%label linux:preempt)
    ,@(let ((preempt (vector 'preempt))
            (exit (vector 'exit)))
        `((mov rbp (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-SP)))
          (test rbp rbp)
          (jz ,exit)                  ;is the scheduler already running?
          (mov eax ,(immediate #f))
          (mov edx ,(immediate #t))
          (cmpxchg (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-RUNNING?)) rdx)
          (je ,preempt)                 ;not already yielding?
          (%label ,exit)
          (ret)
          (%label ,preempt)
          ;; Save the alternate stack in the process's save area. It
          ;; is important that the alternate stack + 8 can't be
          ;; bigger than the save area. DF is clear (AMD64 ABI).
          (mov rbx (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
          (mov rdi (mem64+ rbx ,@(pvec PROCESS-VECTOR:SAVE-AREA)))
          (mov rcx (mem64+ fs ,(* 8 CPU-VECTOR:ALTSIGSTK-BASE))) ;alternate stack
          (add rcx (mem64+ fs ,(* 8 CPU-VECTOR:ALTSIGSTK-SIZE)))
          (sar rcx ,(shift 'fixnum))
          (sub rcx rsp)               ;rcx = bytes to save
          (mov rsi rsp)               ;copy destination
          (shr rcx 3)                 ;rcx = quads to save
          ;; Build a stack frame for resuming the process
          (mov eax linux:resume)
          (stos (mem64+ rdi) rax)     ;push return address for scheduler
          (mov rax rbx)
          (stos (mem64+ rdi) rax)     ;save process vector
          (mov rax rcx)
          (stos (mem64+ rdi) rax)     ;push size of the state
          (mov rax rsp)
          (stos (mem64+ rdi) rax)     ;this stack pointer
          ;; Copy the state that Linux saved
          (rep.movs (mem64+ rdi) (mem64+ rsi))
          ;; Switch back to the scheduler.
          (mov rdi rbp)               ;scheduler's rsp
          (sar rdi ,(shift 'fixnum))
          (mov rsp (mem64+ rbx ,@(pvec PROCESS-VECTOR:SAVE-AREA))) ;rsp for resume
          (mov rax ,(immediate 'preempted)) ;TODO: must be unforgeable
          (jmp switch-stack)))

    ;;;
    ;;; The reverse of linux:preempt. Returned into from switch-stack.
    ;;; Note: SIGURG must be masked.
    ;;;
    (%align 8)
    (%label linux:resume)
    (pop (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (pop rcx)                         ;quads to restore
    (pop rdi)                         ;copy destination
    (mov rsi rsp)                     ;copy source
    ;; Seems that the next instruction disables SIGURG delivery
    (mov rsp rdi)                     ;restore alternate stack pointer
    (rep.movs (mem64+ rdi) (mem64+ rsi)) ;restore sigreturn data
    (mov (mem64+ fs ,(* 8 CPU-VECTOR:SCHEDULER-RUNNING?)) ,(immediate #f)) ;not currently yielding
    (ret)

    ;;;
    ;;; A little helper to let gdb access the process vector.
    ;;;
    (%align 8)
    (%label linux:gdb:pvec)
    (mov rax (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (ret)

    ;;; afl shared memory area (remapped later). Make it twice as
    ;;; large, so that the fork server has somewhere to write to
    ;;; which is not the same area as the instrumented children.
    (%comm afl-map ,(* 2 (expt 2 16)) ,PAGE-SIZE)
    ;; (%comm afl-location 8 8)
    ))

;;; Standard library

(define (syscall syscall-number . args)
  (let lp ((reg64* '(rdi rsi rdx r10 r8 r9))
           (reg32* '(edi esi edx r10d r8d r9d))
           (arg* args)
           (code `((mov eax ,syscall-number)
                   (syscall))))
    (cond ((null? arg*)
           code)
          ((null? reg64*)
           (error 'syscall "Too many arguments to Linux amd64 system call" syscall-number args))
          (else
           (let ((arg (car arg*)))
             (lp (cdr reg64*)
                 (cdr reg32*)
                 (cdr arg*)
                 (cons (cond
                         ;; XXX: The peephole optimizer doesn't touch this code.
                         ((eqv? arg 0)
                          `(xor ,(car reg32*) ,(car reg32*)))
                         ((and (fixnum? arg) (fx<=? arg #xffffffff))
                          `(mov ,(car reg32*) ,arg))
                         (else
                          `(mov ,(car reg64*) ,arg)))
                       code)))))))

(define (text)
  `((%align 8)
    (%label linux:debug-put-u8 linux:panic)
    (push rdi)                        ;rdi = octet
    ,@(syscall __NR_write STDERR_FILENO 'rsp 1)
    (pop rdi)
    (ret)

    (%align 8)
    (%label linux:panic)
    (push rdi)
    ,@(syscall __NR_write STDERR_FILENO 'linux:panic-msg0
               '(- linux:panic-msg0-end linux:panic-msg0 1))
    (pop rdi)
    (call debug-display)
    ,@(syscall __NR_write STDERR_FILENO 'linux:panic-msg1
               '(- linux:panic-msg1-end linux:panic-msg1 1))
    ,@(syscall __NR_exit EX_SOFTWARE)))

;;; .data

(define (data)
  `((%align 8 0)
    (%label afl-location)
    (%u64 #x10000)                    ;Initially writes outside the area.
    (%utf8z "This Scheme image has Linux amd64 support")
    (%label linux:panic-msg0)
    (%utf8z "\x1b;[0;1;31mLoko Scheme panic: \x1b;[33m")
    (%label linux:panic-msg0-end)
    (%label linux:panic-msg1)
    (%utf8z "\r\n")
    (%label linux:panic-msg1-end))))
