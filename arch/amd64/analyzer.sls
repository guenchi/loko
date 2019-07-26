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

;;; Assembler analyzer used by pass-optimize.

(library (loko arch amd64 analyzer)
  (export
    instruction-analyzer
    target-convention)
  (import
    (rnrs)
    (loko match)
    (only (loko arch amd64 objects) immediate))

;; (define mnemonic-aliases
;;    '((jnae . jb) (setnae . setb) (cmovnae . cmovb)
;;      (jae . jnb) (setae . setnb) (cmovae . cmovnb)
;;      (je . jz) (sete . setz) (cmove . cmovz)
;;      (jne . jnz) (setne . setnz) (cmovne . cmovnz)
;;      (jna . jbe) (setna . setbe) (cmovna . cmovbe)
;;      (ja . jnbe) (seta . setnbe) (cmova . cmovnbe)
;;      (jpe . jp) (setpe . setp) (cmovpe . cmovp)
;;      (jpo . jnp) (setpo . setnp) (cmovpo . cmovnp)
;;      (jnge . jl) (setnge . setl) (cmovnge . cmovl)
;;      (jge . jnl) (setge . setnl) (cmovge . cmovnl)
;;      (jng . jle) (setng . setle) (cmovng . cmovle)
;;      (jg . jnle) (setg . setnle) (cmovg . cmovnle)
;;      (jc . jb) (setc . setb) (cmovc . cmovb)
;;      (jnc . jnb) (setnc . setnb) (cmovnc . cmovnb)))

;; Maps register name to the class and index of the register they
;; are part of.
(define register-list
  '(;; 64-bit
    (rax 64 0)
    (rcx 64 1)
    (rdx 64 2)
    (rbx 64 3)
    (rsp 64 4)
    (rbp 64 5)
    (rsi 64 6)
    (rdi 64 7)
    (r8 64 8)
    (r9 64 9)
    (r10 64 10)
    (r11 64 11)
    (r12 64 12)
    (r13 64 13)
    (r14 64 14)
    (r15 64 15)

    ;; 8-bit encodable without REX
    (al 8 0)
    (cl 8 1)
    (dl 8 2)
    (bl 8 3)
    (ah norex8 0)                     ;encoded as 4
    (ch norex8 1)                     ;5
    (dh norex8 2)                     ;6
    (bh norex8 3)                     ;7

    ;; 8-bit encodable with REX
    (spl rex8 4)
    (bpl rex8 5)
    (sil rex8 6)
    (dil rex8 7)
    (r8b rex8 8)
    (r9b rex8 9)
    (r10b rex8 10)
    (r11b rex8 11)
    (r12b rex8 12)
    (r13b rex8 13)
    (r14b rex8 14)
    (r15b rex8 15)
    ;; aliases
    (r8l rex8 8)
    (r9l rex8 9)
    (r10l rex8 10)
    (r11l rex8 11)
    (r12l rex8 12)
    (r13l rex8 13)
    (r14l rex8 14)
    (r15l rex8 15)

    ;; 16-bit
    (ax 16 0)
    (cx 16 1)
    (dx 16 2)
    (bx 16 3)
    (sp 16 4)
    (bp 16 5)
    (si 16 6)
    (di 16 7)
    (r8w 16 8)
    (r9w 16 9)
    (r10w 16 10)
    (r11w 16 11)
    (r12w 16 12)
    (r13w 16 13)
    (r14w 16 14)
    (r15w 16 15)

    ;; 32-bit
    (eax 32 0)
    (ecx 32 1)
    (edx 32 2)
    (ebx 32 3)
    (esp 32 4)
    (ebp 32 5)
    (esi 32 6)
    (edi 32 7)
    (r8d 32 8)
    (r9d 32 9)
    (r10d 32 10)
    (r11d 32 11)
    (r12d 32 12)
    (r13d 32 13)
    (r14d 32 14)
    (r15d 32 15)

    ;; 64-bit vector
    (mm0 mm 0)
    (mm1 mm 1)
    (mm2 mm 2)
    (mm3 mm 3)
    (mm4 mm 4)
    (mm5 mm 5)
    (mm6 mm 6)
    (mm7 mm 7)
    ;; aliases:
    (mmx0 mm 0)
    (mmx1 mm 1)
    (mmx2 mm 2)
    (mmx3 mm 3)
    (mmx4 mm 4)
    (mmx5 mm 5)
    (mmx6 mm 6)
    (mmx7 mm 7)

    ;; 128-bit vector
    (xmm0 xmm 0)
    (xmm1 xmm 1)
    (xmm2 xmm 2)
    (xmm3 xmm 3)
    (xmm4 xmm 4)
    (xmm5 xmm 5)
    (xmm6 xmm 6)
    (xmm7 xmm 7)
    (xmm8 xmm 8)
    (xmm9 xmm 9)
    (xmm10 xmm 10)
    (xmm11 xmm 11)
    (xmm12 xmm 12)
    (xmm13 xmm 13)
    (xmm14 xmm 14)
    (xmm15 xmm 15)

    ;; 256-bit vector
    (ymm0 ymm 0)
    (ymm1 ymm 1)
    (ymm2 ymm 2)
    (ymm3 ymm 3)
    (ymm4 ymm 4)
    (ymm5 ymm 5)
    (ymm6 ymm 6)
    (ymm7 ymm 7)
    (ymm8 ymm 8)
    (ymm9 ymm 9)
    (ymm10 ymm 10)
    (ymm11 ymm 11)
    (ymm12 ymm 12)
    (ymm13 ymm 13)
    (ymm14 ymm 14)
    (ymm15 ymm 15)

    ;; 512-bit vector
    (zmm0 zmm 0)
    (zmm1 zmm 1)
    (zmm2 zmm 2)
    (zmm3 zmm 3)
    (zmm4 zmm 4)
    (zmm5 zmm 5)
    (zmm6 zmm 6)
    (zmm7 zmm 7)
    (zmm8 zmm 8)
    (zmm9 zmm 9)
    (zmm10 zmm 10)
    (zmm11 zmm 11)
    (zmm12 zmm 12)
    (zmm13 zmm 13)
    (zmm14 zmm 14)
    (zmm15 zmm 15)
    ))

(define register-indexen
  (map (lambda (reg)
         (let ((type (cadr reg))
               (idx (caddr reg)))
           (case type
             ((xmm ymm zmm)
              (cons (car reg) (+ 16 idx)))
             (else
              (cons (car reg) idx)))))
       register-list))

;; These are the registers in register states.
(define register-names
  '#(rax rcx rdx rbx rsp rbp rsi rdi
         r8 r9 r10 r11 r12 r13 r14 r15
         ymm0 ymm1 ymm2 ymm3 ymm4 ymm5
         ymm6 ymm7 ymm8 ymm9 ymm10 ymm11
         ymm12 ymm13 ymm14 ymm15
         rfl))

(define register-names32
  '#(eax ecx edx ebx esp ebp esi edi
         r8d r9d r10d r11d r12d r13d r14d r15d))

(define register-names8l
  '#(al cl dl bl spl bpl sil dil
        r8b r9b r10b r11b r12b r13b r14b r15b))

(define rFLAGS (expt 2 (+ 16 16)))

(define (register-compatible-classes? x y)
  (define (class-of r)
    ;; TODO: r is an index of a hardware register or pseudo register
    ;; from codegen. Right now they are only considered compatible
    ;; with integer registers.
    (cond ((fx<? r 16) 1)             ;rN
          ((fx<? r 32) 2)             ;zmmN
          ((fx=? r 32) 3)             ;rfl
          (else 1)))
  (fx=? (class-of x) (class-of y)))

(define (register-index name)
  (cond ((assq name register-indexen) => cdr)
        ((and (pair? name) (eq? (car name) 'reg))
         (fx+ (vector-length register-names)
              (cadr name)))
        (else #f)))

(define (register-type name)
  (cond ((assq name register-list) => cadr)
        ((and (pair? name) (eq? (car name) 'reg))
         ;; TODO: there should be different types of pseudo registers
         64)
        (else #f)))

(define (register-name idx)
  (if (fx<? idx (vector-length register-names))
      (vector-ref register-names idx)
      `(reg ,(fx- idx (vector-length register-names)))))

(define (register x)
  (let ((i (register-index x)))
    (if i (expt 2 i) 0)))

(define (register-set . x)
  (fold-left bitwise-ior 0 (map register x)))

(define (memory? x)
  (and (pair? x)
       (memq (car x) '(mem64+ mem+ mem8+ mem16+ mem32+))))

(define (mem64? x)
  (and (pair? x)
       (memq (car x) '(mem64+))))

(define (register? x)
  (and (register-index x) #t))

(define target-convention
  (case-lambda
    ((key)
     (case key
       ((unavailable)
        ;; Registers always unavailable for register
        ;; assignment/allocation.
        (register-set 'r13 'r14 'r15 'rsp))
       ((scratch)
        (bitwise-ior
         rFLAGS
         (register-set 'rax 'rcx 'rdx 'rsi 'rdi
                       'r8 'r9 'r10 'r11 'r12
                       'r13 'r14 'r15)))
       ((preserved)
        (register-set 'rbx 'rbp 'rsp))
       ((register-names) register-names)
       ((peephole-optimizer) peephole-optimizer)
       ((return-instruction) '(ret))
       ((liveness-annotator) amd64-liveness-annotator)
       ((copy-propagator) copy-propagator)
       ((register-compatible-classes?) register-compatible-classes?)
       (else
        (error 'target-convention
               "Unknown amd64 host convention key." key))))
    ((key arg)
     (case key
       ((reverse-branch-condition)
        ;; TODO: get all of them
        (cond ((assq arg '((je . jne) (jne . je) (jle . jnle)
                           (jnl . jl) (jl . jnl) (jng . jg)
                           (jg . jng) (jge . jnge) (jnge . jge)
                           (jle . jnle) (jnle . jle)
                           (jbe . jnbe) (jnbe . jbe)
                           (js . jns) (jns . js)
                           (jz . jnz) (jnz . jz)
                           (jo . jno) (jno . jo)
                           (jb . jnb) (jnb . jb)))
               => cdr)
              (else
               (display "WARNING: could not reverse branch condition ")
               (display arg)
               (newline)
               #f)))
       ((valid-branch?)
        (match arg
          [('jmp label)
           (or (vector? label) (pair? label) (symbol? label))]
          [('jcc label)
           (or (vector? label) (symbol? label))]
          [else #f]))
       ((safe-load?)
        ;; A load from the stack or closure has no side-effects.
        (match arg
          (('mov _ ('mem64+ (or 'rsp 'r15) . _) . _) #t)
          (_ #f)))
       (else
        (error 'target-convention
               "Unknown amd64 host convention key." key))))))

(define (copy-propagator inst valid-dst valid-src sources opt+)
  ;; Performs copy propagation as best as possible. This is only
  ;; written to handle code generated by Loko.
  (define (address x)
    (if (memory? x) (operand x) x))
  (define (operand x)
    ;; XXX: maybe r15 should never be replaced.
    (cond ((register-index x) =>
           (lambda (i)
             ;; (print "; copyprop looks at register " i " in " sources)
             ;; (print "; valid-dst: " valid-dst)
             (cond ((not (bitwise-bit-set? valid-dst i))
                    ;; No longer a valid copy
                    x)
                   ;; In the general case the values in
                   ;; sources can have multiple bits set.
                   ;; But on amd64 that never happens,
                   ;; because MOV only work on single
                   ;; registers. XXX: what about 32-bit
                   ;; moves etc?
                   ((assv (expt 2 i) sources) =>
                    (lambda (src)
                      (let ((src (cadr src)))
                        (cond ((or (zero? (bitwise-and valid-src src))
                                   (zero? src))
                               x)
                              (else
                               (opt+)
                               (register-name (bitwise-first-bit-set src)))))))
                   (else x))))
          ((pair? x)
           (cons (operand (car x)) (operand (cdr x))))
          (else x)))
  ;; valid-dst has one bit set for each register which a copy
  ;; has been placed into, and which has not been invalidated.
  ;; valid-src has one bit set for each register from which
  ;; a copy was made and which has not been invalidated.
  ;; sources is a list like this one:
  ;; '((2 64 (mov rcx rsi))
  ;;   (1 8 (mov rax rbx)))
  ;; The first two numbers are bit masks that record the
  ;; sets and uses of the copy. Each entry also comes with
  ;; the instruction that created the copy.
  ;; Beware that the sets might contain pseudo registers or
  ;; stack references.
  (apply
   (case-lambda
     ((mnem dst src)
      (case mnem
        ((add sub mov and or)
         ;; Must not do stuff like
         ;; (mov (mem32+ ...) rax)
         ;; TODO: have the above become (mov (mem32+ ...) eax)
         (cond ((and (memory? src) (not (mem64? src)))
                (list mnem dst (operand src)))
               ((and (memory? dst) (not (mem64? dst)))
                (list mnem (address dst) src))
               (else
                (list mnem (address dst) (operand src)))))

        ((cmp test)
         ;; TODO: implement this for smaller registers
         ;;    (mov rax rcx)
         ;;    (cmp al 15)
         ;; => (cmp cl 15)
         (cond ((memory? src)
                (list mnem dst (operand src)))
               ((and (eqv? (register-type dst) 64)
                     (or (eqv? (register-type src) 64)
                         (number? src)))
                (list mnem (operand dst) (operand src)))
               (else
                inst)))
        ((lea)
         ;; The registers in these addresses always use the full
         ;; register size.
         (list mnem dst (operand src)))
        ((movzx)
         (cond ((memory? src)
                (list mnem dst (operand src)))
               (else
                inst)))
        (else
         ;; (display "TODO: copy propagation: ")
         ;; (write (list inst valid-dst valid-src sources)) (newline)
         inst)))
     (_ inst))
   inst))

;; (varstate->list '32843)
;; (varstate->list '72)

;; (copy-propagator '(test cl 7)
;;                  '32843
;;                  '72
;;                  '((2 64 (mov rcx rsi))
;;                    (1 8 (mov rax rbx)))
;;                  (lambda x (display "OPT+: ") (display x) (newline)))

;; Takes an assembler instruction and returns info, sets and uses.
;; If stack-as-pseudo? is true then (mem64+ rsp ...) will be
;; included in uses and sets (using the same indices as pseudo
;; registers, so do not use stack-as-pseudo? while the code still
;; has pseudo registers).
(define (instruction-analyzer inst stack-as-pseudo?)
  (define (operand x)
    (cond ((pair? x)
           (cond ((eq? (car x) 'reg)
                  ;; Pseudo register.
                  (let ((idx (cadr x)))
                    (expt 2 (fx+ (vector-length register-names)
                                 idx))))
                 ((and stack-as-pseudo?
                       (eq? (car x) 'mem64+)
                       (eq? (cadr x) 'rsp))
                  ;; Stack location. XXX: notice that this does not
                  ;; understand mem128+ etc, which might be a
                  ;; problem when adding support for SSE2.
                  (let lp ((x* (cddr x))
                           (idx 0))
                    (cond ((null? x*)
                           (cond ((fx>=? idx 0)
                                  (let ((p (expt 2 (fx+ (vector-length register-names)
                                                        (fxarithmetic-shift-right idx 3)))))
                                    (bitwise-ior (register-set 'rsp) p)))
                                 (else
                                  ;; Clearly this is not a spilled variable
                                  (bitwise-ior (operand (car x))
                                               (operand (cdr x))))))
                          ((fixnum? (car x*))
                           (lp (cdr x*) (fx+ idx (car x*))))
                          (else
                           (lp '() -1)))))
                 (else
                  (bitwise-ior (operand (car x))
                               (operand (cdr x))))))
          ((register x))
          (else 0)))
  (define (address x)
    ;; That which is used to calculate the address. Useful for
    ;; two-operand instructions with stores.
    (if (memory? x) (operand x) 0))
  (define (address/c x)
    ;; The registers used to calculate an address.
    (if (memory? x)
        (operand (cdr x))
        0))
  (define (address/w x)
    ;; The location written to, but not the registers used to
    ;; calculate that location.
    (if (memory? x)
        (bitwise-and (operand x)
                     (bitwise-not (operand (cdr x))))
        (operand x)))
  (define (partial-write? x)
    ;; Would writing to x mean that parts of the original register
    ;; are unchanged (and is, in effect, an input to the
    ;; instruction)?
    (cond ((assq x register-list) =>
           (lambda (reg)
             (case (cadr reg)
               ((16 8 norex8 rex8) #t)
               (else #f))))
          (else #f)))
  (define (register32? x)
    ;; Writing to this register clears the upper 32 bits.
    (cond ((assq x register-list) =>
           (lambda (reg) (eqv? (cadr reg) 32)))
          (else #f)))

  (case (car inst)
    ((jo jno jb jnb jz jnz jbe jnbe js jns jp jnp jl jnl jle jnle
         jnae jae je jne jna ja jpe jpo jnge jge jng jg jc jnc)
     (values 'branch 0 rFLAGS))
    ((jmp)
     (if (pair? (cadr inst))
         ;; XXX: indirect jumps (through r15) are tail-calls.
         (values 'jump 0
                 (bitwise-ior (register-set 'rax)
                              (operand (cadr inst))))
         (values 'jump 0
                 (bitwise-ior (cond ((member (cadr inst)
                                             '(restore-stack))
                                     (register-set 'rax 'rdx))
                                    (else
                                     0))
                              (operand (cadr inst))))))
    ((call)
     (match inst
       [('call 'consargs)
        (values 'call
                (bitwise-ior rFLAGS
                             (register-set 'r10 'r11 'r12))
                (bitwise-ior (register-set 'rax 'r10 'rsp)
                             (operand (cadr inst))))]
       [('call 'stop-and-copy)
        (values 'call
                ;; The GC must either clobber everything or take a
                ;; few arguments in registers. But then these
                ;; registers must *always* contain valid data.
                (bitwise-ior rFLAGS
                             (register-set 'rax 'rcx 'rdx 'rsi 'rdi
                                           'r8 'r9 'r10 'r11 'r12
                                           'r13 'r14 #;'r15))
                (bitwise-ior (register-set 'rdi 'rsp 'r13 'r14)
                             (operand (cadr inst))))]
       [('call (or 'copy-stack 'switch-stack))
        (values 'call
                ;; Everything that's used.... everything.
                (bitwise-ior rFLAGS
                             (register-set 'rax 'rcx 'rbx 'rdx 'rbp 'rsp
                                           'rsi 'rdi 'r8 'r9 'r10 'r11 'r12
                                           'r13 'r14 'r15))
                (bitwise-ior (case (cadr inst)
                               ((switch-stack)
                                (register-set 'rax 'rdi 'rsp))
                               (else
                                (register-set 'rsp)))
                             (operand (cadr inst))))]
       [('call (or 'debug-display
                   '(mem64+ *set-processor-data!)
                   '(mem64+ *debug-put-u8)))
        (values 'call
                (target-convention 'scratch)
                (bitwise-ior (register-set 'rdi 'rsp)
                             (operand (cadr inst))))]
       [else
        (values 'call
                (target-convention 'scratch)
                ;; Full uses is handled by a %comment
                (bitwise-ior (register-set 'rax 'rsp)
                             (operand (cadr inst))))]))
    ((mov movq movd)
     (let ((dst (cadr inst)) (src (caddr inst)))
       (cond
         ((partial-write? dst)
          ;; Partial register write. This is arithmetic because it
          ;; acts like reg=(reg&mask)|value.
          (values 'arithmetic
                  (register dst)
                  (bitwise-ior (operand src) (register dst))))
         ((and (register32? dst) (register32? src))
          ;; Not a true move operation, since it clears the upper 32
          ;; bits. dst = src & 0xffffffff
          (values 'arithmetic (register dst) (register src)))
         (else
          (values (cond ((memory? dst) 'store)
                        ((memory? src) 'load)
                        (else 'move))
                  (address/w dst)
                  (bitwise-ior (operand src) (address/c dst)))))))
    ((movzx movsx movsxd)
     (let ((dst (cadr inst)) (src (caddr inst)))
       (values (if (memory? dst) 'store (if (memory? src) 'load 'arithmetic))
               (address/w dst) (bitwise-ior (operand src) (address/c dst)))))
    ((cmove cmovl cmovo cmovno cmovb cmovnb cmovz cmovnz cmovbe
            cmovnbe cmovs cmovns cmovp cmovnp cmovl cmovnl
            cmovle cmovnle
            cmovnae cmovae cmove cmovne cmovna cmova
            cmovpe cmovpo cmovnge cmovge cmovng cmovg
            cmovc cmovnc)
     ;; XXX: there is no cond-store.
     ;; TODO: if this is cmovcc reg32,reg32 then it's arithmetic.
     (let ((dst (cadr inst)) (src (caddr inst)))
       (let ((dst-op (operand dst)) (src-op (operand src)))
         (values (if (memory? dst) 'cond-store (if (memory? src)
                                                   'cond-load 'cond-move))
                 dst-op (bitwise-ior rFLAGS src-op dst-op)))))
    ((sete setl seto setno setb setnb setz setnz setbe
           setnbe sets setns setp setnp setl setnl
           setle setnle
           setnae setae sete setne setna seta
           setpe setpo setnge setge setng setg
           setc setnc)
     (let ((dst (cadr inst)))
       (values 'arithmetic (address/w dst) (operand dst))))
    ((and or xor add sub sar shr sal shl adc sbb
          ;; SSE
          addss subss mulss divss sqrtss minss maxss roundss)
     (let ((dst (cadr inst)) (src (caddr inst)))
       (let ((dst-op (operand dst)) (src-op (operand src)))
         (if (and (eq? (car inst) 'xor)
                  (eq? dst src))
             (values 'move (bitwise-ior rFLAGS dst-op) 0)
             (values 'arithmetic (bitwise-ior rFLAGS dst-op)
                     (bitwise-ior dst-op src-op
                                  (if (memq (car inst) '(adc sbb))
                                      rFLAGS 0)))))))
    ((not)
     (let ((op (operand (cadr inst))))
       (values 'arithmetic op op)))
    ((neg inc)
     (let ((op (operand (cadr inst))))
       (values 'arithmetic (bitwise-ior rFLAGS op) op)))
    ((bswap)
     (let ((op (operand (cadr inst))))
       (values 'arithmetic op op)))
    ((xchg)
     (let ((op1 (operand (cadr inst)))
           (op2 (operand (caddr inst))))
       (let ((r (register-set op1 op2)))
         (values 'arithmetic r r))))
    ((lea)
     (let ((dst (cadr inst)) (src (caddr inst)))
       (let ((dst-op (operand dst)) (src-op (operand src)))
         (values 'arithmetic dst-op src-op))))
    ((bsf bsr)
     (let ((dst (cadr inst)) (src (caddr inst)))
       (let ((dst-op (operand dst)) (src-op (operand src)))
         (values 'arithmetic (bitwise-ior rFLAGS dst-op) src-op))))
    ((test cmp comiss ucomiss)
     (let ((op1 (operand (cadr inst))) (op2 (operand (caddr inst))))
       (values 'comparison rFLAGS (bitwise-ior op1 op2))))
    ((cdqe)
     (values 'arithmetic (register 'rax) (register 'rax)))
    ((cqo)
     (values 'arithmetic (register-set 'rax 'rdx) (register 'rax)))
    ((imul)
     (match inst
       ((_ src)
        (values 'arithmetic (bitwise-ior rFLAGS (register-set 'rax 'rdx))
                (bitwise-ior (operand src)
                             (register 'rax))))
       ((_ dst src)
        (values 'arithmetic (bitwise-ior rFLAGS (operand dst))
                (bitwise-ior (operand dst)
                             (operand src))))
       ((_ dst src imm)
        (values 'arithmetic (bitwise-ior rFLAGS (register-set dst))
                (operand src)))))
    ((shrd shld)
     (match inst
       ((_ dst src count)
        (values 'arithmetic (bitwise-ior rFLAGS (register-set dst))
                (register-set dst src count)))))
    ((idiv)
     ;; XXX: idiv r/m8 does not use rdx
     (values 'arithmetic/trap (bitwise-ior rFLAGS (register-set 'rax 'rdx))
             (bitwise-ior (register-set 'rax 'rdx)
                          (operand (cadr inst)))))
    ((rep.movs)
     (let ((x (bitwise-ior rFLAGS (register-set 'rcx 'rdi 'rsi))))
       (values '? x x)))
    ((cpuid)
     (values '? (register-set 'rax 'rbx 'rcx 'rdx)
             (register-set 'rax 'rcx)))
    ((rdtsc)
     (values '? (register-set 'rax 'rdx) 0))
    ((rdtscp)
     (values '? (register-set 'rax 'rdx 'rcx) 0))
    ((syscall)
     ;; TODO: the code generator must manually record the registers
     ;; used to pass arguments. it must also do this for calls. this
     ;; is very conservative.
     (values 'call (bitwise-ior rFLAGS (register-set 'rax 'rcx 'r11))
             (register-set 'rax 'rdi 'rsi 'rdx 'r10 'r8 'r9)))
    ((out)
     (values 'store 0 (bitwise-ior (operand (cadr inst))
                                   (operand (caddr inst)))))
    ((in)
     ;; There is a partial dependency on rax
     (let ((ax (operand (cadr inst))))
       (values 'load ax (bitwise-ior ax (operand (caddr inst))))))
    ((rep.ins)
     (let ((rdi (address/c (cadr inst))))
       (values 'load (bitwise-ior rdi (register 'rcx))
               (bitwise-ior rdi
                            (operand (caddr inst))
                            (register 'rcx)))))
    ((push)
     (values 'push (register 'rsp)
             (bitwise-ior (register 'rsp) (operand (cadr inst)))))
    ((pop)
     (values 'pop (bitwise-ior (register 'rsp) (operand (cadr inst)))
             (register 'rsp)))
    ((ret)
     ;; XXX: all procedures return a value in rax. but what if there
     ;; are multiple return values? perhaps the code generator must
     ;; note that somehow. The callee-save registers, rbx and rbp,
     ;; are assumed to be used by the caller.
     (values 'return 0 (register-set 'rax 'rbx 'rbp)))
    ((cli sti clc stc)
     (values '? rFLAGS 0))
    ((int1 int3 ud2)
     ;; Uses r15 so that the error handler can find where the error
     ;; came from.
     (values 'trap 0 (register 'r15)))
    ((%comment %align %u8 nop)
     (match inst
       [('%comment 'call call-type numargs . _)
        ;; FIXME: the code generator must also tell which stack
        ;; locations and registers are used by these. Maybe use
        ;; LIVENESS for that.
        (let ()
          (define %arg-reg*
            (if (eq? call-type 'linux-syscall)
                '(rax rdi rsi rdx r10 r8 r9)
                '(rdi rsi rdx rcx r8 r9)))
          (do ((reg* %arg-reg* (cdr reg*))
               (uses (if (memq call-type '(apply-tail tail))
                         (register-set 'rax 'rbx 'rbp)
                         0)
                     (bitwise-ior uses (register-set (car reg*))))
               (numargs numargs (fx- numargs 1)))
              ((or (null? reg*) (eqv? numargs 0))
               (values 'directive 0 uses))))]
       #;
       [('%comment 'LIVENESS sets* uses*)
        ;; TODO: would this be used?
        (values 'directive (apply register-set sets*) (apply register-set uses*))]
       [else
        ;; XXX: never include %label here
        (values 'directive 0 0)]))
    ;; Floating point
    ((cvtsi2ss cvtsd2ss cvtss2sd)
     (match inst
       [(_ dst src)
        (values 'arithmetic (operand dst) (operand src))]))

    (else
     (error 'instruction-analyzer
            "Unknown amd64 instruction" inst))))

(define (amd64-liveness-annotator live frame-size)
  ;; Places the mask and size inside (nop (mem32+ ...)) instructions.

  ;; #x0F #x1F #b10xxx100 #xXX #xXX #xXX #xXX #xXX
  ;;  ^    ^    ^          ^    ^-----.--------^
  ;;  |    |    `-- ModR/M |          |
  ;;  `----^- Opcode       `--- SIB   `- Displacement

  ;; The frame size is encoded in displacement (in the left-most
  ;; part in the picture). The number of bytes used to encode
  ;; the frame size is given in the lower two bits of
  ;; ModR/M.reg:

  ;; reg | Meaning
  ;; ----+----------------
  ;; 000 | Zero frame size, zero live locals.
  ;; 001 | Frame size is 1 byte (1--2^8 locals)
  ;; 010 | Frame size is 2 bytes (2^8--2^16 locals)
  ;; 011 | Frame size is 3 bytes (2^16--2^24 locals)
  ;; 1xx | Frame size already encoded. NOP only encodes live mask.
  ;; 1xx | -- "" --
  ;; 1xx | -- "" --
  ;; 1xx | -- "" --

  ;; TODO: new scheme:
  ;; reg | Meaning
  ;; ----+----------------
  ;; x00 | Zero frame size, zero live locals.
  ;; x01 | Frame size is 2 bytes (1--2^16 locals)
  ;; x10 | Continuation of the live mask.
  ;; 1xx | Multiple value return point


  ;; The live mask is encoded in the displacement following the
  ;; frame size. If the bitmask does not fit in one instruction,
  ;; multiple NOPs are emitted.

  ;; Another idea for how to do this is to reuse the encoding
  ;; from the DNS RTYPE RRSET (or whichever one it was). There
  ;; are also shorter forms of the NOPs that may be used.
  ;; Another idea is to encode an offset in the displacement
  ;; that points to an array where are live masks are stored.
  (let ((m (bitwise-arithmetic-shift-right live (vector-length register-names))))
    (if (and (zero? m) (zero? frame-size))
        `(%comment nothing to see here)
        (let ((slen (cond ((fx>? (bitwise-length m) frame-size)
                           (error 'liveness-annotator
                                  "Too many variables for this frame"
                                  m frame-size))
                          ((fx<=? frame-size (expt 2 8)) 1)
                          ((fx<=? frame-size (expt 2 16)) 2)
                          #;((fx<=? frame-size (expt 2 24)) 3)
                          (else
                           ;; I don't expect to have over 2^16
                           ;; locals in a single stack frame.
                           (error 'liveness-annotator
                                  "Unencodable frame size" frame-size)))))
          (let lp ((reg slen)
                   (m (bitwise-ior (bitwise-arithmetic-shift-left m (fx* slen 8))
                                   (fx- frame-size 1)))
                   (i* '()))
            (let* ((b (cons (bitwise-and m #xff) '()))
                   (m (bitwise-arithmetic-shift-right m 8))
                   (b (cons (bitwise-and m #xff) b))
                   (m (bitwise-arithmetic-shift-right m 8))
                   (b (cons (bitwise-and m #xff) b))
                   (m (bitwise-arithmetic-shift-right m 8))
                   (b (cons (bitwise-and m #xff) b))
                   (m (bitwise-arithmetic-shift-right m 8))
                   (b (cons (bitwise-and m #xff) b))
                   (m (bitwise-arithmetic-shift-right m 8))
                   (b (cons (fxior (fxarithmetic-shift-left reg 3)
                                   #b10000100)
                            (reverse b)))
                   (b (cons #x0F (cons #x1F b))))
              (if (zero? m)
                  `(%vu8 ,(u8-list->bytevector (apply append
                                                      (reverse (cons b i*)))))
                  (lp #b100 m (cons b i*)))))))))

(define (signed-int32? x)
  ;; Useful to see if x fits inside a sign-extended immediate
  ;; operand.
  (and (number? x)
       (<= (- (expt 2 31)) x (- (expt 2 31) 1))))

(define (unsigned-int32? x)
  (and (number? x)
       (<= 0 x (- (expt 2 32) 1))))

(define (unsigned-int8? x)
  (and (number? x)
       (<= 0 x (- (expt 2 8) 1))))

(define (references-register? in reg)
  (let ((reg-idx (register-index reg)))
    (let lp ((in in))
      (cond ((pair? in)
             (or (lp (car in))
                 (lp (cdr in))))
            ((eqv? (register-index in) reg-idx))
            (else #f)))))

(define (replace-register in from to)
  (let ((from-idx (register-index from)))
    (let lp ((in in))
      (cond ((pair? in)
             (cons (lp (car in))
                   (lp (cdr in))))
            ((eqv? (register-index in) from-idx)
             to)
            (else in)))))

(define (const-memref? ref)
  ;; Evaluate the constant part of a memory reference
  (match ref
    [((? symbol? _) . x)
     (let lp ((mem x))
       (match mem
         [() #t]
         [('* a b)
          (and (lp a) (lp b))]
         [(a . b)
          (and (lp a) (lp b))]
         [(? number? x)
          #t]
         [_ #f]))]
    [_ #f]))

(define (eval-memref ref)
  ;; Evaluate the constant part of a memory reference
  (let lp ((mem ref))
    (match mem
      ([] 0)
      (['* a b]
       (* (lp a) (lp b)))
      ([a . b]
       (+ (lp a) (lp b)))
      ((? number? x)
       x)
      ((? symbol?)
       0)
      (x
       (error 'eval-memref "Unsupported memory reference syntax" ref x)))))

;; Either returns #f or, if there was a match, a list. The first
;; part of the list indicates which lines were affected by the
;; match. Some of the arguments may be dummy instructions where the
;; inst is #f.
(define (peephole-optimizer line-inst line-deads al bl cl dl)
  (define (register? x)
    (and (register-index x) #t))
  (define (reg32 x)
    (vector-ref register-names32 (register-index x)))
  (define (reg8l x)
    (vector-ref register-names8l (register-index x)))
  (define (dead? x deads)
    (bitwise-bit-set? deads (register-index x)))
  #;
  (define (branch? x)
    (let-values (((info . _) (instruction-analyzer x)))
      (eq? info 'branch)))
  #;
  (define (register=? x y)
    (let ((xi (register-index x)))
      (and xi (eqv? xi (register-index y)))))
  (define (register32+? x)
    (cond ((assq x register-list) =>
           (lambda (reg)
             (case (cadr reg)
               ((32 64) #t)
               (else #f))))
          (else #f)))
  (define (register64? x)
    (cond ((assq x register-list) =>
           (lambda (reg)
             (case (cadr reg)
               ((64) #t)
               (else #f))))
          (else #f)))
  (let ((a (line-inst al))
        (b (line-inst bl))
        (c (line-inst cl)))
    (cond
      ;; Loading zero is faster with an xor.
      ((and (pair? a)
            (eq? (car a) 'mov)
            (pair? (cdr a))
            (register32+? (cadr a))
            (pair? (cddr a))
            (eqv? (caddr a) 0))
       (let ((r (reg32 (cadr a))))
         (list #b1 `(xor ,r ,r))))
      ;; Loading small constants into registers.
      ((and (pair? a)
            (eq? (car a) 'mov)
            (pair? (cdr a))
            (register64? (cadr a))
            (pair? (cddr a))
            (number? (caddr a))
            (unsigned-int32? (caddr a)))
       (list #b1 `(mov ,(reg32 (cadr a)) ,(caddr a))))
      ;; Loading small negative constants into rax might be done by
      ;; loading them into eax and doing cdqe.

      ;; Test with a small constant.
      ;; (test rax 7) => (test al 7)
      ((and (pair? a) (pair? (cdr a)) (pair? (cddr a))
            (eq? (car a) 'test)
            (register32+? (cadr a))
            (fixnum? (caddr a))
            (unsigned-int8? (caddr a)))
       (list #b1 `(test ,(reg8l (cadr a)) ,(caddr a))))

      ;; Comparison with zero.
      ;; (xor eax eax)
      ;; (cmp rax rcx)
      ;; (je/jne/jz/jnz ...)
      ((and (pair? a) (pair? (cdr a)) (pair? (cddr a))
            (pair? b) (pair? (cdr b)) (pair? (cddr b))
            (pair? c)
            (memq (car c) '(je jne jz jnz))
            (eq? (car a) 'xor) (eq? (cadr a) (caddr a))
            (eq? (car b) 'cmp)
            (register? (cadr b))
            (register? (caddr b))
            (eqv? (register-type (cadr a)) 32)
            (eqv? (register-type (cadr b)) 64)
            (= (register-index (cadr a))
               (register-index (cadr b))))
       (list #b11
             a
             `(test ,(caddr b) ,(caddr b))))

      ;; Bitwise-and followed by test.
      ;; (and rax reg/imm32/etc)
      ;; (test rax rax)      rax dead
      ;; (je ...)
      ((and (pair? a) (pair? (cdr a)) (pair? (cddr a))
            (pair? b) (pair? (cdr b)) (pair? (cddr b))
            (pair? c)
            (memq (car c) '(je jne jz jnz))
            (eq? (car a) 'and)
            (eq? (car b) 'test)
            (register32+? (cadr b))
            (eq? (cadr b) (caddr b))
            (eq? (cadr a) (cadr b))
            (dead? (cadr a) (line-deads bl)))
       (list #b11
             `(%comment peephole ,a)
             `(test ,(cadr a) ,(caddr a))))

      ;; Loading a constant followed by immediate use
      ;; (mov eax 8)
      ;; (add rcx rax)      rax dead
      ((and (pair? a) (pair? (cdr a)) (pair? (cddr a))
            (pair? b) (pair? (cdr b)) (pair? (cddr b))
            (eq? (car a) 'mov)
            (eqv? (register-type (cadr a)) 32)
            (memq (car b) '(add sub xor and cmp test mov or))
            (register32+? (caddr b))
            (= (register-index (cadr a))
               (register-index (caddr b)))
            (not (eq? (cadr b) (caddr b)))
            (dead? (cadr a) (line-deads bl))
            (or (not (number? (caddr a)))
                ;; The immediate shouldn't become signed extended
                ;; when it's placed on the second instruction.
                (signed-int32? (caddr a))))
       (list #b11
             `(%comment peephole ,a)
             `(,(car b) ,(cadr b) ,(caddr a))))

      ;; FIXME: too narrow
      ;; (xor eax eax) (mov rcx rax) => (xor eax eax) (xor rcx rcx)
      ((and (equal? a '(xor eax eax))
            (pair? b)
            (eq? (car b) 'mov)
            (pair? (cdr b))
            (register32+? (cadr b))
            (pair? (cddr b))
            (memq (caddr b) '(rax eax)))
       (let ((reg (reg32 (cadr b))))
         (list #b11 a
               `(xor ,reg ,reg))))

      ;; 2115C3: 4881FFFF010000                 (cmp rdi #x1FF)
      ;; Use edi, because #x1FF's object mask <= 32.
      ((and (pair? a) (pair? (cdr a)) (pair? (cddr a))
            (eq? (car a) 'cmp)
            (register64? (cadr a))
            ;; TODO: use the constants from objects.scm to decode
            ;; (or at least encode) these constants.
            (or (eqv? (caddr a) (immediate #t))
                (eqv? (caddr a) (immediate #f))
                (eqv? (caddr a) (immediate '()))
                (eqv? (caddr a) (immediate (eof-object)))))
       (list #b1
             `(cmp ,(reg32 (cadr a)) ,(caddr a))))

      ;; 2020BD: 488B4306                       (mov rax (mem64+ rbx #x6))
      ;; 2020C1: 488BD8                         (mov rbx rax)  rax dead

      ;; 2020E3: 488B4C2408                     (mov rcx (mem64+ rsp #x8))
      ;; 2020E8: 488BD9                         (mov rbx rcx)  rcx dead

      ;; 2020AF: 488B442428                     (mov rax (mem64+ rsp #x28))
      ;; 2020B4: 483BE8                         (cmp rbp rax)  rax dead. rax != rsp
      ((and (pair? a) (pair? (cdr a)) (pair? (cddr a))
            (pair? b) (pair? (cdr b)) (pair? (cddr b))
            (eq? (car a) 'mov)
            (eq? (cadr a) (caddr b))
            (memq (car b) '(mov cmp))
            (pair? (caddr a)) (eq? 'mem64+ (car (caddr a)))
            (register64? (cadr b))
            (dead? (cadr a) (line-deads bl))
            (let-values (((info sets uses) (instruction-analyzer a #t)))
              ;; To stop it messing up (mov rax (mem64+ rax 6))
              (zero? (bitwise-and sets uses))))
       (list #b11
             `(%comment peephole ,a) ; the load might not be removed otherwise
             `(,(car b) ,(cadr b) ,(caddr a))))

      ;; 20200C: 488B4C2428                     (mov rcx (mem64+ rsp #x28))
      ;; 202011: 483BC8                         (cmp rcx rax)
      ((and (pair? a) (pair? (cdr a)) (pair? (cddr a))
            (pair? b) (pair? (cdr b)) (pair? (cddr b))
            (eq? (car a) 'mov)
            (eq? (cadr a) (cadr b))
            (eq? (car b) 'cmp)
            (pair? (caddr a)) (eq? 'mem64+ (car (caddr a)))
            (or (register64? (caddr b))
                (integer? (caddr b)))
            (dead? (cadr a) (line-deads bl))
            (let-values (((info sets uses) (instruction-analyzer a #t)))
              ;; To stop it messing up (mov rax (mem64+ rax 6))
              (zero? (bitwise-and sets uses))))
       (list #b11
             `(%comment peephole ,a)
             `(,(car b) ,(caddr a) ,(caddr b))))


      ;; 2143DF: 488BF2                         (mov rsi rdx)
      ;; 2143E2: 4881C600000400                 (add rsi #x40000)   rfl not live out
      ;; 2143E9: 4881FE00000800                 (cmp rsi #x80000)
      ;; ==> (lea rsi (mem+ rdx #x40000))
      ;;     (cmp rsi #x80000)


      ;; 234DFB: B908000000                     (mov ecx #x8)
      ;; 234E00: 483BC8                         (cmp rcx rax)
      ;; 234E03: 0F845C000000                   (jz (+ rip #x5C))

      ;; (or rdx rcx)
      ;; (test dl #x7)  rdx not live
      ;; => (or edx ecx)
      ;;    (test dl #x7) or maybe edx is just fine. measure!

      ;; 26BF44: 488BC4                         (mov rax rdx)
      ;; 26BF47: 48C1E003                       (shl rax #x3)
      ;; => (lea rax (mem+ (* rdx 8))) if rfl is not live out of shl


      (else
       (match (list a b c)
         ;; 20F2BD: B850000000                     (mov eax #x50)
         ;; 20F2C2: 48C1F803                       (sar rax #x3)
         ([('mov (? register32+? mvreg) (? number? const))
           ('sar (? register32+? shreg) (? number? amount))
           . _]
          ;; TODO: rfl should not be live out of b
          (and (= (register-index mvreg) (register-index shreg))
               (list #b11
                     `(%comment peephole ,a)
                     `(mov ,mvreg ,(bitwise-arithmetic-shift-right const amount)))))

         ;; 20F2A9: 33C0                           (xor eax eax)
         ;; 20F2AB: 48C1F803                       (sar rax #x3)
         ([('xor (? register32+? xreg) xreg)
           ('sar (? register32+? shreg) . _)
           . _]
          ;; TODO: rfl should not be live out of b
          (and (= (register-index xreg) (register-index shreg))
               (list #b11
                     a
                     `(%comment peephole ,b))))

         ;; 20F2B5: B80B000000                     (mov eax #xB)
         ;; 20F2BA: EE                             (out dx al)
         ([('mov 'eax (? number? const))
           ('out port 'al)
           . _]
          (and (dead? 'eax (line-deads bl))
               (list #b11
                     `(mov al ,(fxand const #xff))
                     b)))

         ;; 20314B: 33C9                           (xor ecx ecx)
         ;; 20314D: 488B440807                     (mov rax (mem64+ rax #x7 (* rcx #x1)))
         ([('xor (? register32+? xreg) xreg)
           (op dst ('mem64+ memargs ...))
           . _]
          ;; This and the next are poor man's CSE or something
          (and (references-register? memargs xreg)
               (list #b11
                     a
                     `(,op ,dst (mem64+ ,@(replace-register memargs xreg 0))))))

         ;; 203117: B908000000                     (mov ecx #x8)
         ;; 20311C: 488B440807                     (mov rax (mem64+ rax #x7 (* rcx #x1)))
         ([('mov (? register32+? mvreg) (? signed-int32? const))
           (op dst ('mem64+ memargs ...))
           . _]
          (and (references-register? memargs mvreg)
               (let ((new-ref `(mem64+ ,@(replace-register memargs mvreg const))))
                 (and (signed-int32? (eval-memref new-ref))
                      (list #b11
                            a
                            `(,op ,dst ,new-ref))))))

         ;; 21151E: 4533C0                         (xor r8d r8d)
         ;; 211521: 498BC8                         (mov rcx r8)
         #;
         ([('xor (? register32+? xreg) xreg)
           ('mov (? register32+? dst) (? register32+? src))
           . _]
          ;; This rule is apparently never used
          (and (eqv? (register-index xreg) (register-index src))
               (list #b11
                     a
                     (let ((dst32 (reg32 dst)))
                       `(xor ,dst32 ,dst32)))))

         ;; 203B62: 488B808A070000                 (mov rax (mem64+ rax #x78A))
         ;; 203B69: 4C8BF8                         (mov r15 rax)   rax dead

         ;; 22420C: 48B8A2033D0000000000           (mov rax #x3D03A2)
         ;; 224216: 488BC8                         (mov rcx rax)
         ([('mov (? register64? dst1) (and ((or 'mem64+ '+) . _) mref))
           ('mov (? register64? dst2) dst1)
           . _]
          (and (dead? dst1 (line-deads bl))
               (list #b11
                     `(%comment peephole ,a)
                     `(mov ,dst2 ,mref))))

         ;; 202C87: 83E008                         (and eax #x8)
         ;; 202C8A: 4885C0                         (test rax rax)   rax dead
         ([('and (? register32+? r32) (? signed-int32? const))
           ('test (? (lambda (x)
                       (and (register64? x)
                            (= (register-index x)
                               (register-index r32)))) reg) reg)
           . _]
          (and (dead? reg (line-deads bl))
               (list #b11
                     `(%comment peephole ,a)
                     `(test ,r32 ,const))))

         ;; 224D47: 33C9                           (xor ecx ecx)
         ;; 224D49: 483BC1                         (cmp rax rcx)
         ([('xor (? register32+? r32) r32)
           ('cmp reg (? (lambda (x)
                          (and (register64? x)
                               (= (register-index x)
                                  (register-index r32))))))
           . _]
          (and (dead? reg (line-deads bl))
               (list #b11
                     `(%comment peephole ,a)
                     `(cmp ,reg 0))))

         ;; 2027FF: 4883E008                       (and rax #x8)
         ([('and (? register64? dst) (? unsigned-int32? const))
           . _]
          ;; XXX: note that (and rax #xFFFFFFFF) does not exist
          (list #b1
                `(and ,(reg32 dst) ,const)))

         ;; 2522E2: 488D04252F000000               (lea rax (mem+ #x2F))
         ([('lea (? register64? dst) (? const-memref? ref))
           . _]
          (let ((const (eval-memref ref)))
            (and (signed-int32? const)
                 (list #b1
                       `(mov ,dst ,const)))))

         ;; 2258D8: BD2F000000                     (mov ebp #x2F)
         ;; 2258DD: 4883E5F0                       (and rbp #xFFFFFFFFFFFFFFF0)
         ([('mov (? register32+? r1) (? signed-int32? value))
           ('and (? register32+? r2) (? integer? mask))
           . _]
          ;; TODO: rfl should not be live out of b
          (and (= (register-index r1) (register-index r2))
               (list #b11
                     `(%comment peephole ,a)
                     `(mov ,r2 ,(bitwise-and value mask)))))

         ;; 2258D8: BD20000000                     (mov ebp #x20)
         ;; 2258DD: 4C2BED                         (sub r13 rbp)
         ([('mov (? register32+? r1) (? signed-int32? value))
           ((and (or 'sub 'add) op) (? register32+? dst) (? register32+? r2))
           . _]
          (and (= (register-index r1) (register-index r2))
               (list #b11
                     a
                     `(,op ,dst ,value))))

         ;; 203133 81E7FF000000       (and edi #xFF)
         ;; 203139 4881FFFF000000     (cmp rdi #xFF)
         ;; 203140 0F852C000000       (jnz L1)
         ([('and (? register32+? r1) #xFF)
           ('cmp (? register32+? r2) (? unsigned-int8? v))
           . _]
          (and (= (register-index r1) (register-index r2))
               (dead? r2 (line-deads bl))
               (list #b11
                     `(%comment peephole ,a)
                     `(cmp ,(reg8l r2) ,v))))

         (else #f))))))




;;   (assembler-case (a b c d)
;;     ;; Loading a 32-bit immediate.
;;     ([('mov (reg64 dst) (int N))]
;;      (<= 1 N #xffffffff)
;;      (=> `(mov ,(reg32/64->reg32 dst) ,N)))

;;     ;; Loading zero.
;;     ([('mov (reg64 dst) (int 0))]
;;      (let ((dst32 (reg32/64->reg32 dst)))
;;        (=> `(xor ,dst ,dst))))

;;     ;; Testing with a small mask.
;;     ([('test (reg64/32 op) (int N))]
;;      (<= N #xFF)
;;      (=> `(test ,(reg64/32->8 op) ,N)))

;;     ;; Comparison with zero.
;;     ([('xor (reg32 xop0) (reg32 xop1))
;;       ('cmp (reg64 cop0) (reg64 cop1))]
;;      (and (eq? xop0 xop1)
;;           (reg-idx=? xop0 cop0))
;;      (=> a
;;          `(test ,(reg64/32->32 xop0) ,cop1)))

;;     ;; Rinky dink constant propagation.
;;     ([('mov (reg32 const-reg) (int N))
;;       (op (reg64/32 dst) (reg64/32 src))]
;;      (and (<= 0 N #xffffffff)
;;           (reg-idx=? const-reg src)
;;           (memq op '(add sub xor and or)))
;;      (=> a
;;          `(,op ,dst ,N)))

;;     ;; (and ecx #x7FFFF8) (test rcx rcx)
;;     ([('and (reg32 aop0) (int N))
;;       ('test (reg64/32 top0) (reg64/32 top1))]
;;      (and (<= 0 N #xffffffff)
;;           (eq? top0 top1)
;;           (reg-idx=? aop0 top0)
;;           (dead? top0 (line-deads bl)))
;;      (=> #f
;;          `(test ,top0 ,N)))

;;     ;; Rinky dink constant propagation.
;;     ([('xor (reg32 xop0) (reg32 xop1))
;;       ('mov (reg64/32 dst) (reg64/32 src))]
;;      (and (eq? xop0 xop1)
;;           (reg-idx=? xop0 src))
;;      (let ((dst32 (reg64/32->32 dst)))
;;        (=> a
;;            `(xor ,dst32 ,dst32))))


;;                   ;; (mov rax rdi)
;;                   ;; (and al #x7)
;;                   ;; => (mov rax rdi)
;;                   ;;    (and dil #x7)  if rax in deads
;;                   ;; (or rdx rcx)
;;                   ;; (test dl #x7)  rdx not live
;;                   ;; => (or edx ecx)
;;                   ;;    (test dl #x7) or maybe edx is just fine. measure!

;;                   ;; 2115C3: 4881FFFF010000                 (cmp rdi #x1FF)
;;                   ;; use edi, because #x1FF's mask <= 32

;;                   ;; 26BF44: 488BC4                         (mov rax rdx)
;;                   ;; 26BF47: 48C1E003                       (shl rax #x3)
;;                   ;; => (lea rax (mem+ (* rdx 8))) if rfl is not live out of shl





;;                   ;; 2020E3: 488B4C2408                     (mov rcx (mem64+ rsp #x8))
;;                   ;; 2020E8: 488BD9                         (mov rbx rcx)

;;                   ;; 2020EB: 488B4C2410                     (mov rcx (mem64+ rsp #x10))
;;                   ;; 2020F0: 488BE9                         (mov rbp rcx)


)
