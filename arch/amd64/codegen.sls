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

;;; AMD64 code generator

(library (loko arch amd64 codegen)
  (export
    codegen)
  (import
    (loko arch amd64 objects)
    (loko libs context)
    (loko compiler recordize)
    (rename (loko utils) (map-in-order map))
    (loko match)
    (except (rnrs) map)
    (only (rnrs mutable-pairs) set-car!))

(define (bytevector<? x y)
  (let* ((xlen (bytevector-length x))
         (ylen (bytevector-length y))
         (end (fxmin xlen ylen)))
    (let lp ((i 0))
      (if (fx=? i end)
          (fx<? xlen ylen)
          (let ((diff (fx- (bytevector-u8-ref x i) (bytevector-u8-ref y i))))
            (if (eqv? diff 0)
                (lp (fx+ i 1))
                (fxnegative? diff)))))))

(define hashtable->minimal-perfect-hashtable
  ;; Algorithm from http://stevehanov.ca/blog/index.php?id=119
  (let ((EMPTY (vector 'empty)))
    (lambda (dict)
      (let* ((size (hashtable-size dict))
             (buckets (make-vector size '()))
             (G (make-vector size 0))
             (K (make-vector size))
             (V (make-vector size EMPTY)))
        (vector-for-each
         (lambda (key)
           (let ((idx (fxmod (bytevector-hash key) size)))
             (vector-set! buckets idx (cons key (vector-ref buckets idx)))))
         (vector-sort bytevector<? (hashtable-keys dict))) ;reproducability
        ;; Longest bucket first
        (vector-sort! (lambda (x y) (> (length x) (length y)))
                      buckets)
        (let loop-bucket ((b 0))
          (unless (= b size)
            (let ((bucket (vector-ref buckets b)))
              (if (and (pair? bucket) (pair? (cdr bucket)))
                  (let lp ((d 1) (b* bucket) (slots '()))
                    (cond ((pair? b*)
                           (let ((slot (fxmod (bytevector-hash (car b*) d) size)))
                             (if (or (not (eq? (vector-ref V slot) EMPTY))
                                     (memv slot slots))
                                 (lp (+ d 1) bucket '())
                                 (lp d (cdr b*) (cons slot slots)))))
                          (else
                           (vector-set! G (fxmod (bytevector-hash (car bucket)) size) d)
                           (for-each (lambda (slot key)
                                       (vector-set! V slot (hashtable-ref dict key #f))
                                       (vector-set! K slot key))
                                     (reverse slots) bucket)
                           (loop-bucket (+ b 1)))))
                  (let ((free
                         (let lp ((i 0) (ret '()))
                           (cond ((= i (vector-length V))
                                  ret)
                                 ((eq? (vector-ref V i) EMPTY)
                                  (lp (+ i 1) (cons i ret)))
                                 (else
                                  (lp (+ i 1) ret))))))
                    ;; All remaining buckets are empty or contain
                    ;; only one element. They are assigned to the
                    ;; free slots.
                    (let lp ((b b) (free free))
                      (unless (= b size)
                        (let ((bucket (vector-ref buckets b)))
                          (cond ((pair? bucket)
                                 (let ((slot (car free))
                                       (key (car bucket)))
                                   (vector-set! G (fxmod (bytevector-hash key) size)
                                                (- -1 slot))
                                   (vector-set! V slot (hashtable-ref dict key #f))
                                   (vector-set! K slot key)
                                   (lp (+ b 1) (cdr free))))
                                (else
                                 (lp (+ b 1) free)))))))))))
        (values G K V)))))

;; FNV-1 hash
(define bytevector-hash
  (case-lambda
    ((bv)
     (define offset_basis 2166136261)
     (bytevector-hash bv offset_basis))
    ((bv d)
     (define FNV_prime 16777619)
     (do ((i 0 (fx+ i 1))
          (d d
             (fxand (fxxor (fx* d FNV_prime)
                           (bytevector-u8-ref bv i))
                    #xffffffff)))
         ((fx=? i (bytevector-length bv)) d)))))

;; Multiply-with-carry (apparently by George Marsagli, says Wikipedia).
(define compile-time-random-u16
  (let ((w #xc0fec0fe)
        (z #x5ea5ea5a))
    (lambda ()
      (define fxasl fxarithmetic-shift-left)
      (define fxasr fxarithmetic-shift-right)
      (set! z (fx+ (fx* 36969 (fxand z #xffff))
                   (fxasr z 16)))
      (set! w (fx+ (fx* 18000 (fxand w #xffff))
                   (fxasr w 16)))
      (fxand (fx+ (fxasl (fxand z #xffff) 16)
                  (fxand w #xfffff))
             #xffff))))

;; Compute the magic number and shift amount for division with a known
;; divisor. From Hacker's Delight. This is portable, so it should
;; probably be somewhere else.
(define (division-magic d W)
  (assert (not (eqv? d 0)))
  (let* ((twoW-1 (expt 2 (- W 1)))
         (ad (abs d))
         (anc (let ((t (+ twoW-1 (if (< d 0) 1 0))))
                (- t 1 (mod t ad)))))
    (assert (<= 2 ad (- twoW-1 1)))
    (let-values (((q1 r1) (div-and-mod twoW-1 anc))
                 ((q2 r2) (div-and-mod twoW-1 ad)))
      (let lp ((p (- W 1)) (q1 q1) (r1 r1) (q2 q2) (r2 r2))
        (let ((p (+ p 1))
              (q1 (* q1 2))
              (r1 (* r1 2))
              (q2 (* q2 2))
              (r2 (* r2 2)))
          (let-values (((q1 r1) (if (>= r1 anc)
                                    (values (+ q1 1) (- r1 anc))
                                    (values q1 r1)))
                       ((q2 r2) (if (>= r2 ad)
                                    (values (+ q2 1) (- r2 ad))
                                    (values q2 r2))))
            (let ((delta (- ad r2)))
              (if (or (< q1 delta) (and (= q1 delta) (eqv? r1 0)))
                  (lp p q1 r1 q2 r2)
                  (let ((M (+ q2 1))
                        (s (- p W)))
                    (let ((M (cond ((< -1 M (expt 2 (- W 1)))
                                    M)
                                   (else
                                    (assert (<= twoW-1 M (- (expt 2 W) 1)))
                                    (- M (expt 2 W))))))
                      (let ((M (if (< d 0) (- M) M)))
                        (values M s))))))))))))

(define (combinator? x)
  (and (closure? x)
       (null? (closure-free* x))))

;; codes is a list of records. Returns a list of assembler text and
;; a list of assembler data.
(define (codegen codes primlocs make-init-code?)
  (define who 'simple-cg)
  (define debug #f)
  (define use-branch-instrumentation #f) ;for AFL
  (define %heap-rem 'r13)         ;XXX: find through process vector?
  (define %alloc 'r14)
  (define %closure 'r15)
  (define %arg-reg* '(rdi rsi rdx rcx r8 r9))
  (define code '())
  (define data '())
  (define bss-globals '())
  (define globals (make-eq-hashtable))
  (define PROCESS-VECTOR-OFFSET 16)
  (define (get-global name)
    (or (hashtable-ref globals name #f)
        (let ((offset (* 8 (+ PROCESS-VECTOR-OFFSET 1 (hashtable-size globals)))))
          (hashtable-set! globals name offset)
          offset)))
  (define (init-globals!)
    (let-values (((k* v*) (hashtable-entries globals)))
      (vector-for-each
       (lambda (k v)
         (let ((gensym-idx (hashtable-ref gensym-idx-locations k #f)))
           ;; (write (list k v gensym-idx))
           ;; (newline)
           ;; The key is #{standard-input-port |U35CrPcjZ=PvgdY9|}
           ;; and the value is an offset into the process vector.
           ;; gensym-idx is initially (%u64 31). The library manager
           ;; stuff has the gensym in some table, so to reach the
           ;; value stored in the global environment the gensym
           ;; contains an index into the global environment.
           (when gensym-idx
             ;; Subtract eight to adjust for the length field.
             (set-car! (cdr gensym-idx) (fx- v 8)))))
       k* v*))
    ;; Make it so the global environment looks like a vector. The
    ;; stop-and-copy GC currently uses this to know where the
    ;; environment ends. TODO: should use EQU instead
    `((mov rax ,(immediate (+ PROCESS-VECTOR-OFFSET 1 (hashtable-size globals))))
      (mov (mem64+ global-environment) rax)))
  ;; Tables for encode-object.
  (define (encode-const const)
    (encode-object const objs strings symbols gensyms bytevectors
                   gensym-idx-locations emit-data))
  (define objs (make-eq-hashtable))
  (define symbols (make-eq-hashtable)) ;symbol interning
  (define gensyms (make-eq-hashtable))
  (define strings (make-hashtable string-hash string=?))
  (define bytevectors (make-hashtable (lambda (x)
                                        (do ((i 0 (+ i 1))
                                             (sum 3 (+ sum (bytevector-u8-ref x i))))
                                            ((fx=? i (bytevector-length x))
                                             (mod sum 147))))
                                      bytevector=?))
  (define gensym-idx-locations (make-eq-hashtable))
  ;; Pseudo-registers. XXX: there is a definite problem with this.
  ;; the code must be very careful to not put just about any value
  ;; in a pseudo-register, because any pseudo-register can be
  ;; spilled or assigned to a callee-save register and then the GC
  ;; will see it. If the register contains nonsense then the GC may
  ;; error out.
  (define *pcounter* -1)
  (define (reg)
    (set! *pcounter* (fx+ *pcounter* 1))
    `(reg ,*pcounter*))
  (define (reset-pseudo-register-counter!)
    (set! *pcounter* -1))
  (define *lcounter* -1)
  (define (label name)
    (set! *lcounter* (fx+ *lcounter* 1))
    (vector name *lcounter*))
  ;; Various explicit type checks (not complete!)
  (define explicit-checks #f)
  (define explicit-vector-checks #t)  ;for debugability

  (define (last l)
    (if (null? (cdr l))
        (car l)
        (last (cdr l))))

  ;; This takes a stack index, an environment and a variable. The
  ;; variable is given a location in a pseudo register. The golden
  ;; rule for this concept is that only proper objects (that the GC
  ;; can't choke on) are allowed in these locations. This is because
  ;; they might be saved on the stack or in a callee-save register.
  (define-syntax with-loc
    (lambda (x)
      (syntax-case x ()
        ((with-loc ((env loc) (old-env var))
           . body)
         ;; (syntax->datum #'var)
         ;; TODO: this is no longer necessary if var is false
         #'(let ((r (reg)))
             (let ((env (if var (extend-env var r old-env) old-env))
                   (loc r))
               . body))))))

  (define-syntax with-new-frame
    (lambda (x)
      (syntax-case x (emit call)
        ;; Previously this was used to adjust rsp across calls. Now
        ;; it is used to tell pass-optimize where to insert liveness
        ;; information. It must be safe to spill registers across the
        ;; body.
        ((with-new-frame ((env))
           (emit (q (call target))))
         #'(begin
             ;;(emit #;'(%comment save-live-registers))
             (emit (q (call target)))
             ;; The optimizer will do liveness analysis and replace
             ;; this comment with a note to the GC. If there is no
             ;; note then the whole frame is live.
             (emit '(%comment liveness-information))
             #;(emit '(%comment restore-live-registers)))))))

  (define-syntax with-restart-handler
    (lambda (x)
      (syntax-case x (index overflow fixnum? char? flonum?)
        ((with-restart-handler (type restart raise proceed)
           body ...)
         #'(let ((restart (vector 'restart))
                 (raise (vector 'raise))
                 (proceed (vector 'proceed)))
             (emit `(%label ,restart))
             ;; The body expression must return at most one value
             ;; and it should emit a branch to one of the labels.
             (let ((ret (let ()
                          body ...)))
               (emit `(%label ,raise)
                     '(%comment unlikely)
                     ;; It would be better to use int3, but that
                     ;; generates SIGTRAP, which confuses gdb.
                     '(ud2)
                     ;; TODO: when the program encounters UD2, r15
                     ;; should be pointing to the closure for the
                     ;; running procedure. So the error handler can
                     ;; get the entry point from its info, and then
                     ;; it can find the branch that jumped to this
                     ;; UD2. This relies on the optimizer not
                     ;; merging the different UD2 blocks.
                     `(jmp ,restart)
                     `(%label ,proceed))
               ret)))
        ((with-restart-handler (type proceed)
           body ...)
         (with-syntax (((restart raise) (generate-temporaries '(restart raise))))
           #'(with-restart-handler (type restart raise proceed)
               body ...))))))

  ;; FIXME: tabulate
  (define (rflags->SETcc rflags.x)
    (case rflags.x
      ((rflags.z) 'sete)
      ((rflags.s) 'sets)
      ((rflags.l) 'setl)
      ((rflags.g) 'setg)
      ((rflags.o) 'seto)
      ((rflags.ge) 'setge)
      ((rflags.le) 'setle)
      ((rflags.be) 'setbe)
      ((rflags.nz) 'setne)
      ((rflags.ns) 'setns)
      ((rflags.nl) 'setnl)
      ((rflags.ng) 'setng)
      ((rflags.no) 'setno)
      ((rflags.nge) 'setnge)
      ((rflags.nle) 'setnle)
      ((rflags.nbe) 'setnbe)
      (else #f)))

  (define (rflags->Jcc rflags.x)
    ;; XXX: these are inverses, silly enough
    (case rflags.x
      ((rflags.z) 'jne)
      ((rflags.s) 'jns)
      ((rflags.l) 'jnl)
      ((rflags.g) 'jng)
      ((rflags.o) 'jno)
      ((rflags.ge) 'jnge)
      ((rflags.le) 'jnle)
      ((rflags.be) 'jnbe)
      ((rflags.nz) 'jz)
      ((rflags.ns) 'js)
      ((rflags.nl) 'jl)
      ((rflags.ng) 'jg)
      ((rflags.no) 'jo)
      ((rflags.nge) 'jge)
      ((rflags.nle) 'jle)
      ((rflags.nbe) 'jbe)
      (else #f)))

  (define (not-rflags rflags.x)
    (case rflags.x
      ((rflags.z) 'rflags.nz)
      ((rflags.s) 'rflags.ns)
      ((rflags.l) 'rflags.nl)
      ((rflags.g) 'rflags.ng)
      ((rflags.o) 'rflags.no)
      ((rflags.ge) 'rflags.nge)
      ((rflags.le) 'rflags.nle)
      ((rflags.be) 'rflags.nbe)

      ((rflags.nz) 'rflags.z)
      ((rflags.ns) 'rflags.s)
      ((rflags.nl) 'rflags.l)
      ((rflags.ng) 'rflags.g)
      ((rflags.no) 'rflags.o)
      ((rflags.nge) 'rflags.ge)
      ((rflags.nle) 'rflags.le)
      ((rflags.nbe) 'rflags.be)
      (else #f)))

  ;; Emit code. XXX: No more than a single (cg ...) should appear as
  ;; an argument to emit.
  (define (emit . x*)
    (define (emit1 x)
      (when debug
        (display "  ")
        (write x)
        (newline))
      (match x
        (('call 'stop-and-copy)
         (set! code `((%comment unlikely)
                      (call stop-and-copy)
                      ,@code)))
        (_
         (set! code (cons x code)))))
    (for-each emit1 x*))

  (define (emit-data x)
    (when debug
      (display "DATA: ")
      (write x)
      (newline))
    (set! data (cons x data)))

  (define (constant? x)
    (or (const? x)
        (and (infer? x)
             (const? (infer-expr x)))))

  (define (constant-value x)
    (if (const? x)
        (const-value x)
        (const-value (infer-expr x))))

  (define (constant-fixnum? x)
    ;; TODO: use target-fixnum?.
    (or (and (const? x)
             (fixnum? (const-value x)))
        (and (infer? x)
             (const? (infer-expr x))
             (fixnum? (const-value (infer-expr x))))))

  (define (cg-branch-instrumentation)
    ;; AFL instrumentation.
    (when use-branch-instrumentation
      (let (#;(r (reg))
            (current-location (compile-time-random-u16)))
        (emit `(mov r8 (mem64+ afl-location))
              `(xor r8 ,current-location)
              `(inc (mem8+ afl-map r8))
              `(shr r8w 1)
              `(mov (mem64+ afl-location) r8)))))

  (define (cg-get-i/o eax operand* ctxt env)
    (let ((port (car operand*)))
      (if (constant? port)
          (emit `(mov edx ,(constant-value port)))
          (emit `(mov rdx ,(cg port 'value env #f))
                `(sar edx ,(shift 'fixnum))))
      (unless (eq? eax 'eax)
        (emit `(xor eax eax)))
      (emit `(in ,eax dx)
            `(shl rax ,(shift 'fixnum)))
      'rax))

  (define (cg-get-i/o-n! mem+ operand* ctxt env)
    ;; get-i/o-uXX-n! port address count.
    (let ((port (car operand*))
          (address (cadr operand*))
          (count (caddr operand*)))
      (let ((Tcount (reg)) (Taddress (reg)))
        (emit `(mov ,Tcount ,(cg count 'value env #f)))
        (emit `(mov ,Taddress ,(cg address 'value env #f)))
        (emit `(mov rdx ,(cg port 'value env #f))
              `(sar edx ,(shift 'fixnum)))
        ;; Load rcx with the number of uXX units to be read.
        (emit `(mov rcx ,Tcount)
              `(sar rcx ,(fx+ (shift 'fixnum)
                              (case mem+
                                ((mem8+) 0)
                                ((mem16+) 1)
                                ((mem32+) 2))))
              `(mov rdi ,Taddress)
              `(sar rdi ,(shift 'fixnum)) ;address, in bytes
              `(rep.ins (,mem+ rdi) dx))
        (cg-void ctxt))))

  (define (cg-put-i/o eax operand* ctxt env)
    ;; TODO: rdx is not a good temporary
    (let ((port (car operand*))
          (value (cadr operand*)))
      (cond ((and (constant? port)
                  (integer? (constant-value port))
                  (< (constant-value port) 256))
             (let ((port (exact (constant-value port))))
               (emit `(mov rax ,(cg value 'value env #f))
                     `(sar eax ,(shift 'fixnum))
                     `(out ,port ,eax))))
            (else
             (if (constant? port)
                 (emit `(mov edx ,(constant-value port)))
                 (emit `(mov rdx ,(cg port 'value env #f))
                       `(sar edx ,(shift 'fixnum))))
             (emit `(mov rax ,(cg value 'value env #f))
                   `(sar rax ,(shift 'fixnum))
                   `(out dx ,eax))))
      (cg-void ctxt)))

  (define (cg-get-mem mem+ movzx/sx reg operand* ctxt env)
    ;; (get-mem-uNN addr). addr must be aligned. XXX: make sure
    ;; that the optimizer never removes these loads.
    (let ((addr (car operand*)))
      (cond ((and (constant? addr) (< (constant-value addr) #x7fffffff))
             (emit `(,movzx/sx ,reg (,mem+ ,(constant-value addr)))))
            (else
             (emit `(mov rax ,(cg addr 'value env #f)))
             (emit `(sar rax ,(shift 'fixnum)))
             (emit `(,movzx/sx ,reg (,mem+ rax)))))
      (emit `(sal rax ,(shift 'fixnum)))
      'rax))

  (define (cg-put-mem mem+ rax-part operand* ctxt env)
    (let ((addr (car operand*)) (value (cadr operand*)))
      (let ((loc (reg)) (data (reg)))
        (emit `(mov ,loc ,(cg value 'value env #f)))
        (emit `(mov ,data ,(cg addr 'value env #f)))
        (emit `(sar ,data ,(shift 'fixnum)))
        (emit `(mov rax ,loc))
        (emit `(sar rax ,(shift 'fixnum)))
        (emit `(mov (,mem+ ,data) ,rax-part))
        (cg-void ctxt))))

  (define (%cg-helper-bv-idx-check mem+ bv-reg idx-reg idx-op)
    ;; Partially range check the index. Case analysis: if n is near
    ;; greatest-fixnum, then n+1 or n+3 wraps around to a negative
    ;; index. If n>=0 and near zero, the comparison works as
    ;; expected. If n<-3 the comparison also works (because it is an
    ;; unsigned comparison). If n+1>=0 or n+3>=0, then it does not
    ;; work as expected, but the index is misaligned, so this will
    ;; be caught anyway.
    (let ((r (if (eq? mem+ 'mem8+)
                 idx-reg
                 (let ((tmp (reg)))
                   (emit `(lea ,tmp (mem+ ,idx-reg
                                          ,(immediate (case mem+
                                                        ((mem16+) 1)
                                                        ((mem32+) 3)
                                                        (else 7))))))
                   tmp))))
      ;; Type check bv, verify index in bounds.
      (with-restart-handler (index proceed)
        (emit `(cmp ,r (mem64+ ,bv-reg ,(fx- (tag 'bytevector))))
              `(jb ,proceed)))))

  (define (cg-helper-bv-idx-check/u8 mem+ bv-reg idx-reg idx-op)
    ;; Check for a non-negative fixnum.
    (if (inferred-as? idx-op 'fixnum)
        (with-restart-handler (index proceed)
          (emit `(test ,idx-reg ,idx-reg)
                `(jns ,proceed)))
        (let ((m (reg)))
          (with-restart-handler (index proceed)
            (emit `(mov ,m ,(bitwise-ior (mask 'fixnum)
                                         (expt 2 63)))
                  `(test ,idx-reg ,m)
                  `(jz ,proceed)))))
    (%cg-helper-bv-idx-check mem+ bv-reg idx-reg idx-op))

  (define (cg-helper-bv-idx-check mem+ bv-reg idx-reg idx-op)
    ;; Type check index.
    (cg-check-fixnum? idx-reg idx-op)
    (%cg-helper-bv-idx-check mem+ bv-reg idx-reg idx-op))

  (define (cg-bytevector-native-set! mem+ mvreg class bits
                                     operand* ctxt env)
    ;; Verify these evil things: idx is a fixnum, idx is in range of
    ;; the bytevector length, the value is in the proper range.
    (assert (null? (cdddr operand*)))
    (let ((bv-reg (reg)) (idx-reg (reg)) (n-reg 'rdx) (n-reg/32 'edx))
      (emit `(mov ,bv-reg ,(cg (car operand*) 'value env #f)))
      (emit `(mov ,idx-reg ,(cg (cadr operand*) 'value env #f)))
      (emit `(mov ,n-reg ,(cg (caddr operand*) 'value env #f)))
      (cg-helper-bv-idx-check mem+ bv-reg idx-reg (cadr operand*))
      ;; Check that the number is ok.
      (case class
        ((ieee)
         ;; Type check.
         (with-restart-handler (bytevector-value proceed)
           (emit `(cmp ,n-reg/32 ,(tag 'flonum))
                 `(jz ,proceed))))
        ((u)
         ;; Combined type and range check.
         (let ((m (reg)))
           (emit `(mov ,m ,(bitwise-not (immediate (- (expt 2 bits) 1)))))
           (with-restart-handler (bytevector-value proceed)
             (emit `(test ,n-reg ,m)
                   `(jz ,proceed)))))
        (else
         (let ((tmp (reg)))
           (cg-check-fixnum? n-reg (caddr operand*))
           (cond ((fx<=? bits 16)
                  ;; Standard range check trick.
                  (emit `(lea ,tmp (mem+ ,n-reg ,(immediate (expt 2 (- bits 1))))))
                  (with-restart-handler (bytevector-value proceed)
                    (emit `(cmp ,tmp ,(immediate (- (expt 2 bits) 1)))
                          `(jbe ,proceed))))
                 (else
                  (emit `(mov ,tmp ,n-reg))
                  ;; Shift off everything except the sign, and
                  ;; see if it's 0 or 1.
                  (emit `(sar ,tmp ,(+ (shift 'fixnum) bits -1))
                        `(add ,tmp 1))
                  (with-restart-handler (bytevector-value proceed)
                    (emit `(cmp ,tmp 1)
                          `(jbe ,proceed))))))))
      ;; Do the write.
      (case class
        ((ieee)
         (emit `(shr ,n-reg ,(shift 'flonum))))
        (else
         (emit `(sar ,n-reg ,(shift 'fixnum))))) ;as mvreg
      ;; XXX: Checks alignment of idx, and is also part of the range
      ;; check.
      (emit `(sar ,idx-reg ,(shift 'fixnum))
            `(mov (,mem+ ,bv-reg ,idx-reg 8 8 ,(fx- (tag 'bytevector))) ,mvreg))
      (cg-void ctxt)))

  ;; TODO: do more testing of this code (in particular the range check).
  (define (cg-bv-known-ref name bv idx endian ctxt env)
    ;; The endianness is known to be little or big, and the number
    ;; of operands is correct (endianness always present).
    (define r32 'eax)                 ;TODO: classed pseudo regs
    (define r64 'rax)
    (let-values
        (((native? size mask class mov mvreg mem+)
          (case name
            ((bytevector-u8-ref)         (values #t 1 #b000 'u 'movzx  r32 'mem8+))
            ((bytevector-s8-ref)         (values #t 1 #b000 's 'movsx  r64 'mem8+))
            ((bytevector-u16-ref)        (values #f 2 #b001 'u 'movzx  r32 'mem16+))
            ((bytevector-s16-ref)        (values #f 2 #b001 's 'movsx  r64 'mem16+))
            ((bytevector-u32-ref)        (values #f 4 #b011 'u 'mov    r32 'mem32+))
            ((bytevector-s32-ref)        (values #f 4 #b011 's 'movsxd r64 'mem32+))
            ((bytevector-u16-native-ref) (values #t 2 #b001 'u 'movzx  r32 'mem16+))
            ((bytevector-s16-native-ref) (values #t 2 #b001 's 'movsx  r64 'mem16+))
            ((bytevector-u32-native-ref) (values #t 4 #b011 'u 'mov    r32 'mem32+))
            ((bytevector-s32-native-ref) (values #t 4 #b011 's 'movsxd r64 'mem32+))
            ((bytevector-ieee-single-native-ref) (values #t 4 #b011 'ieee 'mov r32 'mem32+))
            (else
             (error 'cg-bv-known-ref "Not prepared to handle this" name)))))
      (define (do-static-index-unaligned bv-reg idx bswap?)
        ;; idx is a register or a fixnum.
        (assert (memq class '(s u)))
        (case size
          ((2)
           ;; Two u8 reads.
           (emit `(xor eax eax))
           (let-values (((al ah)
                         (if bswap? (values 'ah 'al) (values 'al 'ah))))
             (emit `(mov ,al (mem8+ ,bv-reg ,idx 0 8 8 ,(fx- (tag 'bytevector))))
                   `(mov ,ah (mem8+ ,bv-reg ,idx 1 8 8 ,(fx- (tag 'bytevector)))))
             (when (eq? class 's)
               (emit `(,mov ,mvreg ax)))
             (emit `(sal rax ,(shift 'fixnum)))
             'rax))
          ((4)
           (cond
             ((fixnum? idx)
              ;; Two aligned u32 reads, shifting the result.
              (let* ((idx^ (fxand idx (fxnot mask)))
                     (a (fx* 8 (fx- idx idx^))))  ;8, 16, 24
                ;; Load the requested bytes into eax:edx,
                ;; then shift them into eax.
                (emit `(mov eax (mem32+ ,bv-reg ,idx^ 0 8 8 ,(fx- (tag 'bytevector))))
                      `(mov edx (mem32+ ,bv-reg ,idx^ 4 8 8 ,(fx- (tag 'bytevector)))))
                (emit `(shrd eax edx ,a))
                (assert (eq? r32 'eax))
                ;; Now do sign extension and endianness swapping.
                (when bswap?
                  (emit '(bswap eax)))))
             (else
              ;; Four u8 reads.
              (let-values (((i0 i1 i2 i3) (if bswap? (values 0 1 2 3) (values 3 2 1 0))))
                (emit `(movzx eax (mem8+ ,bv-reg ,idx ,i1 8 8 ,(fx- (tag 'bytevector))))
                      `(mov ah (mem8+ ,bv-reg ,idx ,i0 8 8 ,(fx- (tag 'bytevector))))
                      `(shl eax 16)
                      `(mov ah (mem8+ ,bv-reg ,idx ,i2 8 8 ,(fx- (tag 'bytevector))))
                      `(mov al (mem8+ ,bv-reg ,idx ,i3 8 8 ,(fx- (tag 'bytevector))))))))
           ;; Still u32/s32
           (when (eq? class 's)
             (emit '(movsxd rax eax)))
           (emit `(sal rax ,(shift 'fixnum)))
           'rax)))
      (define (do-static-index bv-reg idx bswap?)
        ;; The index is static.
        (let ((idx (constant-value idx)))
          (with-restart-handler (index proceed)
            ;; Verify the index and implicitly check that it's a bytevector.
            (emit `(cmp (mem64+ ,bv-reg ,(fx- (tag 'bytevector)))
                        ,(immediate (+ idx size -1)))
                  `(jnbe ,proceed)))
          (cond
            ((or native? (zero? (fxand idx mask)))
             ;; The index is aligned or can be assumed to be.
             (let ((mref `(,mem+ ,bv-reg ,idx 8 8 ,(fx- (tag 'bytevector)))))
               ;; Do the memory reference, possibly followed
               ;; by endianness swapping. Return a register.
               (cond
                 ((or (eqv? size 1) (not bswap?))
                  (emit `(,mov ,mvreg ,mref))
                  (emit `(sal ,r64 ,(shift 'fixnum)))
                  r64)
                 ((eq? class 's)
                  ;; The sign bit is only in the right place
                  ;; after the xchg/bswap.
                  (case size
                    ((2)
                     (emit `(movzx ax ,mref)
                           `(xchg al ah)
                           `(,mov ,mvreg ax)))
                    ((4)
                     (emit `(mov eax ,mref)
                           `(bswap eax)
                           `(,mov ,mvreg eax))))
                  (emit `(sal ,r64 ,(shift 'fixnum)))
                  r64)
                 (else
                  ;; Unsigned reference.
                  (emit `(,mov ,mvreg ,mref))
                  (case size
                    ((2)
                     (assert (eq? r32 'eax))
                     (emit `(xchg al ah)))
                    ((4)
                     (emit `(bswap ,r32))))
                  (emit `(sal ,r64 ,(shift 'fixnum)))
                  r64))))
            (else
             ;; The index is not aligned.
             (do-static-index-unaligned bv-reg idx bswap?)))))
      (define (do-dynamic-index bv-reg idx bswap?)
        ;; The index is dynamic.
        (let ((idx-reg (reg)))
          (emit `(mov ,idx-reg ,(cg idx 'value env #f)))
          (cond
            (native?
             ;; The index can be assumed to be aligned. Check the
             ;; index and also that it's a bytevector.
             (cg-helper-bv-idx-check mem+ bv-reg idx-reg idx)
             (emit `(sar ,idx-reg ,(shift 'fixnum)))
             ;; XXX: Checks alignment of idx, and is also part
             ;; of the range check.
             (emit `(,mov ,mvreg (,mem+ ,bv-reg ,idx-reg 8 8 ,(fx- (tag 'bytevector)))))
             ;; FIXME: what of the bswap?
             (case class
               ((ieee)
                (emit `(sal ,r64 ,(shift 'flonum))
                      `(or ,r64 ,(tag 'flonum))))
               (else
                (emit `(sal ,r64 ,(shift 'fixnum)))))
             r64)
            (else
             ;; The index might not be aligned. The memory
             ;; references generated here will not help trap -1
             ;; and -3.
             (cg-helper-bv-idx-check/u8 mem+ bv-reg idx-reg idx)
             (emit `(sar ,idx-reg ,(shift 'fixnum)))
             (do-static-index-unaligned bv-reg idx-reg bswap?)))))
      ;; Static or dynamic index argument.
      (let ((bv-reg (reg))
            (bswap? (not (eq? endian 'little))))
        (emit `(mov ,bv-reg ,(cg bv 'value env #f)))
        (cond ((and (constant? idx) (fixnum? (constant-value idx))
                    (< -1 (constant-value idx) (expt 2 30))
                    (memq class '(s u)))
               (do-static-index bv-reg idx bswap?))
              (else
               (do-dynamic-index bv-reg idx bswap?))))))

  (define (cg-syscall operand* ctxt env)
    ;; Linux amd64 syscalls.
    (define reg* '(rax rdi rsi rdx r10 r8 r9))
    (assert (<= 1 (length operand*) 7))
    (let ((acc (reg))
          (preg* (let lp ((operand* operand*) (reg* reg*) (preg* '()))
                   (if (null? operand*)
                       (reverse preg*)
                       (let ((preg (reg)))
                         (emit `(mov ,preg ,(cg (car operand*) 'value env #f)))
                         (lp (cdr operand*) (cdr reg*) (cons preg preg*)))))))
      (emit `(mov ,acc 0))          ;for mass typechecking of the operands
      (let lp ((operand* operand*) (reg* reg*) (preg* preg*))
        (when (pair? operand*)
          (let ((operand (car operand*)) (hreg (car reg*)) (preg (car preg*)))
            (emit `(mov ,hreg ,preg))
            (unless (inferred-as? operand 'fixnum)
              (emit `(or ,acc ,hreg)))
            (emit `(sar ,hreg ,(shift 'fixnum)))
            (when (constant-fixnum? operand)
              (emit `(mov ,hreg ,(constant-value operand))))
            (lp (cdr operand*) (cdr reg*) (cdr preg*)))))
      (with-restart-handler (fixnum? proceed)
        (emit `(test ,acc ,(mask 'fixnum))
              `(jz ,proceed)))
      (emit '(%comment likely)
            `(%comment call linux-syscall ,(length operand*))
            `(syscall)              ; rcx and r11 are clobbered
            `(sal rax ,(shift 'fixnum)))
      ;; TODO: might need to construct a bignum. should maybe
      ;; return two values instead?
      'rax))

  (define (cg-test ctxt rflags.x)
    ;; Used after rflags has been updated. When not called from
    ;; inside an if expression it constructs a boolean.
    (case ctxt
      ((test) rflags.x)
      (else
       (let ((setcc (or (rflags->SETcc rflags.x)
                        (error 'cg-test "Unknown flag" rflags.x))))
         ;; Can only use ah, bh, ch or dh.
         (emit `(mov rax ,(immediate #f)))
         (emit `(,setcc ah)))
       'rax)))

  ;; TODO
  ;; (define (result-nonnegative-fixnum? x)
  ;;   (and (funcall? x)
  ;;        (primref? (funcall-operator x))
  ;;        (memq (primref-name (funcall-operator x))
  ;;              '(length bytevector-length string-length
  ;;                       ))))

  (define (cg-fxdiv-and-mod operand* ctxt env tail?)
    ;; TODO: can the magic constants do the Eucliedan adjustment
    ;; automatically? TODO: is it actually any faster than IDIV?
    (let ((d (constant-value (cadr operand*))))
      ;; TODO: 64?
      (let-values (((M s) (division-magic d 64)))
        (let ((n-reg (reg)) (M-reg (reg)) (r (reg)))
          (emit `(mov ,n-reg ,(cg (car operand*) ctxt env tail?)))
          (cg-check-fixnum? n-reg (car operand*))
          (emit `(sar ,n-reg ,(shift 'fixnum)))
          (emit `(mov ,r ,n-reg))
          ;; if    d<0 && n<0: n = n + d + 1
          ;; elif: d>0 && n<0: n = n - d + 1
          (cond ((negative? d)
                 ;; if n<0: n = n + d + 1
                 (let ((t (reg)))
                   ;; n+=(n>>w)&(d+1)
                   (emit `(lea ,t (mem+ ,n-reg ,(+ d 1)))
                         `(test ,n-reg ,n-reg)
                         `(cmovs ,n-reg ,t))))
                (else
                 ;; if n<0: n = n - d + 1
                 (let ((t (reg)))
                   (emit `(lea ,t (mem+ ,n-reg ,(- d) 1))
                         `(test ,n-reg ,n-reg)
                         `(cmovs ,n-reg ,t)))))
          ;; TODO: would it make sense to swap around the args to IMUL?
          (emit `(mov ,M-reg ,M))
          (emit `(mov rax ,n-reg))
          (emit `(imul ,M-reg))       ;rdx:rax <- rax * M-reg
          (cond ((and (> d 0) (< M 0))
                 (emit `(add rdx ,n-reg)))
                ((and (< d 0) (> M 0))
                 (emit `(sub rdx ,n-reg))))
          (unless (eqv? s 0)
            (emit `(sar rdx ,s)))
          ;; The quotient is now in rdx. TODO: does this just undo
          ;; the previous adjustment? Add one to rdx if rdx < 0.
          (let ((t (reg)))
            (emit `(mov ,t rdx)
                  `(shr ,t ,(- 64 1))
                  `(add rdx ,t)))
          ;; The remainder ends up in r.
          (let ((t (reg)))
            ;; TODO: BLAH. d can't always be an immediate!
            ;; TODO: adjust rdx before? maybe copy unadjusted n into r, too?
            (emit `(imul ,t rdx ,d)
                  `(sub ,r ,t)))
          (emit `(sal rdx ,(shift 'fixnum))
                `(sal ,r ,(shift 'fixnum)))
          (values 'rdx r)))))

  (define (cg-fxcmp-predicate name operand* ctxt env tail?)
    ;; Generates code for fx=?, fx<?, etc.
    (cond ((and (pair? operand*) (pair? (cdr operand*)) (null? (cddr operand*)))
           (let ((condition
                  (case name
                    ((fx=?) 'rflags.z)
                    ((fx<?) 'rflags.l)
                    ((fx>?) 'rflags.g)
                    ((fx>=?) 'rflags.ge)
                    ((fx<=?) 'rflags.le)
                    (else
                     (error 'cg-fxcmp-predicate "Internal error"))))
                 (op1 (reg)) (op2 (reg)))
             (emit `(mov ,op1 ,(cg (car operand*) 'value env #f)))
             (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
             (cg-check-fixnum? op1 (car operand*)
                               op2 (cadr operand*))
             (emit `(cmp ,op1 ,op2))
             (cg-test ctxt condition)))
          ;; TODO: rewrite using match
          ((and (memq name '(fx<=? #;fx<? fx>= #;fx>?))
                (pair? operand*) (pair? (cdr operand*)) (pair? (cddr operand*))
                (null? (cdddr operand*))
                (constant? (car operand*))
                (fixnum? (constant-value (car operand*)))
                (or (and (constant? (caddr operand*))
                         (fixnum? (constant-value (caddr operand*)))
                         (fx<=? (constant-value (car operand*))
                                (constant-value (caddr operand*))))
                    ;; (fx<=? 0 x (length x)) will always fulfill
                    ;; the condition that a ≤ b.
                    #;(and (fx<=? (const-value (car operand*)) 0)
                           (result-nonnegative-fixnum? (caddr operand*)))))
           ;; Hacker's Delight 4-1. a ≤ x ≤ b  ==>  x-a ≤u b-a
           (case name
             ((fx<=?)
              (let ((a (constant-value (car operand*)))
                    (x (cadr operand*))
                    (b (constant-value (caddr operand*)))
                    (t0 (reg))
                    (t1 (reg))
                    (t2 (reg)))
                (emit `(mov ,t0 ,(cg x 'value env #f)))
                (cg-check-fixnum? t0 x)
                (cond ((eqv? a 0)
                       (emit `(mov ,t1 ,t0)))
                      (else
                       ;; TODO: check the range on -a
                       (emit `(lea ,t1 (mem+ ,t0 ,(immediate (- a)))))))
                (emit `(mov ,t2 ,(immediate (fx- b a)))
                      `(cmp ,t1 ,t2))
                (cg-test ctxt 'rflags.be)))
             #;((fx<?)
                )
             ((fx>=?)
              (cg-fxcmp-predicate fx<=? (reverse operand*) ctxt env tail?))
             #;((fx>?)
                (cg-fxcmp-predicate fx<? (reverse operand*) ctxt env tail?))))
          (else
           (cg-primcall-proc name operand* ctxt env tail?))))

  (define (cg-type-predicate operand* type ctxt env)
    ;; TODO: partial access of pseudoregisters
    (emit `(mov rax ,(cg (car operand*) 'value env #f)))
    (let ((M (mask type))
          (T (tag type)))
      (cond ((fxzero? T)
             (emit `(test eax ,M)))
            ((fx=? M #xff)
             (emit `(cmp al ,T)))
            ((fx<=? M #xff)
             (emit `(and al ,M)
                   `(cmp al ,T)))
            (else
             (emit `(and eax ,M)
                   `(cmp eax ,T)))))
    (cg-test ctxt 'rflags.z))

  (define (cg-const-predicate operand* v ctxt env)
    ;; compare with v
    (emit `(mov rax ,(cg (car operand*) 'value env #f)))
    ;; TODO: it is not necessary to compare with all of rax here.
    (emit `(cmp rax ,(immediate v)))
    (cg-test ctxt 'rflags.z))

  (define (cg-length operand* type env)
    ;; boxes do not really have lengths, but the first field is the
    ;; type object.
    (assert (memq type '(bytevector string vector box)))
    (let ((r0 (reg)) (r1 (reg)))
      (emit `(mov ,r0 ,(cg (car operand*) 'value env #f))
            `(mov ,r1 (mem64+ ,r0 ,(fx- (tag type)))))
      r1))

  (define (cg-void ctxt)
    ;; XXX: the assembler needs some sort of $ label if the version
    ;; below is going to be better.
    (unless (eq? ctxt 'effect)
      (if #t
          (emit `(lea rax (mem+ rip))
                `(shl rax ,(shift 'void))
                `(or rax ,(tag 'void)))
          (let ((l (vector 'void-here)))
            (emit `(%label ,l)
                  `(mov rax (+ (bitwise-arithmetic-shift-left ,l ,(shift 'void))
                               ,(tag 'void)))))))
    'rax)

  (define (cg-source src)
    (when (const? src)
      (let ((source (const-value src)))
        (when source
          (emit `(%comment source ,source))))))

  (define (cg-primcall-proc name operand* ctxt env tail?)
    ;; TODO: do this in a separate pass! also see the primref
    ;; code! this fallthrough case is what should happen if the
    ;; primitive can't be open-coded, too (e.g. bad arity).
    (cond ((primlocs name) =>
           (lambda (location)
             (case name
               ((error assertion-violation assertion-error
                       raise raise-continuable)
                ;; Tell the basic block reordering that branches to
                ;; these blocks are probably not going to be taken.
                ;; TODO: (apply assertion-violation ...)
                (emit '(%comment unlikely))))
             (cg (make-funcall (make-funcall (make-primref '$global-ref)
                                             (list (make-const location #f))
                                             #f #f)
                               operand* #f #f)
                 ctxt env tail?)))
          (else
           (error who "TODO: more primitives..." name))))

  (define (cg-explicit-check-pair? reg)
    (when explicit-checks
      (let ((t (reg)))
        (with-restart-handler (pair? proceed)
          (emit `(mov ,t ,reg)
                `(and ,t ,(mask 'pair))
                `(cmp ,t ,(tag 'pair))
                `(je ,proceed))))))

  (define (cg-explicit-check-vector? reg)
    (when explicit-checks
      (let ((t (reg)))
        (with-restart-handler (vector? proceed)
          (emit `(mov ,t ,reg)
                `(and ,t ,(mask 'vector))
                `(cmp ,t ,(tag 'vector))
                `(je ,proceed))))))

  ;; Was the type of the operand inferred as `type'?
  (define (inferred-as? op type)
    (cond ((and (infer? op) (assq 'type (infer-facts op)))
           => (lambda (x) (eq? (cdr x) type)))
          (else #f)))

  (define cg-check-fixnum?
    (case-lambda
      ((reg1 op)
       (unless (inferred-as? op 'fixnum)
         (with-restart-handler (fixnum? proceed)
           (emit `(test ,reg1 ,(mask 'fixnum))
                 `(jz ,proceed)))))
      ((reg1 op1 reg2 op2)
       (cond
         ((inferred-as? op1 'fixnum)
          (cg-check-fixnum? reg2 op2))
         ((inferred-as? op2 'fixnum)
          (cg-check-fixnum? reg1 op1))
         (else
          (let ((tmp (reg)))
            (with-restart-handler (fixnum? proceed)
              (emit `(mov ,tmp ,reg1))
              (emit `(or ,tmp ,reg2)
                    `(test ,tmp ,(mask 'fixnum))
                    `(jz ,proceed)))))))))

  (define cg-check-flonum?
    (case-lambda
      ((reg1 op)
       (unless (inferred-as? op 'flonum)
         (let ((tmp (reg)))
           (with-restart-handler (flonum? proceed)
             (emit `(mov ,tmp ,reg1)
                   `(and ,tmp ,(mask 'flonum))
                   `(cmp ,tmp ,(tag 'flonum))
                   `(je ,proceed))))))
      ((reg1 op1 reg2 op2)
       (cond
         ((inferred-as? op1 'flonum)
          (cg-check-flonum? reg2 op2))
         ((inferred-as? op2 'flonum)
          (cg-check-flonum? reg1 op1))
         (else
          (let ((tmp (reg)))
            (with-restart-handler (flonum? proceed)
              (emit `(mov ,tmp ,reg1))
              (emit `(and ,tmp ,reg2)
                    `(and ,tmp ,(mask 'flonum))
                    `(cmp ,tmp ,(tag 'flonum))
                    `(je ,proceed)))))))))

  (define cg-check-bytevector?
    (case-lambda
      ((reg1 op)
       (unless (inferred-as? op 'bytevector)
         (with-restart-handler (bytevector? proceed)
           (let ((tmp (reg)))
             ;; This sequence should get peephole optimized to a byte
             ;; register compare.
             (emit `(mov ,tmp ,reg1)
                   `(and ,tmp ,(mask 'bytevector))
                   `(cmp ,tmp ,(tag 'bytevector))
                   `(je ,proceed))))))))

  (define (cg-allocation size)
    ;; Generate a heap overflow check. After this code it is ok to
    ;; use the memory in [%alloc,%alloc+size[.
    (unless (eqv? size 0)
      (let ((alloc-ok (vector 'alloc-ok)))
        (emit `(sub ,%heap-rem ,size)
              `(jns ,alloc-ok))
        ;; Indicate where the stack begins. TODO: save and reload
        ;; registers.
        (emit '(mov rdi rsp))
        (with-new-frame ((env))
          (emit '(call stop-and-copy)))
        (emit `(%label ,alloc-ok)))))

  ;; Create a procedure that dispatches on the its first argument.
  ;; This is mostly about about making a procedure that's small
  ;; enough that the optimizer doesn't choke on it.
  (define-syntax lambda-case-dispatch
    (lambda (x)
      (syntax-case x (else)
        ((_ (name formal* ...)
            ((symlist* ...) . body*) ...
            (else else-expr))
         (with-syntax ([(t* ...) (generate-temporaries #'((symlist* ...) ...))])
           #'(letrec ([t* (lambda (name formal* ...) . body*)] ...)
               (lambda (name formal* ...)
                 (case name
                   ((symlist* ...) (t* name formal* ...))
                   ...
                   (else else-expr)))))))))

  #;
  (define-syntax lambda-case-dispatch
    (lambda (x)
      (syntax-case x (else)
        ((_ (name formal* ...)
            ((symlist* ...) . body*) ...
            (else else-expr))
         (with-syntax ([(t* ...) (generate-temporaries #'((symlist* ...) ...))])
           #'(letrec ([t* (lambda (name formal* ...) . body*)] ...)
               (let ((lookup (make-eq-hashtable)))
                 (for-each
                  (lambda (sym)
                    (hashtable-set! lookup sym t*))
                  '(symlist* ...))
                 ...
                 (lambda (name formal* ...)
                   (cond
                     ((hashtable-ref lookup name #f) =>
                      (lambda (proc) (proc name formal* ...)))
                     (else else-expr))))))))))

  ;; TODO: there are a lot of primitives and they need to be
  ;; organized. cp0 needs information about them (foldable, etc).
  ;; cp0 also needs to be able to call them. the arities have to
  ;; be checked before a call to a primitive becomes a primcall.
  ;; calls with bad arities should be funcalls (or warning +
  ;; residualized error). FIXME: these _really_ should check the
  ;; arity.
  (define cg-primcall
    (lambda-case-dispatch (name operand* ctxt env tail?)
      ((null?) (cg-const-predicate operand* '() ctxt env))
      ((eof-object?) (cg-const-predicate operand* (eof-object) ctxt env))
      ((boolean?) (cg-type-predicate operand* 'boolean ctxt env))
      ((pair?) (cg-type-predicate operand* 'pair ctxt env))
      ((vector?) (cg-type-predicate operand* 'vector ctxt env))
      ((char?) (cg-type-predicate operand* 'char ctxt env))
      ((fixnum?) (cg-type-predicate operand* 'fixnum ctxt env))
      ((string?) (cg-type-predicate operand* 'string ctxt env))
      ((bytevector?) (cg-type-predicate operand* 'bytevector ctxt env))
      ((procedure?) (cg-type-predicate operand* 'procedure ctxt env))
      ((flonum?) (cg-type-predicate operand* 'flonum ctxt env))
      (($immsym?) (cg-type-predicate operand* 'immsym ctxt env))
      (($box?) (cg-type-predicate operand* 'box ctxt env))
      (($box-header?) (cg-type-predicate operand* 'box-header ctxt env))

      ((void) (cg-void ctxt))
      (($void?) (cg-type-predicate operand* 'void ctxt env))
      ((eof-object) (emit `(mov rax ,(immediate (eof-object)))) 'rax)

      ((bytevector-length) (cg-length operand* 'bytevector env))
      ((vector-length) (cg-length operand* 'vector env))
      ((string-length) (cg-length operand* 'string env))
      (($box-type) (cg-length operand* 'box env))

      ((not)
       (assert (null? (cdr operand*))) ;FIXME!
       (let* ((result (cg (car operand*) 'test env #f))
              (flag (or (not-rflags result)
                        (let ((t (reg)))
                          (emit `(mov ,t ,result))
                          (emit `(cmp ,t ,(immediate #f)))
                          'rflags.z))))
         (cg-test ctxt flag)))
      ((eq?)
       (assert (eqv? (length operand*) 2))
       (let ((operand* (if (constant? (car operand*))
                           (reverse operand*)
                           operand*)))
         (let ((op1 (reg)) (op2 (reg)))
           (emit `(mov ,op1 ,(cg (car operand*) 'value env #f)))
           (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
           (emit `(cmp ,op1 ,op2))
           (cg-test ctxt 'rflags.z))))
      (($cons)                          ;safe
       (let ((CAR (reg)) (CDR (reg)) (PAIR (reg)))
         (emit `(mov ,CAR ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,CDR ,(cg (cadr operand*) 'value env #f)))
         (cg-allocation 16)
         (emit `(mov (mem64+ ,%alloc) ,CAR)
               `(mov (mem64+ ,%alloc 8) ,CDR)
               `(lea ,PAIR (mem+ ,%alloc ,(tag 'pair)))
               `(add ,%alloc 16))
         PAIR))
      ((car)
       (let ((tmp (reg)) (ret (reg)))
         (emit `(mov ,tmp ,(cg (car operand*) 'value env #f)))
         (cg-explicit-check-pair? tmp)
         (emit `(mov ,ret (mem64+ ,tmp ,(fx- (tag 'pair)))))
         ret))
      ((cdr)
       (let ((tmp (reg)) (ret (reg)))
         (emit `(mov ,tmp ,(cg (car operand*) 'value env #f)))
         (cg-explicit-check-pair? tmp)
         (emit `(mov ,ret (mem64+ ,tmp ,(fx+ (fx- (tag 'pair)) 8))))
         ret))
      ((set-car!)
       (with-loc ((env val) (env #f))
         (let ((r (reg)))
           (emit `(mov rax ,(cg (cadr operand*) 'value env #f))
                 `(mov ,val rax))
           (emit `(mov rax ,(cg (car operand*) 'value env #f)))
           (cg-explicit-check-pair? 'rax)
           (emit `(mov ,r ,val)
                 `(mov (mem64+ rax ,(fx- (tag 'pair))) ,r))))
       (cg-void ctxt))
      ((set-cdr!)
       (with-loc ((env pair) (env #f))
         (let ((r (reg)))
           (emit `(mov rax ,(cg (cadr operand*) 'value env #f))
                 `(mov ,pair rax))
           (emit `(mov rax ,(cg (car operand*) 'value env #f)))
           (cg-explicit-check-pair? 'rax)
           (emit `(mov ,r ,pair)
                 `(mov (mem64+ rax ,(fx- (tag 'pair)) 8) ,r))))
       (cg-void ctxt))
      ((caar)
       (assert (null? (cdr operand*)))
       ;; TODO: include all of these.
       (let ((t0 (reg)) (t1 (reg)) (t2 (reg)))
         (emit `(mov ,t0 ,(cg (car operand*) 'value env #f))
               `(mov ,t1 (mem64+ ,t0 ,(fx- (tag 'pair))))
               `(mov ,t2 (mem64+ ,t1 ,(fx- (tag 'pair)))))
         t2))
      ((cadr)
       (assert (null? (cdr operand*)))
       (let ((t0 (reg)) (t1 (reg)) (t2 (reg)))
         (emit `(mov ,t0 ,(cg (car operand*) 'value env #f))
               `(mov ,t1 (mem64+ ,t0 ,(fx- (tag 'pair)) 8))
               `(mov ,t2 (mem64+ ,t1 ,(fx- (tag 'pair)))))
         t2))
      ((cdar)
       (assert (null? (cdr operand*)))
       (let ((t0 (reg)) (t1 (reg)) (t2 (reg)))
         (emit `(mov ,t0 ,(cg (car operand*) 'value env #f))
               `(mov ,t1 (mem64+ ,t0 ,(fx- (tag 'pair))))
               `(mov ,t2 (mem64+ ,t1 ,(fx- (tag 'pair)) 8)))
         t2))
      ((cddr)
       (assert (null? (cdr operand*)))
       (let ((t0 (reg)) (t1 (reg)) (t2 (reg)))
         (emit `(mov ,t0 ,(cg (car operand*) 'value env #f))
               `(mov ,t1 (mem64+ ,t0 ,(fx- (tag 'pair)) 8))
               `(mov ,t2 (mem64+ ,t1 ,(fx- (tag 'pair)) 8)))
         t2))

      ((fixnum-width)
       (let ((r (reg)))
         (emit `(mov ,r ,(immediate (amd64-fixnum-width))))
         r))
      ((fxzero? fxnegative? fxpositive?)
       (let ((value (reg)) (t (reg)))
         (emit `(mov ,value ,(cg (car operand*) 'value env #f)))
         (cg-check-fixnum? value (car operand*))
         (emit `(test ,value ,value))
         (cg-test ctxt
                  (case name
                    ((fxzero?) 'rflags.z)
                    ((fxnegative?) 'rflags.s)
                    (else 'rflags.nle)))))
      ((fx+)
       (unless (and (pair? operand*)
                    (pair? (cdr operand*))
                    (null? (cddr operand*)))
         (assertion-violation 'cg "Bad fx+ args" operand*))
       (let ((tmp1 (reg)) (tmp2 (reg)))
         (let f ((op1 (car operand*))
                 (op2 (cadr operand*)))
           (cond ((constant? op2)
                  (cond ((fixnum? (constant-value op2))
                         (emit `(mov ,tmp1 ,(cg op1 'value env #f)))
                         (cg-check-fixnum? tmp1 op1)
                         (with-restart-handler (overflow proceed)
                           (emit `(add ,tmp1 ,(cg op2 'value env #f)))
                           (emit `(jno ,proceed)))
                         tmp1)
                        (else
                         (cg-primcall-proc name operand* ctxt env tail?))))
                 ((and (constant? op1) (not (constant? op2)))
                  (f op2 op1))
                 (else
                  (emit `(mov ,tmp1 ,(cg op2 'value env #f)))
                  (emit `(mov ,tmp2 ,(cg op1 'value env #f)))
                  (cg-check-fixnum? tmp1 op2
                                    tmp2 op1)
                  (with-restart-handler (overflow proceed)
                    (emit `(add ,tmp1 ,tmp2))
                    (emit `(jno ,proceed)))
                  tmp1)))))
      ((fx-)
       (cond ((null? (cdr operand*))
              (emit `(mov rax ,(cg (car operand*) 'value env #f)))
              (cg-check-fixnum? 'rax (car operand*))
              (with-restart-handler (overflow proceed)
                (emit `(neg rax))
                (emit `(jno ,proceed)))
              'rax)
             (else
              (let ((op1 (reg)) (op2 (reg)))
                (emit `(mov ,op1 ,(cg (car operand*) 'value env #f)))
                (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
                (cg-check-fixnum? op1 (car operand*)
                                  op2 (cadr operand*))
                (with-restart-handler (overflow proceed)
                  (emit `(sub ,op1 ,op2))
                  (emit `(jno ,proceed)))
                op1))))
      ((fxnot)
       (assert (eqv? (length operand*) 1))
       (let ((r (reg)))
         (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
         (cg-check-fixnum? r (car operand*))
         (emit `(not ,r)
               `(and ,r ,(fxnot (mask 'fixnum))))
         r))
      ((fx=?) (cg-fxcmp-predicate 'fx=? operand* ctxt env tail?))
      ((fx<?) (cg-fxcmp-predicate 'fx<? operand* ctxt env tail?))
      ((fx>?) (cg-fxcmp-predicate 'fx>? operand* ctxt env tail?))
      ((fx>=?) (cg-fxcmp-predicate 'fx>=? operand* ctxt env tail?))
      ((fx<=?) (cg-fxcmp-predicate 'fx<=? operand* ctxt env tail?))
      ((fxand)
       (if (eqv? (length operand*) 2)
           (let f ((op1 (car operand*))
                   (op2 (cadr operand*)))
             (cond ((constant? op2)
                    (cond ((fixnum? (constant-value op2))
                           (let* ((tmp (reg)))
                             (emit `(mov ,tmp ,(cg op1 'value env #f)))
                             (cg-check-fixnum? tmp op1)
                             (emit `(and ,tmp ,(cg op2 'value env #f)))
                             tmp))
                          (else
                           (cg-primcall-proc name operand* ctxt env tail?))))
                   ((and (constant? op1) (not (constant? op2)))
                    (f op2 op1))
                   (else
                    (let* ((tmp1 (reg)) (tmp2 (reg)))
                      (emit `(mov ,tmp1 ,(cg op2 'value env #f)))
                      (emit `(mov ,tmp2 ,(cg op1 'value env #f)))
                      (cg-check-fixnum? tmp1 op2
                                        tmp2 op1)
                      (emit `(and ,tmp1 ,tmp2))
                      tmp1))))
           (cg-primcall-proc name operand* ctxt env tail?)))
      ((fxior)
       (cond ((eqv? (length operand*) 2)
              (let ((ret (reg)) (op2 (reg)))
                (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
                (emit `(mov ,ret ,(cg (car operand*) 'value env #f)))
                (cg-check-fixnum? op2 (cadr operand*)
                                  ret (car operand*))
                (emit `(or ,ret ,op2))
                ret))
             (else (cg-primcall-proc name operand* ctxt env tail?))))
      ((fxxor)
       (cond ((eqv? (length operand*) 2)
              (let ((ret (reg)) (op2 (reg)))
                (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
                (emit `(mov ,ret ,(cg (car operand*) 'value env #f)))
                (cg-check-fixnum? op2 (cadr operand*)
                                  ret (car operand*))
                (emit `(xor ,ret ,op2))
                ret))
             (else (cg-primcall-proc name operand* ctxt env tail?))))
      ((fx*)
       (assert (fx=? (length operand*) 2))
       (let f ((op1 (car operand*))
               (op2 (cadr operand*)))
         (cond
           ((and (constant? op1) (not (constant? op2)))
            (f op2 op1))
           ((and (constant? op2)
                 (let ((c (constant-value op2)))
                   (and (fixnum? c)
                        (<= (- (expt 2 31)) c (- (expt 2 31) 1)))))
            (let ((ret (reg)) (tmp (reg)))
              (emit `(mov ,tmp ,(cg op1 'value env #f)))
              (cg-check-fixnum? tmp op1)
              (with-restart-handler (overflow proceed)
                (emit `(imul ,ret ,tmp ,(constant-value op2))
                      `(jno ,proceed)))
              ret))
           (else
            (let ((r1 (reg)) (r2 (reg)))
              (emit `(mov ,r1 ,(cg op1 'value env #f)))
              (emit `(mov ,r2 ,(cg op2 'value env #f)))
              (cg-check-fixnum? r1 op1
                                r2 op2)
              (emit `(sar ,r2 ,(shift 'fixnum)))
              (with-restart-handler (overflow proceed)
                (emit `(imul ,r1 ,r2)      ;R1 <- R1 * R2
                      `(jno ,proceed)))
              r1)))))
      (($fx*/false)                   ;UNSAFE
       (with-loc ((env loc) (env #f))
         (emit `(mov rax ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,loc rax))
         (emit `(mov rax ,(cg (cadr operand*) 'value env #f)))
         (emit `(sar rax ,(shift 'fixnum)))
         (emit `(mov rdi ,loc))
         (emit `(imul rax rdi))       ;RAX <- RAX * RDI
         (emit `(mov rdx ,(immediate #f))
               `(cmovo rax rdx))
         'rax))
      (($fxquotient)                  ;UNSAFE
       ;; XXX: can trigger #DE
       (with-loc ((env loc) (env #f))
         (emit `(mov rax ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,loc rax))
         (emit `(mov rax ,(cg (car operand*) 'value env #f)))
         (emit `(mov rdi ,loc))
         (emit `(cqo))                ;sign-extend RAX into RDX
         (emit `(idiv #;rdx:rax rdi)) ;RAX, RDX <- RDX:RAX / RDI
         (emit `(sal rax ,(shift 'fixnum)))
         'rax))
      (($fxremainder)                 ;UNSAFE
       ;; XXX: can trigger #DE
       (with-loc ((env loc) (env #f))
         (emit `(mov rax ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,loc rax))
         (emit `(mov rax ,(cg (car operand*) 'value env #f)))
         (emit `(mov rdi ,loc))
         (emit `(cqo))                ;sign-extend RAX into RDX
         (emit `(idiv #;rdx:rax rdi)) ;RAX, RDX <- RDX:RAX / RDI
         'rdx))
      ((fxdiv)
       (cond
         ((and (= (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               (let ((n (constant-value (cadr operand*))))
                 (or (eqv? 1 (bitwise-bit-count n))
                     (and (< n -1)
                          (eqv? 1 (bitwise-bit-count (- n)))))))
          ;; fxdiv with known power-of-two divisor.
          (let ((n (constant-value (cadr operand*)))
                (r (reg)))
            (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? r (car operand*))
            (let ((a (bitwise-first-bit-set (abs n))))
              (emit `(sar ,r ,a)
                    `(and ,r ,(fxnot (mask 'fixnum))))
              (when (negative? n)
                (emit `(neg ,r)))
              r)))
         ((and (= (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               ;; Forbidden: (least-fixnum), -1, 0, 1
               (fx<=? 2 (abs (constant-value (cadr operand*)))
                      (amd64-greatest-fixnum)))
          ;; fxdiv with a known good divisor.
          (let-values (((d m) (cg-fxdiv-and-mod operand* ctxt env tail?)))
            d))
         (else
          (cg-primcall-proc name operand* ctxt env tail?))))
      ((fxmod)
       ;; TODO: (eqv? (bitwise-bit-count d) 1), and d for
       ;; (least-fixnum), -1, and 1 are easy.
       (cond
         ((and (= (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               ;; Forbidden: (least-fixnum), -1, 0, 1
               (fx<=? 2 (abs (constant-value (cadr operand*)))
                      (amd64-greatest-fixnum)))
          ;; fxmod with a known good divisor.
          (let-values (((d m) (cg-fxdiv-and-mod operand* ctxt env tail?)))
            m))
         (else
          (cg-primcall-proc name operand* ctxt env tail?))))
      ;; TODO: mvlet and fxdiv-and-mod.

      ((fxarithmetic-shift-right)
       (cond
         ((and (fx=? (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               (fx<=? 0 (constant-value (cadr operand*)) 60))
          ;; fxasr with a known good shift amount
          (let ((r (reg))
                (a (constant-value (cadr operand*))))
            (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? r (car operand*))
            (unless (eqv? a 0)
              (emit `(sar ,r ,a)
                    `(and ,r ,(fxnot (mask 'fixnum)))))
            r))
         (else
          (cg-primcall-proc name operand* ctxt env tail?))))
      ((fxarithmetic-shift-left)
       (cond
         ((and (fx=? (length operand*) 2)
               (constant-fixnum? (cadr operand*))
               (eqv? (constant-value (cadr operand*)) 1))
          ;; (fxasl x 1).
          (let ((r (reg))
                (a (constant-value (cadr operand*))))
            (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
            (cg-check-fixnum? r (car operand*))
            (with-restart-handler (overflow proceed)
              (emit `(sal ,r 1))
              (emit `(jno ,proceed)))
            r))
         (else
          (cg-primcall-proc name operand* ctxt env tail?))))
      (($fxasr/false)                 ;UNSAFE
       (with-loc ((env loc) (env #f))
         (let ((tmp (reg)))
           (emit `(mov rax ,(cg (car operand*) 'value env #f)))
           (emit `(mov ,loc rax))
           (emit `(mov rcx ,(cg (cadr operand*) 'value env #f)))
           (emit `(mov rax ,loc)
                 `(mov ,tmp ,(immediate #f))
                 `(sar rcx ,(shift 'fixnum))
                 `(sar rax cl)
                 `(and rax ,(fxnot (mask 'fixnum)))
                 `(cmp rcx ,(amd64-fixnum-width))
                 `(cmovnb rax ,tmp)))
         'rax))
      (($fxasl/false)                 ;UNSAFE
       (with-loc ((env loc) (env #f))
         (emit `(mov rax ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,loc rax))
         (emit `(mov rcx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov rax ,loc))
         ;; An alternative that might be faster on some machines:
         ;; (emit `(sar rcx #x3)
         ;;       `(mov edx 1)
         ;;       `(shl rdx cl)
         ;;       `(imul rax rdx)
         ;;       `(mov edx ,(immediate #f))
         ;;       `(cmovo rax rdx)
         ;;       `(cmp rcx ,(fixnum-width))
         ;;       `(cmovnb rax rdx))
         (emit `(sar rcx #x3)
               `(mov rdi rax)
               `(shl rax cl)
               `(mov rdx rax)
               `(sar rdx cl)
               `(cmp rdi rdx)
               `(mov edx ,(immediate #f))
               `(cmovnz rax rdx)
               `(cmp rcx ,(amd64-fixnum-width))
               `(cmovnb rax rdx))
         'rax))
      (($fx+/false)                   ;UNSAFE
       (with-loc ((env loc) (env #f))
         (emit `(mov rax ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,loc rax))
         (emit `(mov rax ,(cg (car operand*) 'value env #f))
               `(mov edi ,(immediate #f))
               `(add rax ,loc)
               `(cmovo rax rdi))
         'rax))
      (($fx-/false)                   ;UNSAFE
       (with-loc ((env loc) (env #f))
         (emit `(mov rax ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,loc rax))
         (emit `(mov rax ,(cg (car operand*) 'value env #f))
               `(mov edi ,(immediate #f))
               `(sub rax ,loc)
               `(cmovo rax rdi))
         'rax))
      (($fxlength)
       ;; LZCNT?
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(xor esi esi)
             `(sar rax ,(fx- (shift 'fixnum) 1)) ;n shifted 1 left
             `(mov rdi rax)
             `(not rax)         ;rax (fxnot n)
             `(cmovl rdi rax)   ;rdi: (if (negative? n) (fxnot n) n)
             `(bsr rax rdi)     ;rax: position of first bit
             `(cmovz eax esi)   ;rax is zero if n is zero
             `(sal eax ,(shift 'fixnum)))
       'rax)
      (($fxfirst-bit-set)
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(mov esi 2)
             `(bsf rax rax)
             `(cmovz eax esi)
             `(lea rax (mem+ (* rax 8) ,(* 8 -3))))
       'rax)

      ((integer->char)
       (assert (= (length operand*) 1))
       (let ((r (reg))
             (t (reg)))
         (emit `(mov ,r ,(cg (car operand*) 'value env #f)))
         (cg-check-fixnum? r (car operand*))
         (emit `(lea ,t (mem+ ,r ,(immediate #x-D800))))
         (with-restart-handler (sv? proceed)
           (emit `(cmp ,r ,(immediate #x10FFFF))
                 `(jbe ,proceed)))
         (with-restart-handler (sv? proceed)
           (emit `(cmp ,t ,(immediate (- #xDFFF #xD800)))
                 `(jnbe ,proceed)))
         (assert (eqv? (tag 'fixnum) (fxarithmetic-shift-right
                                      (tag 'char)
                                      (fx- (shift 'char) (shift 'fixnum)))))
         (emit `(shl ,r ,(fx- (shift 'char) (shift 'fixnum)))
               `(or ,r ,(tag 'char)))
         r))
      ((char->integer)
       (assert (= (mask 'char) #b11111111))
       (emit `(mov rax ,(cg (car operand*) 'value env #f)))
       (unless (inferred-as? (car operand*) 'char)
         (with-restart-handler (char? proceed)
           (emit `(cmp al ,(tag 'char))
                 `(je ,proceed))))
       (emit `(shr rax ,(fx- (shift 'char) (shift 'fixnum))))
       'rax)
      (($immsym->fixnum)              ;UNSAFE
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(shr rax ,(fx- (shift 'immsym) (shift 'fixnum)))
             `(and rax ,(fxnot (mask 'fixnum))))
       'rax)
      (($fixnum->immsym)              ;UNSAFE
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(shl rax ,(fx- (shift 'immsym) (shift 'fixnum)))
             `(or rax ,(tag 'immsym)))
       'rax)
      (($void->fixnum)                ;UNSAFE
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(shr rax ,(fx- (shift 'void) (shift 'fixnum)))
             `(and rax ,(fxnot (mask 'fixnum))))
       'rax)
      (($object->fixnum)
       ;; Bits may be lost if the object is an immediate. The caller
       ;; must not care.
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(sal rax ,(shift 'fixnum)))
       'rax)

      (($make-bytevector)             ;UNSAFE
       (let ((ret (reg)) (len (reg)) (req (reg)))
         (emit `(mov ,len ,(cg (car operand*) 'value env #f))
               `(mov ,req ,len)
               `(shr ,req ,(shift 'fixnum))
               `(add ,req ,(+ 8 8 15))
               `(and ,req -16))
         (cg-allocation req)
         (emit `(mov (mem64+ ,%alloc) ,len)
               `(lea ,ret (mem+ ,%alloc ,(tag 'bytevector)))
               `(add ,%alloc ,req))
         (let ((seek-tag (reg)))
           ;; Create the seek mark
           (emit `(sub ,req 8)        ;length field not included
                 `(mov ,seek-tag ,(tag 'seek-mark)) ;the tag is quite large
                 `(shl ,req ,(shift 'seek-mark))
                 `(or ,req ,seek-tag)
                 `(mov (mem64+ ,ret 8 ,(fx- (tag 'bytevector))) ,req)))
         ret))
      ((bytevector-u8-set!)
       (cg-bytevector-native-set! 'mem8+ 'dl 'u 8 operand* ctxt env))
      ((bytevector-s8-set!)
       (cg-bytevector-native-set! 'mem8+ 'dl 's 8 operand* ctxt env))
      ((bytevector-u16-native-set!)
       (cg-bytevector-native-set! 'mem16+ 'dx 'u 16 operand* ctxt env))
      ((bytevector-s16-native-set!)
       (cg-bytevector-native-set! 'mem16+ 'dx 's 16 operand* ctxt env))
      ((bytevector-u32-native-set!)
       (cg-bytevector-native-set! 'mem32+ 'edx 'u 32 operand* ctxt env))
      ((bytevector-s32-native-set!)
       (cg-bytevector-native-set! 'mem32+ 'edx 's 32 operand* ctxt env))
      ((bytevector-ieee-single-native-set!)
       (cg-bytevector-native-set! 'mem32+ 'edx 'ieee 32 operand* ctxt env))

      ((bytevector-ieee-double-native-set!)
       (match operand*
         [(bv idx v)
          (let ((bv-reg (reg)) (idx-reg (reg)) (v-reg (reg)))
            (emit `(mov ,bv-reg ,(cg bv 'value env #f)))
            (emit `(mov ,idx-reg ,(cg idx 'value env #f)))
            (emit `(mov ,v-reg ,(cg v 'value env #f)))
            (cg-helper-bv-idx-check 'mem64+ bv-reg idx-reg idx)
            (cg-check-flonum? v-reg v)
            (emit `(shr ,v-reg ,(shift 'flonum))
                  `(sar ,idx-reg ,(shift 'fixnum))
                  `(movq xmm1 ,v-reg)
                  `(cvtss2sd xmm0 xmm1)
                  `(movq (mem64+ ,bv-reg 8 8 ,idx-reg ,(- (tag 'bytevector))) xmm0))
            (cg-void ctxt))]
         [else
          (cg-primcall-proc name operand* ctxt env tail?)]))

      ((bytevector-s8-ref bytevector-u8-ref
                          bytevector-s16-ref bytevector-u16-ref
                          bytevector-s32-ref bytevector-u32-ref
                          bytevector-s16-native-ref bytevector-u16-native-ref
                          bytevector-s32-native-ref bytevector-u32-native-ref
                          bytevector-ieee-single-native-ref)
       (match (cons name operand*)
         (((or 'bytevector-s16-ref 'bytevector-s32-ref
               'bytevector-u16-ref 'bytevector-u32-ref)
           bv idx (? (lambda (x)
                       (constant? x)
                       (memq (constant-value x) '(little big)))
                     endian))
          ;; Known endianness.
          (cg-bv-known-ref name bv idx (constant-value endian) ctxt env))
         ((_ bv idx)
          (cg-bv-known-ref name bv idx (endianness little) ctxt env))
         (_
          (cg-primcall-proc name operand* ctxt env tail?))))

      ((bytevector-ieee-double-native-ref)
       (match operand*
         [(bv idx)
          (let ((bv-reg (reg)) (idx-reg (reg)))
            (emit `(mov ,bv-reg ,(cg bv 'value env #f)))
            (emit `(mov ,idx-reg ,(cg idx 'value env #f)))
            (cg-helper-bv-idx-check 'mem64+ bv-reg idx-reg idx)
            (emit `(sar ,idx-reg ,(shift 'fixnum))
                  `(movq xmm0 (mem64+ ,bv-reg 8 8 ,idx-reg ,(- (tag 'bytevector))))
                  `(cvtsd2ss xmm0 xmm0)
                  `(movd eax xmm0)
                  `(shl rax ,(shift 'flonum))
                  `(or rax ,(tag 'flonum)))
            'rax)]
         [else
          (cg-primcall-proc name operand* ctxt env tail?)]))

      ((bytevector-address)
       (let ((bv-reg (reg)) (ret (reg)))
         (emit `(mov ,bv-reg ,(cg (car operand*) 'value env #f)))
         (cg-check-bytevector? bv-reg (car operand*))
         (emit `(lea ,ret (mem64+ ,bv-reg 8 8 ,(fx- (tag 'bytevector)))))
         (emit `(sal ,ret ,(shift 'fixnum)))
         ret))

      (($make-vector)                 ;UNSAFE
       (let ((len (reg)) (req (reg)))
         (emit `(mov ,len ,(cg (car operand*) 'value env #f))
               `(lea ,req (mem64+ 8 ,len 15))
               `(and ,req -16))
         (cg-allocation req)
         (let ((ret (reg)))
           (emit `(mov (mem64+ ,%alloc) ,len)
                 `(lea ,ret (mem+ ,%alloc ,(tag 'vector)))
                 `(add ,%alloc ,req))
           ret)))
      ((vector-ref)
       (let* ((vec (reg)) (idx (reg)) (ret (reg)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,vec ,(cg (car operand*) 'value env #f)))
         (cg-explicit-check-vector? vec)
         (if explicit-vector-checks
             (with-restart-handler (index proceed)
               (emit `(cmp ,idx (mem64+ ,vec ,(fx- (tag 'vector))))
                     `(jb ,proceed)))
             (emit `(cmp ,idx (mem64+ ,vec ,(fx- (tag 'vector))))
                   `(cmovnb ,idx ,vec)))
         (emit `(mov ,ret (mem64+ ,vec ,idx 8 ,(fx- (tag 'vector)))))
         ret))
      ((vector-set!)
       (let* ((vec (reg)) (idx (reg)) (value (reg)))
         (emit `(mov ,value ,(cg (caddr operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,vec ,(cg (car operand*) 'value env #f)))
         (cg-explicit-check-vector? vec)
         (when explicit-checks
           (cg-check-fixnum? idx (cadr operand*)))
         (if explicit-vector-checks
             (with-restart-handler (index proceed)
               (emit `(cmp ,idx (mem64+ ,vec ,(fx- (tag 'vector))))
                     `(jb ,proceed)))
             (emit `(cmp ,idx (mem64+ ,vec ,(fx- (tag 'vector))))
                   `(cmovnb ,idx ,vec)))
         (emit `(mov (mem64+ ,vec ,idx 8 ,(fx- (tag 'vector))) ,value))
         (cg-void ctxt)))

      (($make-box)                    ;VERY UNSAFE
       ;; ($make-box type length). Slots will be filled with zero.
       ;; The length refers to the number of slots.
       (let ((req (reg)) (type (reg)) (len (reg)))
         (emit `(mov ,type ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,len ,(cg (cadr operand*) 'value env #f))
               `(lea ,req (mem64+ 8 ,len 15))
               `(and ,req -16))
         (cg-allocation req)
         (let ((ret (reg)))
           (emit `(mov (mem64+ ,%alloc) ,type)
                 `(lea ,ret (mem+ ,%alloc ,(tag 'box)))
                 `(add ,%alloc ,req))
           ret)))
      (($box-type-set!)               ;VERY UNSAFE
       ;; ($box-type-set! box value)
       (match operand*
         [(box value)
          (let* ((rbox (reg)) (rvalue (reg)))
            (emit `(mov ,rvalue ,(cg value 'value env #f)))
            (emit `(mov ,rbox ,(cg box 'value env #f)))
            (emit `(mov (mem64+ ,rbox ,(fx- (tag 'box))) ,rvalue))
            (cg-void ctxt))]))
      (($box-ref)                     ;VERY UNSAFE
       ;; ($box-ref box index)
       (let ((box (reg)) (idx (reg)) (ret (reg)))
         (emit `(mov ,box ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,ret (mem64+ ,box ,idx 8 ,(fx- (tag 'box)))))
         ret))
      (($box-set!)                    ;VERY UNSAFE
       ;; ($box-set! box index value)
       (let ((box (reg)) (idx (reg)) (value (reg)))
         (emit `(mov ,value ,(cg (caddr operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,box ,(cg (car operand*) 'value env #f)))
         (emit `(mov (mem64+ ,box ,idx 8 ,(fx- (tag 'box))) ,value))
         (cg-void ctxt)))
      (($box-header-type)             ;XXX: should be $box-header-type-eq?
       ;; ($box-header-type box)
       (let* ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(and ,ret ,(bitwise-arithmetic-shift-left (mask 'box-header:type)
                                                          (shift 'box-header:type)))
               `(shr ,ret ,(- (shift 'box-header:type) (shift 'fixnum))))
         ret))
      (($box-header-length)
       ;; ($box-header-length box)
       (let* ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(shr ,ret ,(- (shift 'box-header:length) (shift 'fixnum)))
               `(and ,ret ,(fxnot (mask 'fixnum))))
         ret))
      (($box-header-value)
       ;; ($box-header-value box)
       (let* ((ret (reg)))
         (emit `(mov ,ret ,(cg (car operand*) 'value env #f))
               `(and ,ret ,(bitwise-arithmetic-shift-left (mask 'box-header:value)
                                                          (shift 'box-header:value)))
               `(shr ,ret ,(- (shift 'box-header:value) (shift 'fixnum))))
         ret))
      ;; TODO: $box-header-refs?
      (($make-box-header)
       ;; ($make-box-header type refs? value length)
       (match operand*
         [((? const? type) refs? value length)
          (let* ((rrefs? (reg)) (rvalue (reg)) (rlength (reg)) (ret (reg)))
            (emit `(mov ,ret ,(immediate (btag (const-value type))))
                  `(shl ,ret ,(- (shift 'box-header:type) (shift 'fixnum)))
                  `(or ,ret ,(tag 'box-header)))
            (emit `(mov ,rrefs? ,(cg refs? 'value env #f))
                  `(and ,rrefs? ,(fxnot (mask 'boolean)))
                  `(shl ,rrefs? ,(- (shift 'box-header:refs?) (shift 'boolean)))
                  `(or ,ret ,rrefs?))
            (emit `(mov ,rvalue ,(cg value 'value env #f))
                  `(shl ,rvalue ,(- (shift 'box-header:value) (shift 'fixnum)))
                  `(or ,ret ,rvalue))
            (emit `(mov ,rlength ,(cg length 'value env #f))
                  `(shl ,rlength ,(- (shift 'box-header:length) (shift 'fixnum)))
                  `(or ,ret ,rlength))
            ret)]))

      (($make-string)                 ;UNSAFE
       (let ((size (reg)) (req (reg)) (ret (reg)))
         (emit `(mov ,size ,(cg (car operand*) 'value env #f))
               `(mov ,req ,size)
               `(shr ,req ,(fx- (shift 'fixnum) 2)) ;32-bit slots
               `(lea ,req (mem64+ 8 ,req 15))
               `(and ,req -16))
         (cg-allocation req)
         (emit `(mov (mem64+ ,%alloc) ,size)
               `(lea ,ret (mem+ ,%alloc ,(tag 'string)))
               `(add ,%alloc ,req))
         ret))
      ((string-ref)
       (let ((str (reg)) (idx (reg)))
         (emit `(mov ,str ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         (cg-check-fixnum? idx (cadr operand*))
         (with-restart-handler (index proceed)
           (emit `(cmp ,idx (mem64+ ,str ,(fx- (tag 'string))))
                 `(jb ,proceed)))
         (assert (eqv? (shift 'fixnum) 3))
         (emit `(shr ,idx 1))
         (emit `(mov eax (mem32+ ,str ,idx 8 ,(fx- (tag 'string)))))
         ;; (emit `(shr ,idx ,(shift 'fixnum)))
         ;; (emit `(mov eax (mem32+ ,str (* ,idx 4) 8 ,(fx- (tag 'string)))))
         (emit `(or eax ,(tag 'char)))
         'rax))
      ((string-set!)
       (let ((str (reg)) (idx (reg)) (chtmp (reg)) (char64 'rcx) (char32 'ecx) (char8 'cl))
         (emit `(mov ,str ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,chtmp ,(cg (caddr operand*) 'value env #f)))
         (emit `(mov ,idx ,(cg (cadr operand*) 'value env #f)))
         ;; char is given rcx, because there is no way to make an 8
         ;; or 32-bit pseudo register.
         (emit `(mov ,char64 ,chtmp))
         (cg-check-fixnum? idx (cadr operand*))
         (with-restart-handler (index proceed)
           (emit `(cmp ,idx (mem64+ ,str ,(fx- (tag 'string)))) ;type checks str
                 `(jb ,proceed)))
         (unless (inferred-as? (caddr operand*) 'char)
           (with-restart-handler (char? proceed)
             (assert (eqv? (mask 'char) #xff))
             (emit `(cmp ,char8 ,(tag 'char))
                   `(je ,proceed))))
         ;; XXX: the previous checks are important. This write must
         ;; not trap, or idx will be live out here and will not
         ;; contain a valid reference.
         (emit `(sar ,idx 1))
         (emit `(mov (mem32+ ,str ,idx 8 ,(fx- (tag 'string))) ,char32))
         (cg-void ctxt)))

      (($debug-display)
       (emit `(mov rdi ,(cg (car operand*) 'value env #f)))
       (with-new-frame ((env))
         (emit '(call debug-display)))
       (cg-void ctxt))
      (($debug-put-u8)
       (emit `(mov rdi ,(cg (car operand*) 'value env #f))
             `(sar rdi ,(shift 'fixnum)))
       (with-new-frame ((env))
         (emit `(call (mem64+ *debug-put-u8))))
       (cg-void ctxt))
      (($processor-data-set!)         ;UNSAFE
       (let ((idx (reg)) (data (reg)))
         (emit `(mov ,idx ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,data ,(cg (cadr operand*) 'value env #f)))
         (with-restart-handler (index proceed)
           (emit `(cmp ,idx (mem64+ fs ,(* 8 CPU-VECTOR:LENGTH)))
                 `(jb ,proceed)))
         (emit `(mov (mem64+ fs ,idx) ,data))
         (cg-void ctxt)))
      (($processor-data-ref)          ;UNSAFE
       (let ((idx (reg)) (ret (reg)))
         (emit `(mov ,idx ,(cg (car operand*) 'value env #f)))
         (with-restart-handler (index proceed)
           ;; Range check could be done with EFER.LMSLE
           (emit `(cmp ,idx (mem64+ fs ,(* 8 CPU-VECTOR:LENGTH)))
                 `(jb ,proceed)))
         (emit `(mov ,ret (mem64+ fs ,idx)))
         ret))
      (($boot-loader-type)
       ;; TODO: remove
       (emit `(mov rax (mem64+ boot-loader-type)))
       'rax)
      (($boot-loader-data)
       ;; TODO: remove
       (emit `(mov rax (mem64+ boot-loader-data)))
       'rax)

      (($procedure-entry)
       ;; TODO: remove, no longer needed
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(mov rax (mem64+ rax ,(fx- (tag 'procedure))))
             `(sal rax ,(shift 'fixnum)))
       'rax)
      (($procedure-info)
       (emit `(mov rax ,(cg (car operand*) 'value env #f))
             `(mov rax (mem64+ rax 8 ,(fx- (tag 'procedure)))))
       'rax)

      ;; for call/cc
      (($copy-stack)
       ;; This is somewhat weird, because the call from copy-stack
       ;; might be a second return.
       (with-new-frame ((env))
         (emit '(call copy-stack)))
       (emit `(mov ,%closure ,(lookup %closure env)))
       'rax)
      (($restore-stack)               ;arg: top of stack
       (let ((stack (reg))
             (vals (reg)))
         (emit `(mov ,stack ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,vals ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov rdx ,stack)
               `(mov rax ,vals)
               `(jmp restore-stack)))
       'rax)
      (($bootstrap-symbols)
       (emit `(mov rax (+ bootstrap-symbols ,(tag 'pair))))
       'rax)
      (($global-set!)
       (let ((r (reg)) (v (reg)))
         (emit `(mov ,v ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov ,r (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR))))
         (emit `(mov (mem64+ ,r ,(get-global (constant-value (car operand*)))
                             ,(- (tag 'vector)))
                     ,v)))
       (cg-void ctxt))
      (($global-ref)
       (let ((r (reg)) (ret (reg)))
         (emit `(mov ,r (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR))))
         (emit `(mov ,ret (mem64+ ,r ,(get-global (constant-value (car operand*)))
                                  ,(- (tag 'vector)))))
         ret))

      (($switch-stack)
       (assert (null? (cddr operand*)))
       (let ((tmp (reg)))
         (emit `(mov ,tmp ,(cg (car operand*) 'value env #f)))
         (emit `(mov rax ,(cg (cadr operand*) 'value env #f))
               `(mov rdi ,tmp)
               `(sar rdi ,(shift 'fixnum)))
         (let ((t1 (reg)) (t2 (reg)) (t3 (reg)))
           (emit `(mov ,t1 (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
                 `(mov ,t2 ,%alloc)
                 `(mov ,t3 ,%heap-rem))
           (with-new-frame ((env))
             (emit '(call switch-stack))) ;rdi, rax
           (emit `(mov ,%heap-rem ,t3)
                 `(mov ,%alloc ,t2)
                 `(mov (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)) ,t1)))
         (emit `(mov ,%closure ,(lookup %closure env)))
         'rax))
      (($linker-address)
       ;; There can be no closure to represent this, because the
       ;; symbol table for the boot image is not available at
       ;; runtime.
       (assert (null? (cdr operand*)))
       (assert (symbol? (constant-value (car operand*))))
       (emit `(mov rax (<< ,(constant-value (car operand*))
                           ,(shift 'fixnum))))
       'rax)
      (($heap-remaining)
       ;; This is for the time-it procedure.
       (let ((tmp (reg)))
         (emit `(mov ,tmp ,%heap-rem)
               `(sal ,tmp ,(shift 'fixnum)))
         tmp))
      ;; (($current-closure)
      ;;  ;; Obviously there can't be a closure for this, because it
      ;;  ;; would return bogus information.
      ;;  %closure)
      (($stack-pointer)               ;not unsafe, but obnoxious
       ;; For stack traces.
       (let ((tmp (reg)))
         (emit `(lea ,tmp (mem+ rsp LOCALS))
               `(sal ,tmp ,(shift 'fixnum)))
         tmp))
      (($get-mem-object)              ;very unsafe
       ;; Also for stack traces.
       (let ((ret (reg)))
         (emit `(mov rdx ,(cg (car operand*) 'value env #f))
               `(sar rdx ,(shift 'fixnum))
               `(mov ,ret (mem64+ rdx)))
         ret))

      ;; ((apply) (cg-apply operand* ctxt env tail?))

      ;; (loko system unsafe)
      ((syscall) (cg-syscall operand* ctxt env))
      ((get-i/o-u8) (cg-get-i/o 'al operand* ctxt env))
      ((put-i/o-u8) (cg-put-i/o 'al operand* ctxt env))
      ((get-i/o-u8-n!) (cg-get-i/o-n! 'mem8+ operand* ctxt env))
      ((get-i/o-u16) (cg-get-i/o 'ax operand* ctxt env))
      ((put-i/o-u16) (cg-put-i/o 'ax operand* ctxt env))
      ((get-i/o-u16-n!) (cg-get-i/o-n! 'mem16+ operand* ctxt env))
      ((get-i/o-u32) (cg-get-i/o 'eax operand* ctxt env))
      ((put-i/o-u32) (cg-put-i/o 'eax operand* ctxt env))
      ((get-i/o-u32-n!) (cg-get-i/o-n! 'mem32+ operand* ctxt env))
      ;; TODO: put-i/o-uNN-n
      ((get-mem-u8) (cg-get-mem 'mem8+ 'movzx 'eax operand* ctxt env))
      ((get-mem-u16) (cg-get-mem 'mem16+ 'movzx 'eax operand* ctxt env))
      ((get-mem-u32) (cg-get-mem 'mem32+ 'mov 'eax operand* ctxt env))
      ((get-mem-s61) (cg-get-mem 'mem64+ 'mov 'rax operand* ctxt env))
      ((put-mem-u8) (cg-put-mem 'mem8+ 'al operand* ctxt env))
      ((put-mem-u16) (cg-put-mem 'mem16+ 'ax operand* ctxt env))
      ((put-mem-u32) (cg-put-mem 'mem32+ 'eax operand* ctxt env))
      ((put-mem-s61) (cg-put-mem 'mem64+ 'rax operand* ctxt env))

      ;; loko system $amd64. These are ALL unsafe.
      (($disable-interrupts)
       ;; TODO: it would be nice to be able to actually use CLI/STI, since
       ;; they are permitted at CPL=3 (but not under Linux).
       ;; (emit '(cli))
       (with-new-frame ((env))
         (emit '(call $tmp-cli)))
       (cg-void ctxt))
      (($enable-interrupts)
       ;; (emit '(sti))
       (with-new-frame ((env))
         (emit '(call $tmp-sti)))
       (cg-void ctxt))
      (($cpuid!)
       ;; ($cpuid! eax ecx mutable-vector)
       (let ((op1 (reg)) (op2 (reg)) (op3 (reg)))
         (emit `(mov ,op1 ,(cg (car operand*) 'value env #f)))
         (emit `(mov ,op2 ,(cg (cadr operand*) 'value env #f)))
         (emit `(mov rdi ,(cg (caddr operand*) 'value env #f))
               `(mov rax ,op1)
               `(sar rax ,(shift 'fixnum))
               `(mov rcx ,op2)
               `(sar rcx ,(shift 'fixnum))
               `(cpuid)
               `(sal rax ,(shift 'fixnum))
               `(sal rbx ,(shift 'fixnum))
               `(sal rcx ,(shift 'fixnum))
               `(sal rdx ,(shift 'fixnum))
               `(mov (mem64+ rdi 8 ,(fx- (tag 'vector))) rax)
               `(mov (mem64+ rdi 8 8 ,(fx- (tag 'vector))) rbx)
               `(mov (mem64+ rdi 8 8 8 ,(fx- (tag 'vector))) rcx)
               `(mov (mem64+ rdi 8 8 8 8 ,(fx- (tag 'vector))) rdx)))
       (cg-void ctxt))
      ((rdtsc)
       ;; TODO: double-check that this is true: Equivalent to (mod
       ;; (full-rdtsc) (greatest-fixnum)). TODO: see what happens at
       ;; wraparound by adding some large base to this value. It
       ;; wraps around after ((2^w)/(f*10^9)) seconds.
       (let ((ret (reg)))
         (let ((barrier
                (and (= (length operand*) 1)
                     (constant? (car operand*))
                     (constant-value (car operand*)))))
           (case barrier
             ((start)
              ;; CPUID stops instructions before RDTSC from being
              ;; counted towards the time. The AND is to keep the
              ;; fixnum positive.
              (emit '(xor eax eax)
                    '(xor ecx ecx)
                    '(cpuid)
                    '(rdtsc)))        ;edx:eax <- tsc
             ((stop)
              ;; With RDTSCP instructions before it are all included
              ;; in the time.
              (emit '(rdtscp)))       ;edx:eax <- tsc, ecx <- cpu
             (else
              (emit '(rdtsc))))
           ;; Dependency on edx:eax here stops these
           ;; instructions from running before rdtscp/rdtsc.
           (emit `(and edx ,(- (expt 2 (- 32 (shift 'fixnum) 1))
                               1))
                 `(shl rdx ,(+ 32 (shift 'fixnum)))
                 `(lea ,ret (mem+ rdx (* rax 8))))
           (when (eq? barrier 'stop)
             ;; Stop instructions after CPUID from running before
             ;; RDTSCP. Input to CPUID will be EAX and ECX coming
             ;; from RDTSCP.
             (emit '(cpuid))))
         ret))
      (($valgrind)
       ;; ($valgrind req-location)
       (assert (= (length operand*) 1))
       (cg-funcall '$valgrind (make-const #f #f) operand* ctxt env tail?))

      ;; Floating point
      ((fixnum->flonum)
       (match operand*
         [(fx)
          (let ((op0 (reg)) (ret (reg)))
            (emit `(mov ,op0 ,(cg fx 'value env #f)))
            (cg-check-fixnum? op0 fx)
            ;; TODO: check the rounding mode
            (emit `(sar ,op0 ,(shift 'fixnum))
                  `(cvtsi2ss xmm1 ,op0)
                  `(movq ,ret xmm1)   ;TODO: should be movd
                  `(shl ,ret ,(shift 'flonum))
                  `(or ,ret ,(tag 'flonum)))
            ret)]
         [_ (cg-primcall-proc name operand* ctxt env tail?)]))
      ((fl=? fl>?)
       (match operand*
         [(fl0 fl1)
          (let ((op0 (reg)) (op1 (reg)))
            (emit `(mov ,op0 ,(cg fl0 'value env #f)))
            (emit `(mov ,op1 ,(cg fl1 'value env #f)))
            (cg-check-flonum? op0 fl0 op1 fl1)
            (emit `(shr ,op0 ,(shift 'flonum))
                  `(shr ,op1 ,(shift 'flonum))
                  `(movq xmm1 ,op0)   ;TODO: should be movd
                  `(movq xmm2 ,op1)
                  `(ucomiss xmm1 xmm2))
            (case name
              ((fl=?)
               ;; TODO: would be nice to use this in cg-test. CMPSS
               ;; is somehow better, but doesn't use flags.
               (emit `(mov eax ,(immediate #t))
                     `(mov edx ,(immediate #f))
                     `(setnp ah)         ;eax=#f if NaN
                     `(cmovne eax edx))  ;eax=#f is not equal
               'eax)
              ((fl>?)
               (cg-test ctxt 'rflags.nbe))))]
         [_ (cg-primcall-proc name operand* ctxt env tail?)]))
      ((fl+ fl- fl* fl/ flmin flmax)
       (match operand*
         [(fl0 fl1)
          (let ((op0 (reg)) (op1 (reg)) (ret (reg)))
            (emit `(mov ,op0 ,(cg fl0 'value env #f)))
            (emit `(mov ,op1 ,(cg fl1 'value env #f)))
            (cg-check-flonum? op0 fl0 op1 fl1)
            (emit `(shr ,op0 ,(shift 'flonum))
                  `(shr ,op1 ,(shift 'flonum))
                  `(movq xmm1 ,op0)   ;TODO: should be movd
                  `(movq xmm2 ,op1))
            (case name
              ((fl+) (emit `(addss xmm1 xmm2)))
              ((fl-) (emit `(subss xmm1 xmm2)))
              ((fl*) (emit `(mulss xmm1 xmm2)))
              ((fl/) (emit `(divss xmm1 xmm2)))
              ((flmin) (emit `(minss xmm1 xmm2)))
              ((flmax) (emit `(maxss xmm1 xmm2))))
            (emit `(movq ,ret xmm1)   ;TODO: should be movd
                  `(shl ,ret ,(shift 'flonum))
                  `(or ,ret ,(tag 'flonum)))
            ret)]
         [_ (cg-primcall-proc name operand* ctxt env tail?)]))
      ((flsqrt flfloor flceiling fltruncate flround)
       (match operand*
         [(fl0)
          (let ((op0 (reg)) (ret (reg)))
            (emit `(mov ,op0 ,(cg fl0 'value env #f)))
            (cg-check-flonum? op0 fl0)
            (emit `(shr ,op0 ,(shift 'flonum))
                  `(movq xmm1 ,op0))  ;TODO: should be movd
            (case name
              ((flsqrt) (emit `(sqrtss xmm1 xmm1)))
              ;; XXX: These require SSE4.2.
              ((flfloor)    (emit `(roundss xmm1 xmm1 #b0001)))
              ((flceiling)  (emit `(roundss xmm1 xmm1 #b0010)))
              ((fltruncate) (emit `(roundss xmm1 xmm1 #b0011)))
              ((flround)    (emit `(roundss xmm1 xmm1 #b0000))))
            (emit `(movq ,ret xmm1)   ;TODO: should be movd
                  `(shl ,ret ,(shift 'flonum))
                  `(or ,ret ,(tag 'flonum)))
            ret)]
         [_ (cg-primcall-proc name operand* ctxt env tail?)]))
      ((flabs)
       (match operand*
         [(fl0)
          (let ((op0 (reg)))
            (emit `(mov ,op0 ,(cg fl0 'value env #f)))
            (cg-check-flonum? op0 fl0)
            ;; Clear the sign bit. BZHI can also do it.
            (emit `(shl ,op0 1)
                  `(shr ,op0 1))
            op0)]
         [_ (cg-primcall-proc name operand* ctxt env tail?)]))

      (else
       (cg-primcall-proc name operand* ctxt env tail?))))

  (define (lookup var env)
    (let ((loc (assq var env)))
      (when (not loc)
        (error 'lookup "Variable not bound" (variable-name var)))
      (let ((loc (cdr loc)))
        (cond ((pair? loc)
               (cond ((eq? (car loc) 'reg)
                      loc)
                     (else
                      (assert (eq? (car loc) 'free))
                      `(mem64+ ,%closure ,(- (cdr loc) (tag 'procedure))))))
              (else
               `(mem64+ rsp ,loc LOCALS))))))

  (define (extend-env var reg env)
    (emit `(%comment frame
                     ,(if (variable? var) (variable-name var) var)
                     ,reg))
    (cons (cons var reg) env))

  (define (extend-formals lhs* formals-proper?)
    ;; The first arguments go into registers.
    (do ((lhs* lhs* (cdr lhs*))
         (reg* %arg-reg* (cdr reg*))
         (env '() (let ((r (reg)))
                    (emit `(%comment frame ,(variable-name (car lhs*)) ,r))
                    (cons (cons (car lhs*) r) env))))
        ((or (null? lhs*) (null? reg*))
         ;; Now arguments go onto the stack. The first argument has
         ;; the highest address and optional arguments are at the
         ;; lowest addresses, because we need to statically allocate
         ;; space in the frame to hold all arguments.
         (do ((si -8 (fx- si 8))
              (lhs* lhs* (cdr lhs*))
              (env env
                   (cond ((and (not formals-proper?) (null? (cdr lhs*)))
                          ;; The restargs are always in a register
                          ;; (handled by cg-consargs).
                          (let ((r (reg)))
                            (emit `(%comment frame ,(variable-name (car lhs*)) ,r))
                            (cons (cons (car lhs*) r) env)))
                         (else
                          ;; Otherwise the argument is at the given
                          ;; stack index.
                          (emit `(%comment frame ,(variable-name (car lhs*)) ,si))
                          (cons (cons (car lhs*) si) env)))))
             ((null? lhs*)
              env)))))

  (define (extend-free free* env)
    ;; Free variables start at offset 16 in the closure.
    (let lp ((i 16) (free* free*) (env env))
      (if (null? free*) env
          (lp (fx+ i 8) (cdr free*)
              (begin
                (emit `(%comment closure ,(variable-name (car free*)) ,i))
                (cons (cons (car free*) (cons 'free i)) env))))))

  (define (cg-allocate-frame number-of-formals)
    (emit `(%comment allocate-frame ,number-of-formals)
          `(%comment frame-size)))

  (define (cg-restore-callee-save env)
    (emit `(mov rbx ,(lookup 'rbx env))
          `(mov rbp ,(lookup 'rbp env))))

  (define (cg-deallocate-frame)
    (emit `(%comment deallocate-frame)))

  (define (cg-consargs formals env)
    ;; Conses rest arguments.
    (emit `(mov r10 ,(immediate (fx- (length formals) 1))) ;fixed
          ;; Make room on the stack for all arguments (including
          ;; those actually passed in registers).
          '(cdqe)
          '(add rsp rax)
          ;; Cons the rest args.
          '(call consargs)
          '(sub rsp rax))
    'r12)

  (define (cg-apply operator operand* ctxt env tail?)
    (define (unconsargs rest-loc reg* locals-offset li fixed-args)
      ;; expands rest-args. It takes a list of objects and places
      ;; them on the stack. TODO: catch circular lists? should be
      ;; cheap enough and maybe an error message is nicer than a
      ;; stack overflow.
      (define :loop (vector 'unconsargs 1))
      (define :done (vector 'unconsargs 2))
      (define tmp #;'r10 (reg))
      (define rest #;'r11 #;rest-loc (reg))
      (define regargcount (length %arg-reg*))
      (define fixsize (fxmax 0 (fx* 8 (fx- fixed-args regargcount))))
      ;; rest-loc is the rest list. rax is outgoing argcount.
      (for-each (lambda (reg) (emit `(mov ,reg ,(immediate 0)))) reg*)
      (emit `(mov ,rest ,rest-loc))
      ;; Setup the initial argument count
      (emit `(mov eax ,(immediate (fx- fixed-args)))
            '(cdqe))
      ;; Uncons into registers
      (do ((reg* reg* (cdr reg*)))
          ((null? reg*))
        (emit `(cmp ,rest ,(immediate '()))
              `(je ,:done)
              `(mov ,(car reg*) (mem64+ ,rest ,(fx- (tag 'pair)))) ;car
              `(mov ,rest (mem64+ ,rest 8 ,(fx- (tag 'pair)))) ;cdr
              `(sub rax 8)))
      (emit `(%label ,:loop)
            `(cmp ,rest ,(immediate '()))
            `(je ,:done)
            `(mov ,tmp (mem64+ ,rest ,(fx- (tag 'pair))))    ;car
            `(mov ,rest (mem64+ ,rest 8 ,(fx- (tag 'pair)))) ;cdr
            `(mov (mem64+ rsp rax ,(* 8 regargcount) ,(+ li fixsize) ,locals-offset) ,tmp)
            `(sub rax 8)
            `(jmp ,:loop)
            `(%label ,:done)))
    ;; The operands are: arg0 ... rest-args. This evaluates all
    ;; operands then unconses the rest-args list.
    (let ((fixed-args (fx- (length operand*) 1)))
      (let lp ((operand* operand*) (loc* '()))
        (cond
          ((pair? operand*)
           ;; Evalate the arguments
           (let ((loc (reg)))
             (emit `(mov ,loc ,(cg (car operand*) 'value env #f)))
             (lp (cdr operand*) (cons loc loc*))))
          (else
           (let ((locals-offset (if tail? 'LOCALS 0))
                 (stack-offset (if tail? 0 -8))  ;room for the return address
                 (arg-loc* (reverse (cdr loc*)))
                 (rest-loc (car loc*))
                 (closure-loc (reg)))
             (emit `(mov ,closure-loc ,(cg operator 'value env #f)))
             (when tail? (cg-restore-callee-save env))
             ;; Move the arguments into place
             (do ((reg* %arg-reg* (cdr reg*))
                  (loc* arg-loc* (cdr loc*)))
                 ((or (null? reg*) (null? loc*))
                  (do ((i (fx+ stack-offset -8) (fx- i 8))
                       (loc* loc* (cdr loc*)))
                      ((null? loc*)
                       ;; Uncons the last argument to apply. This
                       ;; will overwrite local variables.
                       (unconsargs rest-loc reg* locals-offset i fixed-args))
                    (emit `(mov (mem64+ rsp ,i ,locals-offset) ,(car loc*)))))
               (emit `(mov ,(car reg*) ,(car loc*))))
             ;; Do the call
             (emit `(mov ,%closure ,closure-loc))
             (cond
               ((not tail?)             ;apply call
                ;; Move the arguments into place (making room
                ;; for the return address written by call).
                (emit `(%comment call apply ,(length %arg-reg*)))
                (with-new-frame ((env))
                  (emit `(call (mem64+ ,%closure ,(fx- (tag 'procedure))))))
                (emit `(mov ,%closure ,(lookup %closure env)))
                'rax)
               (else                  ; apply tail call.
                ;; Move the arguments into place (making room for
                ;; the return address written by call). Overwrites
                ;; the incoming arguments.

                ;; FIXME: must one extra slot be reserved?
                (emit `(%comment reserve-frame-space
                                 ,(fxmax 0 (fx- fixed-args (length %arg-reg*)))))
                (emit '(%comment deallocate-frame))
                (emit `(%comment call apply-tail ,(length %arg-reg*)))
                (emit `(jmp (mem64+ ,%closure ,(fx- (tag 'procedure)))))
                (cg-void ctxt)))))))))

  (define (cg-funcall label operator operand* ctxt env tail?)
    (define operand-length (length operand*))
    (let lp ((operand* operand*) (loc* '()))
      (cond
        ;; Store operands in pseudo registers (loc*).
        ((pair? operand*)
         (let ((loc (reg)))
           (emit `(mov ,loc ,(cg (car operand*) 'value env #f)))
           (lp (cdr operand*) (cons loc loc*))))
        ;; Done with the operands.
        ((not tail?)                  ;not a tail call
         (emit `(mov ,%closure ,(cg operator 'value env #f)))
         ;; Move the arguments into place (making room for the
         ;; return address written by call).
         (do ((reg* %arg-reg* (cdr reg*))
              (loc* (reverse loc*) (cdr loc*)))
             ((or (null? reg*) (null? loc*))
              (do ((i -16 (fx- i 8))
                   (loc* loc* (cdr loc*)))
                  ((null? loc*))
                (emit `(mov (mem64+ rsp ,i) ,(car loc*)))))
           (emit `(mov ,(car reg*) ,(car loc*))))
         ;; Pass the number of arguments.
         (emit `(mov eax ,(immediate (fx- operand-length))))
         (emit `(%comment call normal ,operand-length))
         (if label
             (with-new-frame ((env))
               (emit `(call ,label)))
             (with-new-frame ((env))
               (emit `(call (mem64+ ,%closure ,(fx- (tag 'procedure)))))))
         (emit `(mov ,%closure ,(lookup %closure env)))
         'rax)
        (else                         ;tail call
         ;; TODO: evaluating the operator might create a closure.
         ;; that shouldn't be necessary if this is a self-call. just
         ;; reload %closure?
         (emit `(mov ,%closure ,(cg operator 'value env #f)))
         (cg-restore-callee-save env)
         ;; The arguments have been stored in pseudo registers
         ;; (which might be spilled, but not into the reserved part
         ;; of the frame). The part of the frame storing the
         ;; incoming arguments is no longer needed.
         (do ((reg* %arg-reg* (cdr reg*))
              (loc* (reverse loc*) (cdr loc*)))
             ((or (null? reg*) (null? loc*))
              (do ((i -8 (fx- i 8))
                   (loc* loc* (cdr loc*)))
                  ((null? loc*))
                (emit `(mov (mem64+ rsp ,i LOCALS) ,(car loc*)))))
           (emit `(mov ,(car reg*) ,(car loc*))))
         ;; Pass the number of arguments.
         (emit `(mov eax ,(immediate (fx- operand-length))))
         ;; This optimizer directive ensures that this
         ;; many locations are available for outgoing
         ;; arguments.
         (emit `(%comment reserve-frame-space
                          ,(max 0 (- operand-length (length %arg-reg*)))))
         (emit '(%comment deallocate-frame))
         (emit `(%comment call tail ,operand-length))
         (if label
             (emit `(jmp ,label))
             (emit `(jmp (mem64+ ,%closure ,(fx- (tag 'procedure))))))
         (cg-void ctxt)))))

  (define (cg-labels x label env tail?)
    (unless (labels? x)
      (error 'cg-labels "Not a label" x env tail?))
    ;; First compile the body of the labels.
    (let ((end-label (vector 'end)))
      (emit '(%comment procedure)
            '(%align 8)
            `(%label ,label ,end-label))
      (reset-pseudo-register-counter!)
      (emit '(%comment allocate-frame 0)
            '(%comment frame-size))
      (with-loc ((env save-closure) (env %closure))
        (emit `(mov ,save-closure ,%closure))
        (with-loc ((env save-rbx) (env 'rbx))
          (emit `(mov ,save-rbx rbx))
          (with-loc ((env save-rbp) (env 'rbp))
            (emit `(mov ,save-rbp rbp))
            (emit `(mov rax ,(cg (labels-body x) 'value env tail?))
                  `(mov ,%closure ,save-closure))
            (cg-restore-callee-save env)
            (emit '(%comment deallocate-frame))
            (emit '(ret)
                  `(%label ,end-label)
                  '(%comment procedure-end))))))
    ;; Compile the procedures in the labels.
    (do ((proc* (labels-proc* x) (cdr proc*)))
        ((null? proc*))
      (let* ((x (car proc*))
             (end-label (proc-end-label x)))
        ;; The general entry label of a case-lambda can be used
        ;; as the label field in procedure objects. Entry points
        ;; are therefore aligned so that they may be treated as
        ;; fixnums.
        (reset-pseudo-register-counter!)
        (emit '(%comment procedure)
              `(%align 8)
              `(%label ,(proc-label x) ,end-label))
        (cond ((const-value (proc-source x))
               => (lambda (s) (emit `(%comment source ,s)))))
        (do ((cases (proc-cases x) (cdr cases)))
            ((null? cases))
          (let* ((c (car cases))
                 (info (proccase-info c))
                 (formals (caseinfo-formals info)))
            (cond ((caseinfo-proper? info)
                   (if (null? formals)
                       (emit `(test eax eax))
                       (emit `(cmp eax ,(immediate (fx- (length formals))))))
                   (emit `(je ,(caseinfo-label info))))
                  ((null? (cdr formals))
                   (emit `(jmp ,(caseinfo-label info))))
                  (else
                   (emit `(cmp eax ,(immediate (fx- (fx- (length formals) 1)))))
                   (emit `(jle ,(caseinfo-label info)))))))
        ;; XXX: This can be a jump because r15 already contains
        ;; information about the callee.
        (emit `(jmp formals-mismatch))
        ;; Emit the bodies. By emitting these in reverse order it
        ;; becomes possible to always remove the unconditional
        ;; jump above by doing branch reversal.
        (do ((cases (reverse (proc-cases x)) (cdr cases)))
            ((null? cases))
          (let* ((c (car cases))
                 (info (proccase-info c))
                 (formals (caseinfo-formals info))
                 (label (caseinfo-label info)))
            (let ((env (extend-free (proc-free x)
                                    (extend-formals formals (caseinfo-proper? info))))
                  (arg-space (fxmax 0 (fx- (length formals) (length %arg-reg*)))))
              (emit `(%label ,(caseinfo-label info)))
              (cond ((and (not (caseinfo-proper? info))
                          ;; FIXME: should be residual-referenced?,
                          ;; at least if cp0 was used
                          (variable-referenced? (last formals)))
                     (let ((list (cg-consargs formals env)))
                       (cg-allocate-frame (fxmax 0 (fx- arg-space 1)))
                       (emit `(mov ,(lookup (last formals) env) ,list))))
                    (else
                     ;; We want space in the frame for holding the
                     ;; arguments that were passed on the stack.
                     ;; This is part of the space made available
                     ;; after consargs has run.
                     (cg-allocate-frame arg-space)))
              ;; Move the argument registers into pseudo registers.
              (do ((reg* %arg-reg* (cdr reg*))
                   (formals formals (cdr formals)))
                  ((or (null? reg*) (null? formals)
                       (and (not (caseinfo-proper? info))
                            (null? (cdr formals)))))
                (emit `(mov ,(lookup (car formals) env) ,(car reg*))))
              ;; Pseudo registers for the callee-save registers.
              (with-loc ((env save-closure) (env %closure))
                (emit `(mov ,save-closure ,%closure))
                (with-loc ((env save-rbx) (env 'rbx))
                  (emit `(mov ,save-rbx rbx))
                  (with-loc ((env save-rbp) (env 'rbp))
                    (emit `(mov ,save-rbp rbp))
                    ;; Evaluate the body and return.
                    (emit `(mov rax ,(cg (proccase-body c) 'value env 'tail)))
                    (cg-restore-callee-save env)
                    (cg-deallocate-frame)
                    (emit '(ret))))))))
        (emit `(%label ,end-label)
              '(%comment procedure-end))))
    ;; For the return value of the labels body.
    'rax)

  (define (closure-size c)
    ;; Combinators do not need to be allocated at runtime, so they
    ;; take no memory.
    (if (combinator? c)
        0
        (fxand (fx+ (fx* 8 (length (closure-free* c)))
                    (fx+ 16 15))
               -16)))

  (define (cg-closure x ctxt env)
    ;; Closures: label, info, free*.
    ;; Info (box): 'info, length, # free vars, name, source.
    (when debug
      (display (if (combinator? x) "A COMBINATOR: " "A CLOSURE: "))
      (write (record->sexpr x)) (newline)
      (newline))
    (let* ((c (closure-code x))
           (info (vector 'info (proc-label c)))
           (name (encode-const (proc-name c)))
           (source (encode-const (proc-source c))))
      ;; TODO: remove "false" free variables from closures, e.g.
      ;; references to procedures with known locations (or is it
      ;; references to combinators?).
      (emit-data '(%align 16 0))
      (emit-data `(%label ,info))
      (emit-data `(%u64 ,(immediate 'info)
                        ,(immediate 6)
                        ,(immediate (length (closure-free* x)))
                        ,name ,source
                        (<< ,(proc-label (closure-code x)) ,(shift 'fixnum))
                        (<< ,(proc-end-label (closure-code x)) ,(shift 'fixnum))))
      (cond ((combinator? x)
             ;; There are no free variables, so all the necessary
             ;; information is available at compile time.
             ;; TODO: (eq? (closure foo) (closure foo)) => #t
             (let ((ret (vector 'closure (const-value (proc-name c)))))
               (emit-data '(%align 16 0))
               (emit-data `(%label ,ret))
               (emit-data `(%u64 ,(proc-label (closure-code x))
                                 (+ ,info ,(tag 'box))))
               `(+ ,ret ,(tag 'procedure))))
            (else
             (unless (eq? ctxt 'fix)
               ;; The overflow check was already done by fix.
               (cg-allocation (closure-size x)))
             (emit `(mov (mem64+ ,%alloc) ,(proc-label (closure-code x))))
             (emit `(mov (mem64+ ,%alloc 8) (+ ,info ,(tag 'box))))
             ;; Store the free variables.
             (do ((free* (closure-free* x) (cdr free*))
                  (offset (* 2 8) (fx+ offset 8)))
                 ((null? free*))
               (let ((t (reg)))
                 (emit `(mov ,t ,(lookup (car free*) env)))
                 (emit `(mov (mem64+ ,%alloc ,offset) ,t))))
             ;; Construct the closure pointer.
             (let ((t (reg)))
               (emit `(lea ,t (mem+ ,%alloc ,(tag 'procedure))))
               (emit `(add ,%alloc ,(closure-size x)))
               t)))))

  (define (cg-fix x ctxt env tail?)
    ;; rhs* is always closures (or combinators). tmp points to the
    ;; first closure (which gets filled in later by cg-closure).
    (cg-allocation (fold-left + 0 (map closure-size
                                       (fix-rhs* x))))
    (let ((tmp (reg)))
      (emit `(lea ,tmp (mem+ ,%alloc ,(tag 'procedure))))
      (let lp ((lhs* (fix-lhs* x)) (rhs* (fix-rhs* x)) (env env))
        (cond ((pair? lhs*)
               (with-loc ((env loc) (env (car lhs*)))
                 (cond ((combinator? (car rhs*))
                        ;; This one gets stored in .data.
                        (emit `(mov ,loc ,(cg-closure (car rhs*) ctxt env))))
                       (else
                        ;; Construct an empty closure.
                        (let ((size (closure-size (car rhs*))))
                          (emit `(mov ,loc ,tmp)
                                `(add ,tmp ,size)))))
                 (lp (cdr lhs*) (cdr rhs*) env)))
              (else
               ;; Emit the right-hand sides. Cleverly arranged so
               ;; that cg-closure will use the same addresses as
               ;; were calculated above.
               (do ((rhs* (fix-rhs* x) (cdr rhs*)))
                   ((null? rhs*))
                 (assert (closure? (car rhs*)))
                 (unless (combinator? (car rhs*))
                   (cg-closure (car rhs*) 'fix env)))
               ;; And finally emit the body.
               (cg (fix-body x) 'value env tail?))))))

  (define (cg x ctxt env tail?)
    (when debug
      (display #\:)
      (write (record->sexpr x)) (newline))
    (cond
      ((ref? x)
       (lookup (ref-name x) env))
      ((fix? x)
       (cg-fix x ctxt env tail?))
      ((closure? x)
       (cg-closure x ctxt env))
      ((bind? x)
       (let ((old-env env))
         (let lp ((lhs* (bind-lhs* x)) (rhs* (bind-rhs* x)) (env env))
           (cond ((pair? lhs*)
                  (with-loc ((env loc) (env (car lhs*)))
                    (emit `(mov ,loc ,(cg (car rhs*) 'value old-env #f)))
                    (lp (cdr lhs*) (cdr rhs*) env)))
                 (else
                  (cg (bind-body x) 'value env tail?))))))
      ((infer? x)
       (cg (infer-expr x) ctxt env tail?))
      ((const? x)
       ;; TODO: The constants should be allocated in read-only
       ;; memory. Furthermore, when the compiler is online, the
       ;; constants must be deallocated when the corresponding codes
       ;; are deallocated.
       (cond ((encode-const x) =>
              (lambda (v)
                (let ((t (reg)))
                  (emit `(mov ,t ,v))
                  t)))
             (else
              (error who "Internal error: constant not encoded" (const-value x)))))
      ((seq? x)
       (cg (seq-e0 x) 'effect env #f)
       (cg (seq-e1 x) ctxt env tail?))
      ((test? x)
       (let ((result (cg (test-expr x) 'test env #f)))
         (let* ((alternative (label 'if-alt))
                (exit (label 'if-exit)))
           (let ((Jcc (or (rflags->Jcc result)
                          (begin
                            ;; TODO: this would be more robust if it
                            ;; didn't assume that result can't be an
                            ;; rflags that just wasn't in the table
                            (emit `(cmp ,result ,(immediate #f)))
                            'je))))
             (emit `(,Jcc ,alternative)))
           (let ((t (reg)))
             (cg-branch-instrumentation)
             (emit `(mov ,t ,(cg (test-then x) 'value env tail?))
                   `(jmp ,exit))
             (emit `(%label ,alternative))
             (cg-branch-instrumentation)
             (emit `(mov ,t ,(cg (test-else x) 'value env tail?)))
             (emit `(%label ,exit))
             t))))

      ((funcall? x)
       (let* ((operator (funcall-operator x))
              (operand* (funcall-operand* x)))
         (cond
           ((primref? operator)
            (let ((name (primref-name operator)))
              (cond
                ((and (eq? name 'apply) (pair? operand*) #;(pair? (cdr operand*))
                      )
                 ;; TODO: known-label calls
                 (cg-apply (car operand*) (cdr operand*) ctxt env tail?))
                (else
                 (cg-primcall (primref-name operator) operand* ctxt env tail?)))))
           (else
            ;; TODO: get the label of the correct case-lambda body
            (cg-funcall (funcall-label x) operator operand* ctxt env tail?)))))

      ((primref? x)
       ;; TODO: do this in a separate pass!
       (let ((name (primref-name x)))
         (cond ((primlocs name) =>
                (lambda (location)
                  (cg (make-funcall (make-primref '$global-ref)
                                    (list (make-const location #f))
                                    #f #f)
                      ctxt env tail?)))
               (else
                (error who "TODO: more primitives..." name)))))

      ;; These three are introduced by pass-loops:
      ((mutate? x)
       ;; This is used by pass-loops and can never update closure
       ;; variables.
       (let ((t (reg)))
         (emit `(mov ,t ,(cg (mutate-expr x) 'value env #f)))
         (let ((loc (lookup (mutate-name x) env)))
           (assert (not (memq %closure loc)))
           (emit `(mov ,loc ,t)))))
      ((goto? x)
       (cg-source (goto-source x))
       (emit `(jmp ,(goto-label x)))
       (cg-void ctxt))
      ((tagbody? x)
       ;; TODO: what is the return value of a tagbody anyway?
       (cg-source (tagbody-source x))
       (emit `(%label ,(tagbody-label x)))
       (cg (tagbody-body x) 'value env tail?))

      (else
       (error who "Internal error: unknown code type" x))))

  (let ((labels
         (map (lambda (code)
                (string->symbol
                 (call-with-string-output-port
                   (lambda (p)
                     (display "lib" p)
                     (for-each (lambda (sym)
                                 (put-char p #\_)
                                 (display sym p))
                               (labels-top-level-name code))))))
              codes)))
    ;; First emit a procedure that calls each of the codes.
    (let ((end (vector 'end-start)))
      (emit '(%comment procedure)
            '(%align 8)
            `(%label scheme-start ,end))
      (let lp ((labels labels))
        (cond ((null? labels))
              ((null? (cdr labels))
               (emit `(mov ,%closure ,(immediate #f)))
               (emit `(jmp ,(car labels)))
               (emit `(%label ,end)
                     '(%comment procedure-end)))
              (else
               (emit `(mov ,%closure ,(immediate #f)))
               (with-new-frame ((env))
                 (emit `(call ,(car labels))))
               (lp (cdr labels))))))
    ;; Now emit the codes.
    (for-each (lambda (label code)
                (assert (labels? code))
                (cg-labels code label '() #t))
              labels codes))

  ;; Makes the global environment look like a vector.
  (when make-init-code?
    (let ((end-label (vector 'end-init)))
      (emit '(%comment procedure)
            '(%align 8)
            `(%label init-global-environment ,end-label))
      (for-each emit (init-globals!))
      (emit '(xor eax eax)
            '(ret)
            `(%label ,end-label)
            '(%comment procedure-end))))

  ;; Make a minimal perfect hashtable of the bootstrap symbol
  ;; interning table.
  (when make-init-code?
    (emit-data '(%align 16 0))
    (emit-data `(%label bootstrap-symbols))
    ;; k* is a vector of host symbols and v* is a vector of assembler
    ;; labels for those symbols. Things like (+ #(const ...) 1).
    (let ((bv->sym (make-hashtable bytevector-hash equal?)))
      (let-values (((k* v*) (hashtable-entries symbols)))
        (vector-for-each
         (lambda (k v)
           (hashtable-set! bv->sym (string->utf8 (symbol->string k)) v))
         k* v*)
        ;; The perfect hashtable can take any bytevector and find the
        ;; symbol that matches it, if there is such a symbol. You
        ;; always gets some symbol, but you must verify that it's the
        ;; right one.
        (let-values (((G K V) (hashtable->minimal-perfect-hashtable bv->sym)))
          ;; TODO: G could be a vector of integers that can fit the
          ;; length of the vector * 2, maybe an s16.
          (let ((G-label (vector 'G)) (V-label (vector 'V)))
            (emit-data `(%u64 (+ ,G-label ,(tag 'vector))
                              (+ ,V-label ,(tag 'vector))))
            (emit-data '(%align 16 0))
            (emit-data `(%label ,G-label))
            (emit-data `(%u64 ,(immediate (vector-length G))
                              ,@(vector->list (vector-map immediate G))))
            (emit-data '(%align 16 0))
            (emit-data `(%label ,V-label))
            (emit-data `(%u64 ,(immediate (vector-length V))
                              ,@(vector->list V))))))))

  (values (reverse code)
          (reverse data))))
