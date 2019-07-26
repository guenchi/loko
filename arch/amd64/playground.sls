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

;;; Playground for the standard library.

;; This is stuff that should be improved and moved elsewhere.

(library (loko arch amd64 playground)
  (export
    syscall
    time-it time-it*
    garbage-collection-count
    disassemble
    stack-trace
    valgrind)
  (import
    (except (rnrs))
    (only (loko system $asm-amd64) $syscall rdtsc
          ;; Stack traces
          $get-mem-u8 $get-mem-u32)
    (only (loko system $procedures) $procedure-info)
    (only (loko system $boxes) $box-ref)
    (only (loko system $bytevectors) $bytevector-location)
    (loko match)
    (prefix (rnrs) sys:)
    (only (loko system $host)
          $processor-data-ref
          $heap-remaining
          current-processor-time
          $valgrind
          ;; Stack traces
          $stack-pointer
          $get-mem-object)
    ;; Disassemble
    (only (loko system $disassembler)
          disassemble1)
    (loko system $procedures)
    (only (loko libs context)
          CPU-VECTOR:PROCESS-VECTOR
          PROCESS-VECTOR:STACK-TOP
          PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT))

(define (garbage-collection-count)
  (vector-ref ($processor-data-ref CPU-VECTOR:PROCESS-VECTOR)
              PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT))

(define (stack-top)
  (fx* (vector-ref ($processor-data-ref CPU-VECTOR:PROCESS-VECTOR)
                   PROCESS-VECTOR:STACK-TOP)
       8))

;; Btw: doing syscalls directly from the REPL stands a good chance
;; of not working every now and then. To use $bytevector-location it
;; is necessary that there's no chance of a GC run between that and
;; the syscall. This will work better when the compiler is up and
;; running.
(define syscall
  (case-lambda
    ((n)
     ($syscall n))
    ((n a)
     ($syscall n a))
    ((n a b)
     ($syscall n a b))
    ((n a b c)
     ($syscall n a b c))
    ((n a b c d)
     ($syscall n a b c d))
    ((n a b c d e)
     ($syscall n a b c d e))
    ((n a b c d e f)
     ($syscall n a b c d e f))))

(define (time-it what thunk)
  (define (print . x) (for-each display x) (newline))
  ;; Number of garbage collections, elapsed CPU and real time, time
  ;; spent in the garbage collector, bytes allocated.
  (let* ((t0 (current-processor-time))
         (t1 (current-processor-time))
         (t2 (current-processor-time))
         (t3 (current-processor-time))
         (_ (garbage-collection-count))
         (_ ($heap-remaining))
         ;; TODO: check if this is right by averaging even more
         ;; numbers and see if this is a good approximation
         (overhead (div (- t3 t0) 4)))
    (let* ((gc0 (garbage-collection-count))
           (time0 (current-processor-time))
           (hr0 ($heap-remaining))
           (tsc0 (rdtsc 'start)))
      (let ((ret (thunk)))
        (let* ((tsc1 (rdtsc 'stop))
               (hr1 ($heap-remaining))
               (time1 (current-processor-time))
               (gc1 (garbage-collection-count)))
          (print "Timings for " what ":")
          (print "  " (- gc1 gc0) " garbage collection runs")
          (print "  " (div (- time1 time0 overhead) (expt 10 3))
                 " µs elapsed process time")
          (print "  " (- tsc1 tsc0) " elapsed processor cycles")
          ;; hr0-hr1 is accurate if there were no collections
          ;; If there were collections then take ...
          (when (= gc0 gc1)
            (print "  " (div (- hr0 hr1) 8) " Q allocated")))
        ret))))

(define (time-it* what iterations thunk)
  ;; A good reference on this is "How to Benchmark Code Execution
  ;; Times on Intel® IA-32 and IA-64 Instruction Set Architectures
  ;; (324264-001)." This should preferably run without interrupts.
  ;; Under Linux the thread should be pinned to a CPU (see taskset).
  ;; Any sort of power optimizations and core boosting should really
  ;; be turned off. TODO: add a complexity count, as in the paper?
  ;; TODO: read the cycle count with RDPMC
  (define (print . x) (for-each display x) (newline))
  (define (fmt v)
    (let* ((s (number->string (exact (round (* v 100)))))
           (n (string-length s)))
      (if (< n 2)
          (string-append "." s)
          (string-append (substring s 0 (fx- n 2)) "."
                         (substring s (fx- n 2) n)))))
  (assert (fxpositive? iterations))
  (print "Timing " what " to find the minimum cycle time:")

  (do ((i 0 (fx+ i 1)))
      ((fx=? i 3))
    ;; Warm up the code.
    (thunk))
  (let lp ((least (expt 2 60))
           (greatest 0)
           (i iterations)
           (n 0)
           (sum 0)
           (sumsq 0))
    (cond ((fxzero? i)
           (let* ((µ (/ sum n))
                  (σ² (/ (- sumsq (/ (* sum sum) n))
                         n))          ;For s² use (/ ... (- n 1))
                  (σ (sqrt σ²)))
             ;; I use the population variance, because the whole
             ;; population was sampled. Outliers were discarded, but
             ;; they are not wanted anyway.
             (print "\n  The cycle count varied between " least " and " greatest)
             (print "  (Arithmetic mean)      µ  = " (fmt µ))
             (print "  (Standard deviation)   σ  = " (fmt σ))
             (print "  (Population variance)  σ² = " (fmt σ²))
             (display "                    min x_i = µ")
             (let ((devs (/ (- least µ) σ)))
               (cond ((positive? devs) (print "+" (fmt devs) "σ"))
                     ((negative? devs) (print (fmt devs) "σ")))
               (print "  Used " n " samples ("
                      (- iterations n) " outliers discarded).")
               (when (> (abs devs) 1)
                 (print "\n  You might have a bad test setup. Disable frequency scaling, etc.\n"
                        "  Do not simply rerun the test until this message disappears.\n")))
             least))
          (else
           (let ((tsc0 (rdtsc 'start)))
             (thunk)
             (let ((cycles (fx- (rdtsc 'stop) tsc0)))
               (assert (fx>=? cycles 0))
               (when (< cycles least)
                 (print "New minimum is " cycles " cycles with " i " iterations to go."))
               (let ((least (min cycles least))
                     (greatest (max cycles greatest))
                     (i (fx- i 1)))
                 ;; Discard outliers. Doing this seems questionable.
                 (if (> cycles (* least 2))
                     (lp least greatest i n sum sumsq)
                     (lp least greatest i
                         (+ n 1)
                         (+ sum cycles)
                         (+ sumsq (* cycles cycles)))))))))))

;; XXX: this should basically be like fcdisasm, but better. It
;; should be able to mark the "current" instruction. It should
;; insert labels for local branches. It should look up branch
;; destinations in the symbol table. It should translate
;; immediates into objects. It should translate comparisons into
;; predicates. It should translate memory references.
(define (disassemble proc)
  (define (print . x) (for-each display x) (newline))
  (define (info-label x) ($box-ref x 4))
  (define (info-end-label x) ($box-ref x 5))
  (define (copy-code x)
    (let* ((info ($procedure-info x))
           (label (info-label info))
           (size (fx- (info-end-label info) label)))
      (do ((bv (make-bytevector size))
           (addr label (fx+ addr 1))
           (i 0 (fx+ i 1)))
          ((fx=? i size) bv)
        (bytevector-u8-set! bv i ($get-mem-u8 addr)))))
  (define (get-instructions p)
    (let lp ((rip- 0))
      (let ((bytes '()))
        (let* ((ins (disassemble1 p (lambda x (set! bytes (cons x bytes)))))
               (rip+ (port-position p)))
          (if (eof-object? ins)
              '()
              (cons (cons* rip- rip+ (reverse bytes) ins)
                    (lp rip+)))))))
  (define (recover-local-labels ins*)
    (define last-rip (caar (reverse ins*)))
    (define addrs (make-eqv-hashtable))
    ;; Build a hashtable of branch targets.
    (let lp ((ins* ins*) (i 0))
      (match ins*
        ;; XXX: not very clever about what is a branch.
        [((rip- rip+ bytes . (J ('+ 'rip disp))) . ins*)
         (let ((target (+ rip+ disp)))
           (cond ((fx<=? 0 target last-rip)
                  (let ((l (vector i)))
                    ;; TODO: have the label numbers incrementing
                    (hashtable-set! addrs target l)
                    (lp ins* (fx+ i 1))))
                 (else (lp ins* i))))]
        [(_ . ins*) (lp ins* i)]
        [() #f]))
    ;; Now insert label declarations and find the maximum size (w)
    ;; of an instruction (used later when printing).
    (let lp ((ins* ins*) (w 0) (ret* '()))
      (match ins*
        [((rip- rip+ bytes . ins) . ins*)
         (let ((ret* (cond ((hashtable-ref addrs rip- #f)
                            => (lambda (l) `((%label ,l) ,@ret*)))
                           (else ret*)))
               (w (fxmax w (fx- rip+ rip-)))
               (ins^
                (match ins
                  [(J ('+ 'rip disp))
                   (let ((target (+ rip+ disp)))
                     (cond ((hashtable-ref addrs target #f)
                            => (lambda (l) `(,J ,l)))
                           (else ins)))]
                  [_ ins])))
           (lp ins* w `((,rip- ,rip+ ,bytes . ,ins^) ,@ret*)))]
        [() (values (reverse ret*) w)])))
  (define print-instr/sexpr
    (match-lambda
     [('* reg 1)
      (display reg)]
     [(and (_ . _) i)
      (display #\()
      (let lp ((i i))
        (unless (null? i)
          (print-instr/sexpr (car i))
          (unless (null? (cdr i))
            (display #\space)
            (lp (cdr i)))))
      (display #\))]
     [(? number? i)
      (display "#x")
      (display (number->string i 16))]
     [(? vector? L)
      ;; Label hack. Might be nice to have error-handling labels
      ;; named assertion0, restart0, etc.
      (display (label-name L))]
     [x (display x)]))
  (define (print1 entry maxsz rip- rip+ bytes ins)
    (display "   ")
    (display (number->string (fx+ entry rip-) 16))
    (display #\space)
    (for-each (match-lambda
               [(tag . byte*)
                (case tag
                  ((modr/m sib) (display "\x1b;[1;34m"))
                  ((opcode) (display "\x1b;[1;32m"))
                  ((prefix) (display "\x1b;[1;33m"))
                  ((immediate) (display "\x1b;[1;37m"))
                  ((disp offset) (display "\x1b;[1;35m"))
                  (else (display "\x1b;[0m")))
                (for-each (lambda (byte)
                            (when (fx<? byte #x10)
                              (display #\0))
                            (display (number->string byte 16)))
                          byte*)])
              bytes)
    (display "\x1b;[0m")
    ;; Align the hexdump
    (display (make-string (fx+ 1 (fx* 2 (fx- maxsz (fx- rip+ rip-)))) #\space))
    (print-instr/sexpr ins)
    (newline))
  (define (comment x)
    (print " ; " x))
  (define (label-name x)
    (match x
      [#(L) (string-append "L" (number->string L))]))
  (define (instruction*-comment ins*)
    ;; Two instructions are available. Can't see past labels.
    (match ins*
      [(('mov r1 ('mem64+ r2 -2)) . _)
       `(set! ,r1 (car ,r2))]
      [(('mov r1 ('mem64+ r2 +6)) . _)
       `(set! ,r1 (cdr ,r2))]
      [(('mov r1 ('mem64+ r2 (? fixnum? disp) ('* r3 '1))) . _)
       (let ((idx (fxand disp (fxnot #b111))))
         (case (fx- 8 (fxand disp #b111))
           ((#b001) `(set! ,r1 ($box-ref ,r2 ,r3)))
           ;; ((#b011) "procedure.")
           ;; ((#b100) "string.")
           ((#b110) `(set! ,r1 (vector-ref ,r2 (+ ,r3 ,idx))))
           ;; ((#b101) "bytevector.")
           (else #f)))]
      [(('cmp r1 ('mem64+ r2 (? fixnum? disp)))
        ('jnb L) . _)
       (let ((idx (fxand disp (fxnot #b111))))
         (and (zero? idx)
              (case (fx- 8 (fxand disp #b111))
                ((#b100) `(unless (fx<? -1 ,r1 (string-length ,r2))
                            (goto ,(label-name L))))
                ((#b110) `(unless (fx<? -1 ,r1 (vector-length ,r2))
                            (goto ,(label-name L))))
                ((#b101) `(unless (fx<? -1 ,r1 (bytevector-length ,r2))
                            (goto ,(label-name L))))
                (else #f))))]
      [(('test r1 '7) ('jnz L) . _)
       `(unless (fixnum? ,r1) (goto ,(label-name L)))]
      [(('ud2 . _) . _)
       `(raise (make-assertion-violation))]
      [(('sub 'r13 (? fixnum? n)) ('js L) . _)
       `(unless (can-allocate? r14 (* ,(/ n 8) 8)) (goto ,(label-name L)))]
      [(('add 'r14 (? fixnum? n)) . _)
       `(allocate! r14 (* ,(/ n 8) 8))]
      [(('mov ('mem64+ 'r14 . x) rhs) . _)
       `(allocate-set! (+ r14 . ,x) ,rhs)]
      [(('lea r ('mem+ 'r14 2)) . _)
       `(set! ,r (->pair r14))]
      [_ #f]))
  (define (print-code/amd64 ins* maxsz entry)
    (print "  entry:")
    (let lp ((ins* ins*))
      (match ins*
        [(('%label L) . ins*)
         (print "  " (label-name L) ":")
         (lp ins*)]
        [((rip- rip+ bytes . ins) . ins*)
         (cond ((instruction*-comment
                 (cons ins (match ins*
                             [((_ _ _ . ins) . _) (list ins)]
                             [((%label . _) . _) '()]
                             [() '()])))
                => comment))
         (print1 entry maxsz rip- rip+ bytes ins)
         (lp ins*)]
        [() (if #f #f)])))
  (assert (procedure? proc))
  (print "Disassembly for " proc #\newline)
  (let ((p (open-bytevector-input-port (copy-code proc))))
    ;; TODO: might be interesting to use pass-optimize to analyze
    ;; the code.
    (let-values (((ins* w) (recover-local-labels (get-instructions p))))
      (let ((entry (info-label ($procedure-info proc))))
        (print-code/amd64 ins* w entry)))))

(define (stack-trace p)
  ;; This code is currently specific to amd64. This code will work
  ;; *very* unreliably if interpreted by a tree code interpreter.
  (define (print . x) (for-each (lambda (x) (display x p)) x) (newline p))
  (define (get-mem64 addr)
    (bitwise-ior (bitwise-arithmetic-shift-left ($get-mem-u32 (fx+ addr 4)) 32)
                 ($get-mem-u32 addr)))
  (define (get-mem64/unaligned addr)
    (bitwise-ior (bitwise-arithmetic-shift-left ($get-mem-u32 (fx+ addr 4)) 32)
                 ($get-mem-u32 addr)))
  (define (get-mem-uint addr size)
    ;; XXX: this clearly shows that the encoding is too complex for
    ;; an assembler decoder. Always using two bytes for the size
    ;; should solve this.
    (let lp ((addr addr) (size size) (ret 0))
      (if (fxzero? size)
          ret
          (lp (fx+ addr 1)
              (fx- size 1)
              (fxior (fxarithmetic-shift-left ret 8)
                     ($get-mem-u8 addr))))))
  (define (get-livemask addr size1)
    ;; The first nop instruction has size1 bytes of live mask. The
    ;; following ones (if they exist) have five bytes each.
    (let lp ((mask (get-mem-uint addr size1))
             (addr (fx+ addr size1)))
      (let ((op0 ($get-mem-u8 addr))
            (op1 ($get-mem-u8 (fx+ addr 1)))
            (modr/m ($get-mem-u8 (fx+ addr 2)))
            (addr (fx+ addr 3)))
        (if (and (fx=? op0 #x0F)
                 (fx=? op1 #x1F)
                 (fx=? modr/m #b10100100))
            (lp (bitwise-ior (bitwise-arithmetic-shift-left
                              (get-mem-uint addr 4 #;what?))
                             mask)
                (fx+ addr 5))
            mask))))
  (print "Stack trace:")
  (let ((top (stack-top))
        (rsp ($stack-pointer)))
    (let lp ((frame 0) (rsp rsp))
      (cond ((fx=? rsp top)
             (print "End of stack trace"))
            ((fx>? rsp top)
             (print "End of stack trace due to OVERRUN"))
            (else
             (let ((rip (get-mem64 rsp))
                   (rsp (fx+ rsp 8)))
               ;; TODO: look up where the code of rip comes from
               (print " Frame " frame " has return address #x" (number->string rip 16) ".")
               (when (<= #x200000 rip #xffffffff) ;XXX: should probably just catch #PF
                 (let ((frame (fx+ frame 1))
                       (op0 ($get-mem-u8 rip))
                       (op1 ($get-mem-u8 (fx+ rip 1)))
                       (modr/m ($get-mem-u8 (fx+ rip 2))))
                   (let ((size-bytes (fxbit-field modr/m 3 7)))
                     (cond ((or (not (fx=? op0 #x0F))
                                (not (fx=? op1 #x1F))
                                (fxzero? size-bytes))
                            ;; This is not a liveness NOP, skip it.
                            (print "  (no live locals)")
                            (lp frame rsp))
                           (else
                            (let ((locals (fx+ (get-mem-uint (fx+ rip 3) size-bytes) 1))
                                  (livemask (get-livemask (fx+ (fx+ rip 3) size-bytes)
                                                          size-bytes)))
                              (do ((i 0 (fx+ i 1))
                                   (laddr rsp (fx+ laddr 8)))
                                  ((fx=? i locals))
                                (cond ((bitwise-bit-set? livemask i)
                                       ;; XXX: ONLY if the local is in
                                       ;; the livemask is it ever safe
                                       ;; to do $get-mem-object on it.
                                       (display "  Local " p)
                                       (display i p)
                                       (display ": " p)
                                       (let ((v ($get-mem-object laddr)))
                                         ;; TODO: size-limited pretty printing
                                         (write v p)
                                         (newline p)))
                                      #;
                                      (else
                                       (print "   Local " i " is not live."))))
                              (lp frame (fx+ rsp (fx* locals 8)))))))))))))))

;; Valgrind requests (valgrind.h)
(define RUNNING_ON_VALGRIND #x1001)
(define DISCARD_TRANSLATIONS #x1002)
(define CLIENT_CALL0 #x1101)
(define CLIENT_CALL1 #x1102)
(define CLIENT_CALL2 #x1103)
(define CLIENT_CALL3 #x1104)
(define COUNT_ERRORS #x1201)
(define GDB_MONITOR_COMMAND #x1202)
(define MALLOCLIKE_BLOCK #x1301)
(define RESIZEINPLACE_BLOCK #x130b)
(define FREELIKE_BLOCK #x1302)
(define CREATE_MEMPOOL #x1303)
(define DESTROY_MEMPOOL #x1304)
(define MEMPOOL_ALLOC #x1305)
(define MEMPOOL_FREE #x1306)
(define MEMPOOL_TRIM #x1307)
(define MOVE_MEMPOOL #x1308)
(define MEMPOOL_CHANGE #x1309)
(define MEMPOOL_EXISTS #x130a)
(define PRINTF_VALIST_BY_REF #x1403)
(define PRINTF_BACKTRACE_VALIST_BY_REF #x1404)
(define STACK_REGISTER #x1501)
(define STACK_DEREGISTER #x1502)
(define STACK_CHANGE #x1503)
(define LOAD_PDB_DEBUGINFO #x1601)
(define MAP_IP_TO_SRCLOC #x1701)
(define CHANGE_ERR_DISABLEMENT #x1801)

(define (user-request-tool-base a b)
  (bitwise-ior (bitwise-arithmetic-shift-left (fxand (char->integer a) #xff) 24)
               (bitwise-arithmetic-shift-left (fxand (char->integer b) #xff) 16)))

;; Memcheck requests (memcheck.h)
(define MAKE_MEM_NOACCESS (user-request-tool-base #\M #\C))
(define MAKE_MEM_UNDEFINED (+ MAKE_MEM_NOACCESS 1))
(define MAKE_MEM_DEFINED (+ MAKE_MEM_NOACCESS 2))
(define DISCARD (+ MAKE_MEM_NOACCESS 3))
(define CHECK_MEM_IS_ADDRESSABLE (+ MAKE_MEM_NOACCESS 4))
(define CHECK_MEM_IS_DEFINED (+ MAKE_MEM_NOACCESS 5))
(define DO_LEAK_CHECK (+ MAKE_MEM_NOACCESS 6))
(define COUNT_LEAKS (+ MAKE_MEM_NOACCESS 7))
(define GET_VBITS (+ MAKE_MEM_NOACCESS 8))
(define SET_VBITS (+ MAKE_MEM_NOACCESS 9))
(define CREATE_BLOCK (+ MAKE_MEM_NOACCESS 10))
(define MAKE_MEM_DEFINED_IF_ADDRESSABLE (+ MAKE_MEM_NOACCESS 11))
(define COUNT_LEAK_BLOCKS (+ MAKE_MEM_NOACCESS 12))
(define ENABLE_ADDR_ERROR_REPORTING_IN_RANGE (+ MAKE_MEM_NOACCESS 13))
(define DISABLE_ADDR_ERROR_REPORTING_IN_RANGE (+ MAKE_MEM_NOACCESS 14))


(define valgrind
  (case-lambda
    ((request)
     (assert (fx=? request RUNNING_ON_VALGRIND))
     (let ((req (make-bytevector (* 7 8) 0)))
       (bytevector-u64-native-set! req 0 request)
       ($valgrind ($bytevector-location req))))
    ((request arg0)
     (assert (fx=? request RUNNING_ON_VALGRIND))
     (let ((req (make-bytevector (* 7 8) 0)))
       (bytevector-u64-native-set! req 0 request)
       (bytevector-u64-native-set! req 8 arg0)
       ($valgrind ($bytevector-location req))))))


)
