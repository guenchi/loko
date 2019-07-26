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

;;; Multiboot-specific initialization

;; At this point the standard library has been loaded and should be
;; available.

;; This libary is responsible for reading the boot loader data,
;; starting up processes and doing process scheduling.

;;; TODO: mmap must be able to manage virtual addresses itself (like
;;; Linux's mmap). Make it so the address can be a type of address
;;; instead: 'low-1MB, 'low-4GB or 'full-64bit ?

(library (loko arch amd64 pc-init)
  (export)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (srfi :98 os-environment-variables)
    (loko arch amd64 pc-ap-boot)
    (only (loko system $repl) print-condition)
    (loko system $asm-amd64)
    (loko system $host)
    (loko system $io)
    (only (loko system $bytevectors)
          $bytevector-location)
    (loko system $processes)
    (only (loko libs context)
          CPU-VECTOR:SCHEDULER-SP
          CPU-VECTOR:LAST-INTERRUPT-VECTOR))

;; Alist of filename to textual input port. This will represent some
;; sort of early filesystem.
(define *modules* '())

;; Loko syscalls
(define (sys_hlt) ($syscall -1))

(define (read-msr reg)
  (let ((bv (make-bytevector 8)))
    ($syscall -2 reg ($bytevector-location bv))
    (bytevector-u64-native-ref bv 0)))

(define (write-msr reg v)
  ($syscall -3 reg (bitwise-and v #xffffffff)
            (bitwise-arithmetic-shift-right v 32)))

(define (copy-utf8z addr)
  (do ((end addr (fx+ end 1))
       (len 0 (fx+ len 1)))
      ((fxzero? ($get-mem-u8 end))
       (do ((ret (make-bytevector len))
            (i (fx- end 1) (fx- i 1))
            (len (fx- len 1) (fx- len 1)))
           ((fx<=? len -1) ret)
         (bytevector-u8-set! ret len ($get-mem-u8 i))))))

;; Custom ports for reading from memory
(define (open-memory-input-port fn base size)
  (define (read! bv start count)
    (do ((n (fxmin count size))
         (addr base (fx+ addr 1))
         (i start (fx+ i 1)))
        ((fx=? i n)
         (set! base (fx+ base n))
         (set! size (fx- size n))
         n)
      (bytevector-u8-set! bv i ($get-mem-u8 addr))))
  (define (close)
    #f)
  (transcoded-port (make-custom-binary-input-port fn read! #f #f close)
                   (native-transcoder)))

(define (clear-page &page)
  (do ((top (fx+ &page 4096))
       (addr &page (fx+ addr 16)))
      ((fx=? addr top))
    ($put-mem-s61 addr 0)
    ($put-mem-s61 (fx+ addr 8) 0)))

;; Parse the multiboot command line. Environment variables are
;; key=value before "--" and anything after is parsed as command line
;; arguments. Assumes that the very first item on the command line is
;; the name of the Multiboot binary.
;; Example:
;; (pc-init-parse-command-line "./loko repl=no x -- arg1 arg2")
;; => (("repl" . "no") ("x" . ""))
;;    ("./loko" "arg1" "arg2")
(define (pc-init-parse-command-line cmdline)
  (define (string-index s c)
    (let lp ((i 0))
      (and (not (fx=? i (string-length s)))
           (if (eqv? c (string-ref s i))
               i
               (lp (fx+ i 1))))))
  (define (get-word p)
    (call-with-string-output-port
      (lambda (outp)
        (let lp ()
          (when (memv (lookahead-char p) '(#\space #\tab))
            (get-char p)
            (lp)))
        (let lp ()
          (let ((c (get-char p)))
            (unless (or (eof-object? c) (memv c '(#\space #\tab)))
              (put-char outp c)
              (lp)))))))
  (let ((p (open-string-input-port cmdline)))
    (let* ((cmdline0 (get-word p))
           (env (let lp ((var* '()))
                  (let ((str (get-word p)))
                    (if (or (string=? str "") (string=? str "--"))
                        (reverse var*)
                        (lp (cons (let ((idx (string-index str #\=)))
                                    (if (not idx)
                                        (cons str "")
                                        (cons (substring str 0 idx)
                                              (substring str (fx+ idx 1) (string-length str)))))
                                  var*))))))
           (cmdline (let lp ((var* '()))
                      (let ((str (get-word p)))
                        (if (string=? str "")
                            (reverse var*)
                            (lp (cons str var*)))))))
      (values env (cons cmdline0 cmdline)))))

;; Check that the CPU is supported.
(define (check-cpu)
  (let-values (((a b c d) (cpuid 1)))
    ;; Check for Local APIC and RDMSR/WRMSR
    (unless (and (fxbit-set? d 9)
                 (fxbit-set? d 5))
      (error 'check-cpu
             "This kernel does not run on systems without a Local APIC and MSR"
             a b c d))))

;;; Early serial driver

(define (init-early-serial-driver)
  ;; Trivial early serial driver
  (define com0 #x3f8)
  (define rbr (+ com0 0))
  (define thb (+ com0 0))
  (define lsr (+ com0 5))
  (define (serial-put-u8 b)
    (let lp ()
      (when (fxzero? (fxand ($get-i/o-u8 lsr) #b100000))
        (lp)))
    ($put-i/o-u8 thb b))
  (define (serial-put bv start count)
    (do ((end (fx+ start count))
         (i start (fx+ i 1)))
        ((fx=? i end) count)
      (serial-put-u8 (bytevector-u8-ref bv i))))
  ($init-standard-ports (lambda (bv start count)
                          (let lp ()
                            (when (fxzero? (fxand ($get-i/o-u8 lsr) #b1))
                              (lp)))
                          (bytevector-u8-set! bv start ($get-i/o-u8 rbr))
                          1)
                        serial-put serial-put
                        (eol-style crlf)))

;;; Interrupt controller code

;; Intel reserves the first 32 interrupt vectors for exceptions
;; and such. Let's put the legacy IRQs immediately after that.
(define PIC1-vector-offset 32)
(define PIC2-vector-offset (+ PIC1-vector-offset 8))
(define picm #x20)
(define pics #xA0)
(define *picm-mask* #b11111111)
(define *pics-mask* #b11111111)

(define (pic-init base idt-offset)
  (define cmd base)
  (define data (fx+ base 1))
  (define icw3 (if (fx=? base picm)
                   #b100          ;IRQ2 has a slave
                   2))            ;slave ID
  ($put-i/o-u8 cmd #b10001)       ;ICW1: ICW4 needed
  ($put-i/o-u8 data idt-offset)   ;ICW2: vector address
  ($put-i/o-u8 data icw3)         ;ICW3
  ($put-i/o-u8 data #b1))         ;ICW4: Intel Architecture

(define (OCW1 base m)
  ;; Used to mask IRQs
  ($put-i/o-u8 (fx+ base 1) m))

(define (OCW2 base rotate? specific? eoi? level)
  ;; Used to acknowledge IRQs
  ($put-i/o-u8 base (fxior (if rotate?   #b10000000 0)
                           (if specific? #b1000000 0)
                           (if eoi?      #b100000 0)
                           (fxand level #b111))))

(define (OCW3 base special-mask-mode poll? reg)
  ;; Used to read IRR/ISR
  ($put-i/o-u8 base (fxior (if poll?   #b100 0)
                           (case reg
                             ((ir) #b10)
                             ((is) #b11)
                             (else 0))
                           (case special-mask-mode
                             ((set)   #b1100000)
                             ((reset) #b1000000)
                             (else #f))
                           #b1000)))

(define (pic-mask base mask)
  ;; Bit 0 in the mask is IRQ 0 (or IRQ 8).
  (if (fx=? base picm)
      (OCW1 base (fxand mask #b11111011)) ;enable cascade
      (OCW1 base mask)))

(define (pic-ack irq)
  ;; Signal End of Interrupt and set `irq' to the bottom
  ;; priority level.
  (cond ((fx<? irq 8)
         (OCW2 picm #t #t #t irq))
        (else
         ;; Slave + cascade level
         (OCW2 pics #t #t #t irq)
         (OCW2 picm #f #t #t 2))))

(define (pic-enable irq)
  (cond ((fx<? irq 8)
         (let ((bit (fxarithmetic-shift-left 1 irq)))
           (set! *picm-mask* (fxand *picm-mask* (fxnot bit)))
           (pic-mask picm *picm-mask*)))
        ((fx<? irq 16)
         (let ((bit (fxarithmetic-shift-left 1 (fx- irq 8))))
           (set! *pics-mask* (fxand *pics-mask* (fxnot bit)))
           (pic-mask pics *pics-mask*)))))

;; Interrupt vector numbers. The spurious vector has the three lower
;; bits set to #b111.
(define APIC-vector-offset (+ PIC2-vector-offset 8))
(define APIC-vector-timer (+ APIC-vector-offset 1))
(define APIC-vector-spurious (+ APIC-vector-offset 15))

;; The APIC base address MSR
(define APIC-MSR #x0000001B)
(define APIC-base-reg:AE 11)  ;APIC enabled
(define APIC-base-reg:BSC 8)  ;boot strap CPU core
(define ABA #xFEE00000)       ;default APIC base

;; APIC registers.
(define APIC:task-priority (+ ABA #x80))
(define APIC:logical-destination (+ ABA #xD0))
(define APIC:destination-format (+ ABA #xE0))
(define APIC:spurious-vector (+ ABA #xF0))
(define APIC:ICR-low (+ ABA #x300))
(define APIC:ICR-high (+ ABA #x310))
(define APIC:thermal-vector (+ ABA #x330))
(define APIC:performance-vector (+ ABA #x340))
(define APIC:LINT0-vector (+ ABA #x350))
(define APIC:LINT1-vector (+ ABA #x360))
(define APIC:error-vector (+ ABA #x370))

(define (check-apic-base)
  (unless (eqv? ABA (bitwise-and (read-msr APIC-MSR) #x000FFFFFFFFFF000))
    (error 'check-apic-base
           "The Local APIC must not be relocated before boot")))

;; APIC register helpers
(define (write-spurious-int enable? spurious-vector)
  (define ASE (expt 2 8))     ;APIC Software Enable
  (define FCC (expt 2 9))     ;Focus CPU Core Checking
  ($put-mem-u32 APIC:spurious-vector
                (fxior (fxand spurious-vector #xff)
                       (if enable? ASE 0))))

(define (write-timer-initial-count value)
  ($put-mem-u32 (fx+ ABA #x380) value))

(define (read-timer-current-count)
  ($get-mem-u32 (fx+ ABA #x390)))

(define (write-timer-divide divisor)
  ;; The CPU core clock divisor for the timer.
  (let ((value
         (case divisor
           ((1) #b111)
           ((2) #b000)
           ((4) #b001)
           ((8) #b010)
           ((16) #b011)
           ((32) #b100)
           ((64) #b101)
           ((128) #b110)
           (else
            (error 'write-timer-divide "Invalid divisor" divisor)))))
    ($put-mem-u32 (fx+ ABA #x3E0)
                  (fxior
                   (fxarithmetic-shift-left (fxand value #b100) 1)
                   (fxand value #b11)))))

(define (write-timer-vector x)
  ($put-mem-u32 (fx+ ABA #x320) x))

(define (apic-EOI)
  ;; End of interrupt. It acknowledges one interrupt.
  ;; Whichever one it was, I can't recall.
  ($put-mem-u32 (fx+ ABA #xB0) 0))

(define LVT-MASK (expt 2 16))
(define LVT-TMM (expt 2 17))  ;timer mode (1 = periodic)
(define LVT-TGM (expt 2 15))  ;1 = level, 0 = edge
(define LVT-MT-FIXED    #b00000000000) ;vector field is used
(define LVT-MT-SMI      #b01000000000) ;SMI
(define LVT-MT-NMI      #b10000000000) ;NMI
(define LVT-MT-EXTERNAL #b11100000000) ;external interrupt
(define apic-divisor 32)               ;for the calibration

(define (calibrate-APIC&CPU-against-PIT)
  ;; i8253/i8254 PIT constants
  (define PIT-delay 1/100)    ;seconds to run calibration
  (define PIT-freq (+ 1193181 2/3))
  ;; (define PIT-count (round (* PIT-freq PIT-delay)))
  (define PIT-count 11932)
  ;; I/O ports for PIT
  (define COUNTER-0 #x40)     ;IRQ0
  (define COUNTER-1 #x41)
  (define COUNTER-2 #x42)     ;Speaker
  (define CONTROL #x43)
  ;; Control register bit definitions
  (define COUNT-BINARY 0)
  (define COUNT-BCD 1)
  (define MODE-INTERRUPT-ON-TERMINAL-COUNT #b000000)
  (define MODE-PROGRAMMABLE-ONESHOT        #b000010)
  (define MODE-RATE-GENERATOR              #b000100)
  (define MODE-SQUARE-WAVE-GENERATOR       #b001000)
  (define MODE-SOFTWARE-TRIGGERED-STROBE   #b010000)
  (define MODE-HARDWARE-TRIGGERED-STROBE   #b100000)
  (define LATCH-COUNTER       #o000)
  (define COUNTER-LOW         #o020)
  (define COUNTER-HIGH        #o040)
  (define COUNTER-WORD        #o060)
  (define SELECT-0            #o000)
  (define SELECT-1            #o100)
  (define SELECT-2            #o200)
  ;; NMI Status and Control Register bits
  (define NMI-S&C #x61)     ;NMI Status and Control Register
  (define NMI-MBZ      #b11110000) ;Written as zero
  (define TMR2-OUT-STS   #b100000)
  (define SPKR-DAT-EN    #b000010) ;Speaker Data Enable
  (define TIM-CNT2-EN    #b000001) ;Timer Counter 2 Enable
  ;; Disable the spaker and enable the timer counter 2
  ;; output bit. Then start the PIT timer.
  ($put-i/o-u8 NMI-S&C
               (fxand (fxior ($get-i/o-u8 #x61)
                             TIM-CNT2-EN)
                      (fxnot NMI-MBZ)
                      (fxnot SPKR-DAT-EN)))
  ($put-i/o-u8 CONTROL
               (fxior COUNT-BINARY
                      MODE-INTERRUPT-ON-TERMINAL-COUNT
                      COUNTER-WORD
                      SELECT-2))
  ($put-i/o-u8 COUNTER-2 (fxand PIT-count #xff))
  ($put-i/o-u8 COUNTER-2 (fxarithmetic-shift-right PIT-count 8))
  (write-timer-vector APIC-vector-timer)
  (write-timer-initial-count #xffffffff) ;Start APIC counting
  (let ((initial-tsc (rdtsc)))
    ;; Wait for PIT to finish. No GC runs, please.
    (let lp ()
      (when (fxzero? (fxand ($get-i/o-u8 NMI-S&C) TMR2-OUT-STS))
        (lp)))
    ;; Stop the timer
    (write-timer-vector LVT-MASK)
    ;; Finally read the APIC timer and calculate the bus frequency.

    ;; TODO: the bus frequency can vary from calibration to
    ;; calibration, but it is usually something like 200 MHz, 400 MHz,
    ;; 1 GHz, etc. Maybe it would be a good idea to try and round the
    ;; measured value if it's close to a multiple of 100 MHz?

    ;; TODO: what about running it multiple times and taking the minimums?

    ;; TODO: investigate what the divisor is for
    (let* ((current-count (read-timer-current-count))
           (current-tsc (rdtsc))) ;FIXME: can wrap
      (values (* (- #xffffffff current-count)
                 (* apic-divisor (/ PIT-delay)))
              (* (- current-tsc initial-tsc)
                 (/ PIT-delay))))))

;;; Scheduler

(define-record-type pcb
  (sealed #t)
  (fields (immutable pid)
          (mutable next)
          (mutable prev)
          (mutable sp)
          (mutable status)
          (mutable msg)
          (mutable wakeup)
          (mutable wait-vector)
          (mutable queue))
  (protocol
   (lambda (p)
     (lambda (pid sp)
       (p pid #f #f sp 'run #f #f #f '())))))

(define (pcb-link! q np)
  (cond ((or (not q) (eq? q np))
         (pcb-prev-set! np np)
         (pcb-next-set! np np)
         np)
        (else
         (let ((a (pcb-prev q))
               (b np)
               (c q))
           (pcb-next-set! a b)
           (pcb-prev-set! b a)
           (pcb-next-set! b c)
           (pcb-prev-set! c b)
           q))))

(define (pcb-unlink! q)
  ;; q must also be the currently pointed to entry.
  (let ((a (pcb-prev q))
        (c (pcb-next q)))
    (cond ((eq? a q)
           #f)
          (else
           (pcb-next-set! a c)
           (pcb-prev-set! c a)
           c))))

(define (pcb-enqueue-message! pcb msg)
  ;; TODO: real messages and real queues
  ;; (print "Message to " (pcb-pid pcb) ":")
  ;; (write msg)
  ;; (newline)
  (pcb-queue-set! pcb (cons msg (pcb-queue pcb))))

(define (pcb-dequeue-message! pcb)
  (let* ((rd (reverse (pcb-queue pcb)))
         (ret (car rd)))
    (pcb-queue-set! pcb (reverse (cdr rd)))
    ret))

(define (nanoseconds->TSC cpu-freq t)
  ;; Converts from nanoseconds to CPU cycles.
  (round (* t (/ cpu-freq (expt 10 9)))))

(define (TSC->nanoseconds cpu-freq t)
  (div (* t (expt 10 9)) cpu-freq))

;; Pid 0 for Loko on PC
(define (pc-scheduler cpu-freq interval dma-allocate)
  ;; FIXME: passing in dma-allocate here is ugly.
  (define *runq* #f)
  (define *waitq* '())
  (define *irq-vectors* (make-vector 256 #f))
  (define *next-pid* 2)
  (define *pids* (make-eqv-hashtable))
  (define (get-saved-sp!)
    ;; After $switch-stack has returned to the scheduler
    ;; this is where the stack pointer of the other
    ;; process is stored.
    (let ((sp ($processor-data-ref CPU-VECTOR:SCHEDULER-SP)))
      ($processor-data-set! CPU-VECTOR:SCHEDULER-SP 0) ;indicate scheduler is running
      sp))
  (define (get-saved-irq!)
    ;; The returned value is the interrupt vector number
    ;; if the interrupt that happened during the time a
    ;; process was scheduled or at the time the HLT
    ;; instruction was issued.
    (let ((IRQ ($processor-data-ref CPU-VECTOR:LAST-INTERRUPT-VECTOR)))
      ($processor-data-set! CPU-VECTOR:LAST-INTERRUPT-VECTOR #f)
      IRQ))

  ;; Start the boot process.
  (let ((boot (make-pcb 1 ($process-start 1))))
    (set! *runq* (pcb-link! *runq* boot))
    (hashtable-set! *pids* (pcb-pid boot) boot))

  (let loop ()
    (define current-time (rdtsc)) ;FIXME: can wrap
    (let ((IRQ (get-saved-irq!)))
      ;; First make runnable any process that was
      ;; waiting for this IRQ. If a runnable process
      ;; would receive an IRQ then it must be queued as
      ;; a message.

      ;; TODO: if there is an IRQ destined for a runnable
      ;; process, then it must be queued.
      (when (fixnum? IRQ)
        #;
        (cond ((eqv? IRQ (+ PIC1-vector-offset 7))
               ;; TODO: check ISR to see if it was really IRQ7
               ;; If it was not, do nothing.
               )
              ((eqv? IRQ (+ PIC2-vector-offset 7))
               ;; TODO: check ISR to see if it was really IRQ15
               ;; If it was not, then ACK irq 2 (cascade).
               ))
        (cond ((eqv? IRQ APIC-vector-timer)
               (apic-EOI))
              ((vector-ref *irq-vectors* IRQ) =>
               (lambda (pcb)
                 ;; TODO: IRQ sharing
                 (pcb-enqueue-message! pcb IRQ)))
              ((eqv? IRQ APIC-vector-spurious)
               ;; XXX: if the spurious interrupts come
               ;; without end, then probably an
               ;; interrupt in the APIC has not received
               ;; its EOI. Should increment a counter.
               ;; (display #\S)
               ;; (flush-output-port (current-output-port))
               #f)
              (else
               ;; TODO: disable the IRQ
               (display "nobody was interested in interrupt ")
               (display IRQ)
               (newline)
               ))))

    ;; TODO: this implementation of the wait queue
    ;; conses too much and is O(n) in the number of
    ;; sleeping processes.
    (unless (null? *waitq*)
      ;; Make runnable any process where there is a
      ;; message in the queue or the wakeup time is in
      ;; the past.
      (set! *waitq*
            (filter (lambda (pcb)
                      (let ((wakeup (pcb-wakeup pcb))
                            (wv (pcb-wait-vector pcb)))
                        (unless (vector? wv)
                          (error 'scheduler
                                 "A process is in the wait queue without a wait vector"
                                 (pcb-pid pcb)))
                        (cond ((not (null? (pcb-queue pcb)))
                               (when wakeup
                                 (vector-set! wv 1 (TSC->nanoseconds
                                                    cpu-freq
                                                    (- wakeup current-time))))
                               (vector-set! wv 2 (pcb-dequeue-message! pcb))
                               (set! *runq* (pcb-link! *runq* pcb))
                               (pcb-status-set! pcb 'runnable)
                               (pcb-wakeup-set! pcb #f)
                               (pcb-msg-set! pcb 'message)
                               #f)
                              ((not wakeup))
                              ((<= current-time wakeup))
                              (else
                               ;; (display "WAKING UP: ")
                               ;; (display current-time)
                               ;; (newline)
                               (vector-set! wv 1 (TSC->nanoseconds
                                                  cpu-freq
                                                  (- wakeup current-time)))
                               (set! *runq* (pcb-link! *runq* pcb))
                               (pcb-status-set! pcb 'runnable)
                               (pcb-wakeup-set! pcb #f)
                               (pcb-msg-set! pcb 'timeout)
                               #f))))
                    *waitq*)))

    ;; TODO: set the timeout based on the next wakeup
    (write-timer-initial-count interval)
    (write-timer-vector (fxior APIC-vector-timer LVT-TMM))
    (write-timer-divide apic-divisor)
    (cond ((not *runq*)
           (cond ((null? *waitq*)
                  (error 'scheduler "The last process has exited"))
                 (else
                  ;; Wait for an interrupt
                  (sys_hlt)
                  (loop))))
          (else
           ;; Schedule the next runnable process
           (let* ((pcb *runq*)
                  (msg ($switch-stack (pcb-sp pcb) (pcb-msg pcb))))
             ;; (write-timer-vector LVT-MASK)
             (pcb-sp-set! pcb (get-saved-sp!))
             (unless (eq? msg 'preempted)
               (pcb-msg-set! pcb #f))

             ;; These messages should be cleaned up
             ;; already by wrappers around $process-yield,
             ;; so no error catching is necessary here.
             (let lp ((msg msg))
               (when (vector? msg)
                 (case (vector-ref msg 0)
                   ((send)
                    (let ((recipient (vector-ref msg 1))
                          (msg (vector-ref msg 2)))
                      (cond ((hashtable-ref *pids* recipient #f) =>
                             (lambda (target)
                               (pcb-enqueue-message! target msg)
                               (pcb-msg-set! pcb 'ok)))
                            (else
                             ;; There is no such process...
                             (pcb-msg-set! pcb #f)))))
                   ((wait)
                    ;; TODO: if the queue is not empty,
                    ;; then take a message immediately
                    (let ((timeout (vector-ref msg 1)))
                      (cond ((null? (pcb-queue pcb))
                             (when timeout
                               ;; Minimum wait in nanoseconds,
                               ;; unless awoken by a message.
                               (let ((wakeup (+ current-time
                                                (nanoseconds->TSC cpu-freq timeout))))
                                 (pcb-wakeup-set! pcb wakeup)))
                             ;; The process is now waiting.
                             (pcb-status-set! pcb 'wait)
                             (pcb-wait-vector-set! pcb msg)
                             (set! *runq* (pcb-unlink! pcb))
                             (set! *waitq* (cons pcb *waitq*)))
                            (else
                             (let ((wv msg))
                               (vector-set! wv 1 timeout)
                               (vector-set! wv 2 (pcb-dequeue-message! pcb))
                               (pcb-msg-set! pcb 'message))))))
                   ((allocate)
                    (let ((type (vector-ref msg 1))
                          (size (vector-ref msg 2))
                          (mask (vector-ref msg 3)))
                      ;; Allocate a consecutive memory
                      ;; region. TODO: jot down this
                      ;; area as allocated to this
                      ;; process, and free it when the
                      ;; process dies. If the process is
                      ;; a PCI device driver then
                      ;; disconnect it from the memory
                      ;; bus first. Or just have a big
                      ;; old process that takes care of
                      ;; stuff like that.
                      (case type
                        ((dma)
                         (let-values (((cpu dma)
                                       (dma-allocate size mask)))
                           (cond (cpu
                                  (vector-set! msg 4 cpu)
                                  (vector-set! msg 5 dma)
                                  (pcb-msg-set! pcb 'ok))
                                 (else
                                  (pcb-msg-set! pcb #f)))))
                        (else
                         (display "Bad allocate message: ")
                         (write msg)
                         (newline)))))
                   (else
                    (display "Bad message: ")
                    (write msg)
                    (newline))))
               ;; Old message types
               (when (pair? msg)
                 (case (car msg)
                   ((enable-irq)
                    (let ((irq (cadr msg)))
                      (pic-enable irq)
                      (cond ((fx<? irq 8)
                             (vector-set! *irq-vectors*
                                          (+ PIC1-vector-offset irq)
                                          pcb))
                            ((fx<? irq 16)
                             (vector-set! *irq-vectors*
                                          (+ PIC2-vector-offset (fx- irq 8))
                                          pcb)))
                      (pcb-msg-set! pcb 'ok)))
                   ((acknowledge-irq)
                    (let ((irq (cadr msg))
                          (k (caddr msg)))
                      (pic-ack irq)
                      (lp k)))

                   ((new-process)
                    (let* ((pid *next-pid*)
                           (sp ($process-start pid))
                           (npcb (make-pcb pid sp)))
                      ;; (display "new process\n")
                      (set! *runq* (pcb-link! *runq* npcb))
                      (hashtable-set! *pids* pid npcb)
                      (pcb-msg-set! pcb pid)
                      (set! *next-pid* (+ *next-pid* 1))))
                   ((get-pid)
                    (pcb-msg-set! pcb (pcb-pid pcb)))
                   ((exit)
                    ;; XXX: `reason` is a condition
                    ;; object, but using the types from
                    ;; the other process.
                    (let ((target (cadr msg))
                          (reason (caddr msg)))
                      ;; TODO: well...
                      (display "process exited: ")
                      (display (list target reason))
                      (newline)
                      (when (eqv? (pcb-pid pcb) target)
                        ;; XXX: free the resources etc,
                        ;; remove from *irq-vectors*
                        (hashtable-set! *pids* (pcb-pid pcb) #f)
                        (set! *runq* (pcb-unlink! pcb)))))
                   ((boot-loader)
                    (pcb-msg-set! pcb 'multiboot))
                   ((command-line)
                    ;; XXX: command-line and environment
                    ;; are extremely iffy. Process must
                    ;; immediately copy the variables to
                    ;; its own storage.
                    (pcb-msg-set! pcb (command-line)))
                   ((environment)
                    (pcb-msg-set! pcb (get-environment-variables)))
                   (else
                    (display "bad message: ")
                    (write msg)
                    (newline)))))

             (when *runq*
               ;; TODO: it would be better to always
               ;; have a process in *runq*, and let that
               ;; process be the "idle" process.
               (set! *runq* (pcb-next *runq*)))
             (loop))))))

;;; Multiboot initialization

(define (pc-init)
  ;; XXX: print is just for debugging
  (define (print . x) (for-each display x) (newline))
  (define (mem64 a)
    ;; Read a u64 from memory
    (bitwise-ior ($get-mem-u32 a)
                 (bitwise-arithmetic-shift-left ($get-mem-u32 (fx+ a 4)) 32)))
  (define (fmt-quantums x)
    ;; Converts x (bytes) into quantums, with a suffix. A quantum is
    ;; Lisp data, such as a fixnum or a pointer to something.
    ;; Defined in "Address/Memory Management For A Gigantic Lisp
    ;; Environment or, GC Considered Harmful", Jon L White, 1980.
    (define k (* 1024))
    (define M (* k 1024))
    (define G (* M 1024))
    (define T (* G 1024))
    (let ((x (div x 8)))
      (cond ((> x (* 10 T)) (string-append (number->string (div x T)) "TQ"))
            ((> x (* 10 G)) (string-append (number->string (div x G)) "GQ"))
            ((> x (* 10 M)) (string-append (number->string (div x M)) "MQ"))
            ((> x (* 10 k)) (string-append (number->string (div x k)) "kQ"))
            (else (string-append (number->string x) "Q")))))
  ;; Multiboot information structure
  (define flag-mem              #b000000000001)
  (define flag-boot-device      #b000000000010)
  (define flag-cmdline          #b000000000100)
  (define flag-mods             #b000000001000)
  (define flag-syms-a.out       #b000000010000)
  (define flag-syms-elf         #b000000100000)
  (define flag-mmap             #b000001000000)
  (define flag-drives           #b000010000000)
  (define flag-config-table     #b000100000000)
  (define flag-boot-loader-name #b001000000000)
  (define flag-apm-table        #b010000000000)
  (define flag-vbe              #b100000000000)
  (define field-flags            0)
  (define field-memory-lower     1)
  (define field-memory-upper     2)
  (define field-boot-device      3)
  (define field-cmdline          4)
  (define field-mods-count       5)
  (define field-mods-addr        6)
  (define field-elf-sec-num      7)
  (define field-elf-sec-size     8)
  (define field-elf-sec-addr     9)
  (define field-elf-sec-shndx    10)
  (define field-mmap-length      11)
  (define field-mmap-addr        12)
  (define field-drives-length    13)
  (define field-drives-addr      14)
  (define field-config-table     15)
  (define field-boot-loader-name 16)
  (define field-apm-table        17)
  (define mbi-loc ($boot-loader-data))
  (define (mbi-ref i)
    (assert (fx<=? i 21))
    ($get-mem-u32 (fx+ (fx* i 4) mbi-loc)))
  (define flags (mbi-ref field-flags))
  (define (set? flag) (not (fxzero? (fxand flag flags))))

  (define (fmt-addr x) (and x (number->string x 16)))
  (define (area-base x) (car x))
  (define (area-top x) (cadr x))
  (define (area-info x) (caddr x))
  (define (area-type x) (info-type (area-info x)))
  (define (set-area-base! x y) (set-car! x y))
  (define (info-start x) (car x))
  (define (info-length x) (cadr x))
  (define (info-type x) (caddr x))
  (define (print-area x)
    (let ((i (area-info x)))
      (print " [" (fmt-addr (area-base x)) "," (fmt-addr (area-top x)) ") "
             ;; "(" (fxarithmetic-shift-right (fx- (area-top x) (area-base x)) 12) " pages) "
             (fmt-quantums (fx- (area-top x) (area-base x)))
             " {" (fmt-addr (info-start i)) "+" (fmt-addr (info-length i)) "}: "
             (info-type i))))

  (define areas '())
  (define (mark-area start len type)
    (define (page-align-down x) (fxand x -4096))
    (define (page-align-up x) (fxand (fx+ x 4095) -4096))
    ;; The types `usable' and `nothing' are special and everything
    ;; else means the memory is not available for allocation. An area
    ;; marked `nothing' will have a lower priority than anything, and
    ;; `usable' will have lower priority than everything else.
    (let ((base (if (eq? type 'usable)
                    (page-align-up start)
                    (page-align-down start)))
          (top (if (eq? type 'usable)
                   (page-align-down (fx+ start len))
                   (page-align-up (fx+ start len)))))
      (let ((area (list base top (list start len type))))
        ;; (print-area area)
        (set! areas (cons area areas)))))

  (define (find-usable a)
    ;; Find the usable areas and remove those parts that have been
    ;; marked as unusable. Stupid but simple algorithm.
    (define (fix-overlaps usable reserved)
      (if (null? usable)
          '()
          (let ((U (car usable)))
            (let lp ((res reserved))
              (if (null? res)
                  (cons U (fix-overlaps (cdr usable) reserved))
                  (let ((R (car res)))
                    (cond ((and (fx<=? (area-base R) (area-base U))
                                (fx>=? (area-top R) (area-top U)))
                           ;; if R completely overlaps U, remove U
                           (fix-overlaps (cdr usable) reserved))
                          ((and (fx<=? (area-base R) (area-base U))
                                (fx>? (area-top R) (area-base U)))
                           ;; if R overlaps the start of U, remove
                           ;; that part of U
                           (cons (list (area-top R) (area-top U) (area-info U))
                                 (fix-overlaps (cdr usable) reserved)))
                          ((and (fx<? (area-base R) (area-top U))
                                (fx>=? (area-base R) (area-top U)))
                           ;; if R overlaps the end of U, remove that
                           ;; part of U
                           (cons (list (area-base U) (area-base R) (area-info U))
                                 (fix-overlaps (cdr usable) reserved)))
                          ((and (fx>=? (area-base R) (area-base U))
                                (fx<=? (area-top R) (area-top U)))
                           ;; if R overlaps the middle of U, remove
                           ;; that part of U
                           (cons (list (area-base U) (area-base R) (area-info U))
                                 (cons (list (area-top R) (area-top U) (area-info U))
                                       (fix-overlaps (cdr usable) reserved))))
                          (else
                           ;; no overlap, try the next reserved area
                           (lp (cdr res))))))))))
    (define (split a usable reserved)
      (cond ((null? a)
             (let lp ((usable usable))
               (let ((usable* (fix-overlaps usable reserved)))
                 (if (equal? usable usable*)
                     usable
                     (lp usable*)))))
            ((eq? (area-type (car a)) 'usable)
             (split (cdr a) (cons (car a) usable) reserved))
            (else
             (split (cdr a) usable (cons (car a) reserved)))))
    (split a '() '()))

;;; Paging

  ;; Page allocation. Long mode page tables with PAT only. All
  ;; tables are 4096 bytes and contain 64-bit entries. CR3 points to
  ;; the PML4, which point to PDPE's, which point to 4-Kbyte PDE's
  ;; or 2-Mbyte PDE's. A 4-Kbyte PDE points to a 4-Kbyte PTE. The
  ;; PML4E may also point to a 1-Gbyte PDPE. Bit 0 always means
  ;; "present".
  (define (get-cr3)
    ;; Doesn't really return CR3...
    ($linker-address 'pml4))
  (define cr3 (get-cr3))
  (define (get-pml4t)
    (fxand cr3 (fxnot (- (expt 2 12) 2))))
  (define supports-1G-pages
    (let-values (((a b c d) (cpuid #x80000001)))
      (fxbit-set? d 26)))
  ;; Flags available in all levels of the page table hierarchy
  (define page-P   0)                 ;present
  (define page-R/W 1)                 ;read/write
  (define page-U/S 2)                 ;user/supervisor
  (define page-PWT 4)                 ;page-level writethrough(*)
  (define page-PCD 4)                 ;page-level cache disable(*)
  (define page-A   5)                 ;accessed
  (define page-NX  63)                ;no-execute
  ;; Flags available only in the lowest level of the page table
  (define page-D       6)             ;dirty
  (define pte-PAT      7)             ;page-attribute table(*)
  (define pte-G        8)             ;global
  (define pde/pdpe-PAT 12)            ;page-attribute table(*)
  ;; Flags available only at the PDE level, and the PDPE level if
  ;; support for 1GB pages is available.
  (define page-PS 7)       ;page size
  ;; Bit field offsets in virtual addresses
  (define PHYSICAL-SIGN 52)           ;XXX:
  (define VIRTUAL-SIGN 48)            ;XXX:
  (define VIRTUAL-PML4 39)
  (define VIRTUAL-PDP 30)
  (define VIRTUAL-PD 21)
  (define VIRTUAL-PT 12)
  ;; Very nice handy stuff.
  (define (entry-present? x) (bitwise-bit-set? x page-P))
  (define (entry-r/w? x) (bitwise-bit-set? x page-R/W))
  (define (entry-u/s? x) (bitwise-bit-set? x page-U/S))
  (define (entry-accessed? x) (bitwise-bit-set? x page-A))
  (define (entry-nx? x) (bitwise-bit-set? x page-NX))
  (define (pml4e-address x)
    (bitwise-and x (bitwise-and (fxnot (- (expt 2 12) 1))
                                (- (expt 2 52) 1))))
  (define (pdpe-1G? x) (bitwise-bit-set? x page-PS))
  (define (pdpe-1G-address x)
    (bitwise-and x (bitwise-and (fxnot (- (expt 2 30) 1))
                                (- (expt 2 52) 1))))
  (define pdpe-address pml4e-address)
  (define pde-2M? pdpe-1G?)
  (define pde-4K-address pml4e-address)
  (define (pde-2M-address x)
    (bitwise-and x (bitwise-and (fxnot (- (expt 2 20) 1))
                                (- (expt 2 52) 1))))
  (define (pde-2M-pat x)
    (fxior (if (bitwise-bit-set? x pde/pdpe-PAT) #b100 0)
           (if (bitwise-bit-set? x page-PCD) #b10 0)
           (if (bitwise-bit-set? x page-PWT) #b1 0)))
  (define pte-address pml4e-address)
  (define (pte-pat x)
    (fxior (if (bitwise-bit-set? x pte-PAT) #b100 0)
           (if (bitwise-bit-set? x page-PCD) #b10 0)
           (if (bitwise-bit-set? x page-PWT) #b1 0)))
  (define (pflags e . fs)
    (let lp ((fs fs))
      (if (null? fs)
          '()
          (let ((bit (car fs)) (name (cadr fs)) (fs (cddr fs)))
            (if (bitwise-bit-set? e bit)
                (cons name (lp fs))
                (lp fs))))))
  (define (pte-flags e)
    (cons (list 'PAT (pte-pat e))
          (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
                  page-D 'D pte-G 'G page-NX 'NX)))
  (define (pde-2M-flags e)
    (cons (list 'PAT (pde-2M-pat e))
          (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
                  page-D 'D pte-G 'G page-NX 'NX)))
  (define (pde-4K-flags e)
    (pflags e page-P 'P page-R/W 'R/W page-U/S 'U/S page-A 'A
            page-NX 'NX))
  (define pdpe-flags pde-4K-flags)
  (define pml4e-flags pde-4K-flags)
  (define (vaddr-pml4 addr)
    (fxand (bitwise-arithmetic-shift-right addr VIRTUAL-PML4)
           (- (expt 2 (- 64 VIRTUAL-SIGN)) 1)))
  (define (vaddr-pdp addr)
    (fxbit-field addr VIRTUAL-PDP VIRTUAL-PML4))
  (define (vaddr-pd addr)
    (fxbit-field addr VIRTUAL-PD VIRTUAL-PDP))
  (define (vaddr-pt addr)
    (fxbit-field addr VIRTUAL-PT VIRTUAL-PD))
  (define (table-for-each proc table-base)
    ;; Run proc on each of the entries in a descriptor table.
    (do ((i 0 (fx+ i 8)))
        ((fx=? i 4096))
      (let ((e (mem64 (fx+ table-base i))))
        (when (entry-present? e)
          (proc e (fxarithmetic-shift-right i 3))))))
  (define (print-full-table)
    (print "Full page table follows.")
    (print "CR3: #x" (fmt-addr (get-cr3))
           #\tab (pflags (get-cr3) page-PWT 'PWT page-PCD 'PCD))
    (print "Page-Map Level-4 Table:")
    (table-for-each
     (lambda (pml4e i)
       ;; TODO: sign-extension
       (print-pml4e pml4e (fxarithmetic-shift-left i VIRTUAL-PML4)))
     (get-pml4t)))
  (define (print-pml4e e base)
    (print " PML4E table base #x" (fmt-addr base) " entry at #x"
           (fmt-addr (pml4e-address e)) #\tab (pml4e-flags e))
    (table-for-each
     (lambda (pdpe i)
       (print-pdpe pdpe (fxior base (fxarithmetic-shift-left i VIRTUAL-PDP))))
     (pml4e-address e)))
  (define (print-pdpe e base)
    (cond ((pdpe-1G? e)
           (print "  PDPE 1G page #x" (fmt-addr base) " => #x"
                  (fmt-addr (pdpe-1G-address e))))
          (else
           (print "  PDPE table base #x" (fmt-addr base)
                  " entry at #x" (fmt-addr (pdpe-address e))
                  #\tab (pdpe-flags e))
           (table-for-each
            (lambda (pde i)
              (print-pde pde (fxior base (fxarithmetic-shift-left i VIRTUAL-PD))))
            (pdpe-address e))
           (newline))))
  (define (print-pde e base)
    (cond ((pde-2M? e)
           (display #\.)
           #;(print "   PDE 2M page #x" (fmt-addr base) " => #x"
                    (fmt-addr (pde-2M-address e)) #\tab (pde-2M-flags e)))
          (else
           (print "   PDE 4K table base #x" (fmt-addr base) " entry at #x"
                  (fmt-addr (pde-4K-address e)) #\tab (pde-4K-flags e))
           (table-for-each
            (lambda (pte i)
              (print-pte pte (fxior base (fxarithmetic-shift-left i VIRTUAL-PT))))
            (pde-4K-address e))
           (newline))))
  (define (print-pte e base)
    (print "    PTE page #x" (fmt-addr base) " => #x"
           (fmt-addr (pte-address e)) #\tab (pte-flags e)))

  ;; Returns the physical address that the virtual address is mapped
  ;; to, or #f if it is not mapped.
  (define (virtual->physical addr)
    (define (print . _) #f)
    (assert (= VIRTUAL-SIGN 48))
    (let ((pml4 (vaddr-pml4 addr))
          (rest (bitwise-and addr (- (expt 2 VIRTUAL-SIGN) 1))))
      (let ((pdp (vaddr-pdp rest)) (pd (vaddr-pd rest)) (pt (vaddr-pt rest)))
        (print "Fields: "  (list pml4 pdp pd pt))
        (let ((pml4e (mem64 (fx+ (get-pml4t) (fx* pml4 8)))))
          (print "pml4e: " pml4e)
          (and (entry-present? pml4e)
               (let ((pdpe (mem64 (fx+ (pml4e-address pml4e) (fx* pdp 8)))))
                 (assert (not (pdpe-1G? pdpe)))
                 (print "pdpe: " pdpe)
                 (if (pdpe-1G? pdpe)
                     (fxior (fxand addr (- (expt 2 VIRTUAL-PDP) 1))
                            (pdpe-1G-address pdpe))
                     (and (entry-present? pdpe)
                          (let ((pde (mem64 (fx+ (pdpe-address pdpe) (fx* pd 8)))))
                            (print "pde: " pde)
                            (and (entry-present? pde)
                                 (if (pde-2M? pde)
                                     (fxior (fxand addr (- (expt 2 VIRTUAL-PD) 1))
                                            (pde-2M-address pde))
                                     (let ((pte (mem64 (fx+ (pde-4K-address pde)
                                                            (fx* pt 8)))))
                                       (print "pt: " pte " " (pte-address pte))
                                       (and (entry-present? pte)
                                            (fxior (fxand addr (- (expt 2 VIRTUAL-PT) 1))
                                                   (pte-address pte)))))))))))))))

  (define *available* '())
  (define (get-4K-zero-page)
    ;; XXX: this must return identity-mapped memory because it's used
    ;; to populate the page tables. FIXME: This eats up all the low
    ;; memory.
    (if (null? *available*)
        (error 'get-4K-zero-page "TODO: no available pages")
        (let* ((a (car *available*))
               (page (area-base a))
               (nbase (fx+ page 4096)))
          (if (fx=? nbase (area-top a))
              (set! *available* (cdr *available*))
              (set-area-base! a nbase))
          ;; TODO: the page should have been cleared already
          (clear-page page)
          page)))
  ;; Create virtual memory mapping for the addresses from start to
  ;; start+len-1. Does not remove/overwrite existing mappings.
  (define (longmode-mmap start* len type)
    ;; TODO: identity mappings should be 1GB or 2MB pages.
    (define (print . _) #f)
    (define (set-mem64! a v)
      ($put-mem-u32 a (bitwise-and v #xffffffff))
      ($put-mem-u32 (fx+ a 4) (bitwise-arithmetic-shift-right v 32)))
    (define (page-align-down x) (fxand x -4096))
    (define (page-align-up x) (fxand (fx+ x 4095) -4096))
    (define start (page-align-down start*))
    (define end (page-align-up (fx+ start len)))
    (define attributes (fxior (expt 2 page-P)
                              (expt 2 page-R/W)
                              (expt 2 page-U/S)))
    (assert (memq type '(stack heap code identity)))
    (assert (fxzero? (fxbit-field start 0 VIRTUAL-PT)))
    (assert (fxpositive? len))
    (assert (= VIRTUAL-SIGN 48))
    (when (eq? type 'identity)
      ;; This is needed because of where thread areas are placed.
      (assert (< end (* 1024 1024 1024 1024))))
    (do ((addr start (fx+ addr 4096)))
        ((fx=? addr end))
      (let ((pml4 (vaddr-pml4 addr))
            (rest (bitwise-and addr (- (expt 2 VIRTUAL-SIGN) 1))))
        (let ((pdp (vaddr-pdp rest)) (pd (vaddr-pd rest)) (pt (vaddr-pt rest)))
          ;; Populate the table.
          (print "Populate: " (list pml4 pdp pd pt))
          (let ((pml4e@ (fx+ (get-pml4t)
                             (fx* pml4 8))))
            ;; Add a PML4E
            (when (not (entry-present? ($get-mem-u32 pml4e@)))
              (print "Adding a page to PML4T")
              (set-mem64! pml4e@ (bitwise-ior (get-4K-zero-page) attributes)))
            (let ((pdpe@ (fx+ (pml4e-address (mem64 pml4e@))
                              (fx* pdp 8))))
              ;; Add a PDPE
              (when (not (entry-present? ($get-mem-u32 pdpe@)))
                (print "Adding a page to PDPT")
                (set-mem64! pdpe@ (bitwise-ior (get-4K-zero-page) attributes)))
              (let ((pdpe (mem64 pdpe@)))
                (unless (pdpe-1G? pdpe)
                  (let ((pde@ (fx+ (pdpe-address pdpe)
                                   (fx* pd 8))))
                    ;; Add a PDE
                    (when (not (entry-present? ($get-mem-u32 pde@)))
                      (print "Adding a page to PDT")
                      (set-mem64! pde@ (bitwise-ior (get-4K-zero-page)
                                                    attributes)))
                    (let ((pde (mem64 pde@)))
                      (unless (pde-2M? pde)
                        (let ((pte@ (fx+ (pde-4K-address pde)
                                         (fx* pt 8))))
                          ;; Add a PTE
                          (when (not (entry-present? ($get-mem-u32 pte@)))
                            (print "Adding a PTE")
                            (let ((address
                                   (if (eq? type 'identity)
                                       addr
                                       (get-4K-zero-page))))
                              (set-mem64! pte@ (bitwise-ior address
                                                            attributes))))))))))))))))

  (define (longmode-unmap addr len)
    ;; TODO: unmap and invalidate tlb
    #f)

  (define (dma-allocate size mask)
    ;; TODO: more than one page, natural alignment, move other pages
    ;; if none are free, use non-identity mapped pages, have
    ;; get-4K-zero-page allocate >4GB pages before using low pages.
    ;; FIXME: This only looks at the first area.
    (assert (eqv? size 4096))
    (print "DMA allocate: " (list 'size size 'mask mask '*available* *available*))
    (if (null? *available*)
        (values #f #f)
        (let* ((a (car *available*))
               (page (area-base a))
               (nbase (fx+ page 4096)))
          (cond
            ((fx=? (fxand page mask) page)
             (if (fx=? nbase (area-top a))
                 (set! *available* (cdr *available*))
                 (set-area-base! a nbase))
             ;; TODO: the page should have been cleared already
             (clear-page page)
             ;; (print "dma allocated " (fmt-addr page))
             (longmode-mmap page 4096 'identity)
             ;; XXX: I got lazy and these two values now have to be
             ;; the same.
             (values page page))
            (else
             ;; (print "no physical pages with the requested mask are available")
             (values #f #f))))))

  ;; (print-full-table)

;;; The fun starts here

  (init-early-serial-driver)
  (check-cpu)
  (check-apic-base)
  (print " ┃  ┏━┃┃ ┃┏━┃  ┏━┛┏━┛┃ ┃┏━┛┏┏ ┏━┛")
  (print " ┃  ┃ ┃┏┛ ┃ ┃  ━━┃┃  ┏━┃┏━┛┃┃┃┏━┛")
  (print " ━━┛━━┛┛ ┛━━┛  ━━┛━━┛┛ ┛━━┛┛┛┛━━┛")
  (init-set! '$mmap longmode-mmap)
  (init-set! 'exit
             (lambda (status)
               (display "Loko has stopped running. Sorry about that.\nExit status: "
                        (current-error-port))
               (write status (current-error-port)) (newline (current-error-port))
               (let lp ()
                 (sys_hlt)
                 (lp))))
  (init-set! 'machine-type '#(amd64 loko))

  ;; Check that #AC works
  (guard (exn (else #f))
    ($get-mem-u32 #x200001)           ;safe way to trigger #AC
    (display "Fatal: Loko can't run because the system does not support #AC.\n")
    (exit 70))

  ;; ;; Copy the BIOS data area.
  ;; (do ((bv (make-bytevector #x100))
  ;;      (addr #x400 (fx+ addr 4))
  ;;      (i 0 (fx+ i 4)))
  ;;     ((fx=? i #x100)
  ;;      (set! *modules*
  ;;            (cons (cons 'bios-data (open-bytevector-input-port bv))
  ;;                  *modules*)))
  ;;   (bytevector-u32-native-set! bv i ($get-mem-u32 addr)))

  (when (set? flag-cmdline)
    (let-values ([(env cmdline) (pc-init-parse-command-line
                                 (utf8->string (copy-utf8z (mbi-ref field-cmdline))))])
      (init-set! 'environment-variables env)
      (init-set! 'command-line cmdline)))
  (when (set? flag-boot-loader-name)
    (init-set! 'environment-variables
               (cons (cons "BOOT_LOADER_NAME"
                           (utf8->string (copy-utf8z (mbi-ref field-boot-loader-name))))
                     (get-environment-variables))))
  ;; Parse the memory map
  (unless (set? flag-mmap)
    (error 'system "The boot-loader did not pass a memory map"))
  ;; Find all usable memory.
  (let* ((len (mbi-ref field-mmap-length))
         (start (mbi-ref field-mmap-addr))
         (end (fx+ start len)))
    (define (size addr) ($get-mem-u32 addr))
    (define (base addr)
      ;; XXX: maybe bitwise-operators instead.
      (fxior ($get-mem-u32 addr)
             (fxarithmetic-shift-left ($get-mem-u32 (fx+ addr 4)) 32)))
    (define (len addr)
      (fxior ($get-mem-u32 (fx+ addr 8))
             (fxarithmetic-shift-left ($get-mem-u32 (fx+ addr 12)) 32)))
    (define (type addr) ($get-mem-u32 (fx+ addr 16)))
    (do ((addr start (fx+ addr (fx+ (size addr) 4))))
        ((fx>=? addr end))
      (let* ((saddr (fx+ addr 4))
             (type (type saddr)))
        (mark-area (base saddr) (len saddr)
                   (case type
                     ((1) 'usable) ((2) 'reserved)
                     ((3) 'acpi-data) ((4) 'acpi-nvs)
                     (else (cons 'e820 type)))))))
  ;; Mark the kernel's own memory
  (let ((load-addr ($linker-address 'image-address-zero))
        (bss-end-addr ($linker-address 'bss-end)))
    (mark-area load-addr (fx- bss-end-addr load-addr) 'kernel))

  ;; Modules contain Scheme code. These will be compiled later on,
  ;; so the memory must be marked. All modules are page-aligned.
  (when (set? flag-mods)
    (do ((mods (mbi-ref field-mods-count))
         (addr (mbi-ref field-mods-addr) (fx+ addr 16))
         (i 0 (fx+ i 1)))
        ((fx=? i mods))
      (let ((start ($get-mem-u32 addr))
            (end ($get-mem-u32 (fx+ addr 4)))
            (str (utf8->string (copy-utf8z ($get-mem-u32 (fx+ addr 8))))))
        (mark-area start (fx- end start) (cons 'module str))
        ;; TODO: free the pages used by the modules after using them
        (set! *modules*
              (cons (cons str (open-memory-input-port str start (fx- end start)))
                    *modules*)))))

  ;; BIOS data area + some extra for good measure, VGA, EBDA, ROMs.
  (mark-area 0 #x3000 'bios)
  (mark-area #xA0000 #x20000 'vga)
  (mark-area #xC0000 #x40000 'bios)

  ;; Alright... at this point all reserved memory has been marked as
  ;; such. The multiboot info will be clobbered and all available
  ;; RAM will be identity-mapped.
  (print "RAM areas:")
  (for-each print-area
            (list-sort (lambda (a b)
                         (< (area-base a) (area-base b)))
                       areas))
  (let ((usable (find-usable areas)))
    (print "Usable areas:")
    (for-each print-area usable)
    (set! *available* usable)
    (print "Usable RAM: "
           (fmt-quantums
            (fold-left (lambda (acc area)
                         (+ acc (- (area-top area) (area-base area))))
                       0 usable)))
    (for-each (lambda (base top)
                ;; Only the first 4GB is identity-mapped initially.
                (when (or (> base (* 4 1024 1024 1024))
                          (> top (* 4 1024 1024 1024)))
                  (longmode-mmap base (fx- top base) 'identity)))
              (map area-base usable)
              (map area-top usable)))

  ;; A null page might catch some bugs.
  ;; (longmode-unmap 0 #x1000)

  ;; Initialize the PICs and mask all IRQs. This is running with
  ;; interrupts disabled, FWIW. This must be done before APs are
  ;; booted. Some systems route the PIC interrupts to an AP
  ;; processor, and without this initialization the AP will get
  ;; interrupts at the same vectors BIOS uses (e.g. IRQ0 = #DF's
  ;; vector).
  (pic-init picm PIC1-vector-offset)
  (pic-init pics PIC2-vector-offset)
  (pic-mask picm #b11111111)
  (pic-mask pics #b11111111)

  ;; Initialize APIC
  (calibrate-APIC&CPU-against-PIT)
  (write-timer-initial-count 0) ;stop timer
  ($put-mem-u32 APIC:task-priority 32)
  ($put-mem-u32 APIC:logical-destination 0)
  ($put-mem-u32 APIC:destination-format #xf0000000)
  (write-timer-vector LVT-MASK)
  ($put-mem-u32 APIC:thermal-vector LVT-MASK)
  ($put-mem-u32 APIC:performance-vector LVT-MASK)
  ($put-mem-u32 APIC:LINT0-vector (fxior LVT-MT-EXTERNAL LVT-TGM))
  ($put-mem-u32 APIC:LINT1-vector LVT-MT-NMI)
  ($put-mem-u32 APIC:error-vector LVT-MASK)
  ($put-mem-u32 APIC:ICR-high 0)
  (write-timer-divide apic-divisor)

  ;; Enable the APIC and disable anything not supported.
  (write-msr APIC-MSR (bitwise-ior ABA (expt 2 APIC-base-reg:AE)))
  (write-spurious-int 'enable APIC-vector-spurious)

  ;; Calibrate the timer against the i8253/i8254 PIT and set
  ;; the local APIC timer to fire periodically at 100 Hz.
  (let-values ([(bus-freq cpu-freq) (calibrate-APIC&CPU-against-PIT)])
    (let* ((scheduler-frequency 1/100)
           (interval (max (div bus-freq (* apic-divisor (/ scheduler-frequency)))
                          100)))
      (print "CPU frequency: " cpu-freq " Hz")
      (print "APIC frequency: " bus-freq " Hz")
      (print "Timer set to " interval)
      (print "Preemption frequency: " (/ scheduler-frequency) " Hz")
      (print "Hardware uptime: " (div (rdtsc) cpu-freq) " seconds")
      (write-timer-initial-count interval)
      (write-timer-vector (fxior APIC-vector-timer LVT-TMM))
      (write-timer-divide apic-divisor)

      (print "Booting application processors...")
      (let-values ([(&cpu &dma) (dma-allocate 4096 #x000ff000)])
        (define (busywait seconds)
          (let* ((start (rdtsc))
                 (target (+ start (nanoseconds->TSC cpu-freq (* seconds #e1e9)))))
            (let lp ()
              (let ((current (rdtsc)))
                (unless (and (> current start)
                             (> current target))
                  (lp))))))
        (assert (eqv? &cpu &dma))
        ;; TODO: after this, when all CPUs have booted, free the page
        (print "AP boot page: #x" (and &cpu (number->string &cpu 16)))
        (when &cpu
          (boot-application-processors &cpu APIC:ICR-low busywait))
        (pc-scheduler cpu-freq interval dma-allocate)))))

(when (eq? ($boot-loader-type) 'multiboot)
  (init-set! 'init pc-init)))
