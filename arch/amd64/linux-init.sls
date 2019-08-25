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

;;; Linux-specific initialization

;; At this point the standard library has been loaded and should be
;; available.

;; This code runs in pid 0. Fibers are not available here.

(library (loko arch amd64 linux-init)
  (export)
  (import
    (rnrs (6))
    (only (loko init) init-set!)
    (loko system unsafe)
    (loko arch amd64 linux-numbers)
    (loko arch amd64 linux-syscalls)
    (loko arch amd64 processes)
    (only (loko libs io) $init-standard-ports)
    (loko system $host)
    (loko system $primitives)
    (only (loko libs context)
          CPU-VECTOR:ALTSIGSTK-BASE
          CPU-VECTOR:ALTSIGSTK-SIZE
          CPU-VECTOR:SCHEDULER-SP)
    (srfi :98 os-environment-variables))

;; Less general than the libc counterpart
(define (timer-create clock-id signal)
  (let ((evp (make-bytevector sizeof-sigevent))
        (timer-id (make-bytevector 8 0)))
    (bytevector-u32-native-set! evp offsetof-sigevent-sigev_signo signal)
    (bytevector-u32-native-set! evp offsetof-sigevent-sigev_notify SIGEV_SIGNAL)
    (let nuts ()
      (unless (sys_timer_create clock-id
                                (bytevector-address evp)
                                (bytevector-address timer-id)
                                (lambda (errno)
                                  (if (or (eqv? errno EAGAIN)
                                          (eqv? errno EINTR))
                                      #f
                                      (raise
                                        (make-syscall-error 'timer_create errno)))))
        (nuts)))
    (bytevector-u64-native-ref timer-id 0)))

;; Less general than the libc counterpart
(define (timer-settime timer seconds nanoseconds)
  (let ((itimerspec (make-bytevector sizeof-itimerspec))
        (flags 0)
        (NULL 0))
    ;; FIXME: better code for the offsets
    (bytevector-u64-native-set! itimerspec #x00 seconds)
    (bytevector-u64-native-set! itimerspec #x08 nanoseconds)
    (bytevector-u64-native-set! itimerspec #x10 seconds)
    (bytevector-u64-native-set! itimerspec #x18 nanoseconds)
    (sys_timer_settime timer flags (bytevector-address itimerspec)
                       NULL)))

(define (linux-init-signal-handlers)
  (define NULL 0)
  (define put! bytevector-u64-native-set!)
  (define (rt_sigaction signal act)
    (sys_rt_sigaction signal (bytevector-address act) NULL sizeof-sigset_t))
  (define (rt_sigprocmask how set)
    (let ((buf (make-bytevector sizeof-sigset_t)))
      (bytevector-u64-native-set! buf 0 set)
      (sys_rt_sigprocmask how (bytevector-address buf) NULL (bytevector-length buf))))
  ;; SIGPIPE is not needed because the syscalls that would raise it
  ;; will instead get errors that are handled properly.
  (rt_sigprocmask SIG_BLOCK (fxarithmetic-shift-left 1 (- SIGPIPE 1)))
  (let ((act (make-bytevector sizeof-sigaction)))
    ;; TODO: SIGWINCH via signalfd
    (put! act offsetof-sigaction-sa_handler ($linker-address 'signal-handler))
    (put! act offsetof-sigaction-sa_flags
          (bitwise-ior SA_SIGINFO SA_NODEFER SA_RESTORER))
    (put! act offsetof-sigaction-sa_restorer ($linker-address 'signal-return))
    (rt_sigaction SIGBUS act)
    (rt_sigaction SIGSEGV act)
    (rt_sigaction SIGFPE act)
    ;; (rt_sigaction SIGTRAP act)
    (rt_sigaction SIGILL act)
    (let ((ss (make-bytevector sizeof-sigaltstack)))
      ;; SIGURG runs on the alternate signal stack
      (put! ss offsetof-sigaltstack-ss_sp ($processor-data-ref CPU-VECTOR:ALTSIGSTK-BASE))
      (put! ss offsetof-sigaltstack-ss_size ($processor-data-ref CPU-VECTOR:ALTSIGSTK-SIZE))
      (sys_sigaltstack (bytevector-address ss) NULL))
    ;; SIGURG is for preemption.
    ;; TODO: SIGSEGV must have the alternate stack, too. (stack overflow)
    (put! act offsetof-sigaction-sa_handler ($linker-address 'linux:preempt))
    (put! act offsetof-sigaction-sa_flags
          (bitwise-ior SA_SIGINFO SA_RESTORER SA_RESTART SA_ONSTACK))
    (put! act offsetof-sigaction-sa_restorer ($linker-address 'signal-return))
    (rt_sigaction SIGURG act)))

;; Initialize the terminal
(define (linux-init-terminal)
  ($init-standard-ports (lambda (bv start count)
                          (assert (fx<=? (fx+ start count) (bytevector-length bv)))
                          (sys_read STDIN_FILENO (fx+ (bytevector-address bv) start) count))
                        (lambda (bv start count)
                          (assert (fx<=? (fx+ start count)
                                         (bytevector-length bv)))
                          (sys_write STDOUT_FILENO (fx+ (bytevector-address bv) start) count))
                        (lambda (bv start count)
                          (assert (fx<=? (fx+ start count)
                                         (bytevector-length bv)))
                          (sys_write STDERR_FILENO (fx+ (bytevector-address bv) start) count))
                        (eol-style lf)))

;; Verify that #AC is working and triggers SIGBUG
(define (linux-init-check-alignment)
  (guard (exn (else #f))
    (get-mem-u32 #x200001)              ;safe way to trigger #AC
    (cond
      ((eqv? (valgrind #x1001) 0)
       (display "Fatal: Loko can't run because the system does not support #AC.\n"
                (current-error-port))
       (exit 70))
      (else
       (display "Warning: Valgrind does not emulate #AC, expect trouble!\n"
                (current-error-port))))))

;; Handle data placed on the stack by Linux exec
(define (linux-init-process-data)
  (define field-argc 0)
  (define field-argv 1)
  (define stk-loc ($boot-loader-data))
  (define (copy-utf8z addr)
    (do ((end addr (fx+ end 1))
         (len 0 (fx+ len 1)))
        ((fxzero? (get-mem-u8 end))
         (do ((ret (make-bytevector len))
              (i (fx- end 1) (fx- i 1))
              (len (fx- len 1) (fx- len 1)))
             ((fx<=? len -1) ret)
           (bytevector-u8-set! ret len (get-mem-u8 i))))))
  (define (string-index s c)
    (let lp ((i 0))
      (and (not (fx=? i (string-length s)))
           (if (eqv? c (string-ref s i))
               i
               (lp (fx+ i 1))))))
  (define (stk-ref i) (get-mem-s61 (fx+ stk-loc (fx* i 8))))
  (let ((argc (stk-ref field-argc)))
    ;; Parse command line
    (do ((i (fx- argc 1) (fx- i 1))
         (cmdline '() (cons (utf8->string
                             (copy-utf8z (stk-ref (fx+ field-argv i))))
                            cmdline)))
        ((fx=? i -1)
         (init-set! 'command-line cmdline)))
    ;; Parse environment
    (do ((envp (fx+ argc 2) (fx+ envp 1))
         (env '() (cons (let ((str (utf8->string (copy-utf8z (stk-ref envp)))))
                          (let ((idx (string-index str #\=)))
                            (if (not idx)
                                (cons str "") ;can happen
                                (cons (substring str 0 idx)
                                      (substring str (fx+ idx 1) (string-length str))))))
                        env)))
        ((fxzero? (stk-ref envp))
         (init-set! 'environment-variables env)
         ;; Parse ELF auxiliary vector. XXX: AT_RANDOM is interesting.
         ;; Maybe the UID/GID stuff, too. AT_SYSINFO* points to the
         ;; syscall page!
         (do ((auxp (fx+ envp 1) (fx+ auxp 2))
              (auxv '() (cons (cons (stk-ref auxp) (stk-ref (fx+ auxp 1)))
                              auxv)))
             ((fxzero? (stk-ref auxp))
              (init-set! 'auxiliary-vector auxv)
              ;; (command-line) must not return empty, so use the
              ;; executable's filename if the parent process didn't
              ;; supply any arguments.
              (cond
                ((and (eqv? argc 0) (assv AT_EXECFN auxv)) =>
                 (lambda (execfn)
                   (init-set! 'command-line
                              (list (utf8->string (copy-utf8z (cdr execfn))))))))))))))

;; Setup for AFL, American Fuzzy Lop.
(define (linux-init-setup-afl)
  (define (afl-fork-server)
    (define FORKSRV_FD 198)
    (define SIZE 4)
    (define NULL 0)
    (let ((tmp (make-bytevector SIZE 0)))
      ;; Tells AFL that the fork server has started.
      (when (eqv? (sys_write (+ FORKSRV_FD 1) (bytevector-address tmp) SIZE
                             (lambda (errno) #f))
                  SIZE)
        (let loop ()
          ;; wait
          (unless (eqv? SIZE (sys_read FORKSRV_FD (bytevector-address tmp) SIZE))
            (exit 2))
          (let ((pid (sys_fork)))
            (cond ((eqv? pid 0)
                   ;; Child.
                   (put-mem-s61 ($linker-address 'afl-location) 0)
                   (sys_close FORKSRV_FD)
                   (sys_close (+ FORKSRV_FD 1)))
                  (else
                   (bytevector-u32-native-set! tmp 0 pid)
                   (sys_write (+ FORKSRV_FD 1) (bytevector-address tmp) SIZE)
                   (sys_wait4 pid (bytevector-address tmp) WUNTRACED NULL)
                   (sys_write (+ FORKSRV_FD 1) (bytevector-address tmp) SIZE)
                   (loop))))))))
  ;; Having "__AFL_SHM_ID" as a bytevector is necessary for AFL to
  ;; recognize that the binary is instrumented.
  (define __AFL_SHM_ID (utf8->string #vu8(95 95 65 70 76 95 83 72 77 95 73 68)))
  (let ((id (cond ((get-environment-variable __AFL_SHM_ID)
                   => string->number)
                  (else #f))))
    (when id
      (guard (exn (else #f))
        (sys_shmat id ($linker-address 'afl-map) SHM_REMAP))
      (afl-fork-server))))

;; Preemption timer
(define (linux-init-preemption-timer)
  ;; Create a timer that sends a SIGURG to the current thread at a
  ;; frequency of 50 Hz, but only counts up while the thread is
  ;; running.
  (let ((freq 1/50)
        (timer (timer-create CLOCK_THREAD_CPUTIME_ID SIGURG)))
    ;; The frequency of the SIGURG signals is at most once per
    ;; second.
    (let ((freq (if (>= freq 1) 999/1000 freq)))
      (let ((seconds 0)
            (nanoseconds (* freq (expt 10 9))))
        (timer-settime timer seconds nanoseconds)))))

(define (linux-init)
  (define (linux-mmap start length type)
    (sys_mmap start length
              (case type
                ((stack heap)
                 (fxior PROT_READ PROT_WRITE))
                #;
                ((trap)
                 (mmap-protection 'PROT_NONE))
                ((text)
                 (fxior PROT_READ PROT_WRITE PROT_EXEC))
                (else
                 (error 'mmap "Unsupported type" type)))
              (case type
                ((stack)
                 (fxior MAP_PRIVATE MAP_FIXED MAP_ANONYMOUS))
                ((heap text)
                 (fxior MAP_PRIVATE MAP_FIXED MAP_ANONYMOUS))
                (else
                 (error 'mmap "Unsupported type" type)))
              -1 0)
    (if #f #f))

  (init-set! '$mmap linux-mmap)
  (init-set! 'exit (lambda (status)
                     ;; XXX: status must be a byte?
                     (let lp ()
                       (sys_exit (if (fixnum? status)
                                     status
                                     (if (not status) 1 0)))
                       (lp))))
  (init-set! 'machine-type '#(amd64 linux))
  (linux-init-signal-handlers)
  (linux-init-terminal)
  (linux-init-check-alignment)
  (linux-init-process-data)
  (linux-init-setup-afl)
  (linux-init-preemption-timer)

  ;; TODO: if msg is 'preempted then SIGURG delivery has been
  ;; disabled. When scheduling a process that was preempted there
  ;; is no need to manually unmask SIGURG. But if SIGURG is
  ;; blocked and we're scheduling a process that yielded or that
  ;; has not been started yet it is necessary to unmask SIGURG. If
  ;; SIGURG is unmasked and we're returning to a process that was
  ;; preempted it is likely necessary to mask SIGURG first, to
  ;; prevent a race condition. See sigprocmask.

  ;; TODO: more than one process

  ;; TODO: use epoll here to do the equivalent of IRQs and HLT, to
  ;; allow fiber schedulers to exist in multiple processes

  ;; Pid 0 for Linux
  (let ((sp* ($process-start 1))
        (m* #f)
        (pid* 1)
        (will-unmask?* #f)
        (URG-masked #f))
    (define-syntax print
      (syntax-rules ()
        #;
        ((_ args ...) (begin (display args) ... (newline)))
        ((_ . args) (begin 'dummy))))
    (let lp ((sp* sp*))
      (print "In scheduler! SP=" (number->string sp* 16))
      ;; Switch back to the process
      (let ((msg ($switch-stack sp* m*)))
        (print "Message: " msg)
        (cond ((eq? msg 'preempted)
               ;; The SIGURG signal handler preempted the process,
               ;; so SIGURG is now masked.
               (set! URG-masked #t)
               (set! will-unmask?* #t))
              (else
               ;; The process yielded, which means SIGURG is
               ;; unmasked.
               (set! URG-masked #f)
               (set! will-unmask?* #f)))
        (when (pair? msg)
          (case (car msg)
            ((exit)
             (let ((target (cadr msg))
                   (reason (caddr msg)))
               ;; XXX: Can't use conditions as reason because they are
               ;; generative.
               (exit (if (fixnum? reason) reason (eqv? reason #t)))))
            ((new-process)
             ;; TODO:
             (set! m* #f))
            ((boot-loader)
             (set! m* 'linux))
            ((command-line)
             ;; XXX: command-line and environment are extremely iffy.
             ;; Process must immediately copy the variables to its own
             ;; storage, before we do a GC here.
             (set! m* (command-line)))
            ((environment)
             (set! m* (get-environment-variables)))
            ((get-pid)
             (set! m* pid*)))))
      (let ((sp ($processor-data-ref CPU-VECTOR:SCHEDULER-SP)))
        ($processor-data-set! CPU-VECTOR:SCHEDULER-SP 0)  ;indicate scheduler is running
        (lp sp)))))

(when (eq? ($boot-loader-type) 'linux)
  (init-set! 'init linux-init)))
