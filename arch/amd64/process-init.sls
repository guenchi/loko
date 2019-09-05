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

;;; Process-specific initialization

;; This library is responsible for initializing all processes (except
;; schedulers). These are preemptible Loko processes. It communicates
;; with other processes (and its scheduler) by using $process-yield.

;; Blocking syscalls should be kept out of here.

(library (loko arch amd64 process-init)
  (export
    enable-irq
    acknowledge-irq
    wait-irq-operation)
  (import
    (except (rnrs) command-line exit)
    (rnrs mutable-pairs)
    (srfi :98 os-environment-variables)
    (loko match)
    (loko system unsafe)
    (only (loko init) init-set!)
    (loko arch amd64 processes)
    (only (loko libs io) $init-standard-ports $port-buffer-mode-set!
          port-file-descriptor-set!)
    (only (loko libs time) time-init-set!)
    (loko libs fibers)
    (except (loko system $host) allocate
            enable-irq acknowledge-irq wait-irq-operation)
    (loko system $primitives)
    (loko arch amd64 linux-numbers)
    (loko arch amd64 linux-syscalls)
    ;; Temporarily for the UART driver, which should be using ring buffers
    (pfds queues naive)    )

(define-record-type pid
  (sealed #t) (opaque #f)
  (fields value))

(define (get-command-line)
  (let ((cmdline ($process-yield '(command-line))))
    (assert (list? cmdline))
    (map string-copy cmdline)))

(define (get-environment)
  (let ((env ($process-yield '(environment))))
    (assert (list? env))
    (map (lambda (var)
           (cons (string-copy (car var))
                 (string-copy (cdr var))))
         env)))

(define (get-boot-modules)
  (let ((modules ($process-yield '(boot-modules))))
    (assert (list? modules))
    (map (match-lambda
          [((? string? fn) _args (? fixnum? start) (? fixnum? len))
           (list (string-copy fn) '() start len)])
         modules)))

(define (get-pid)
  (let ((id ($process-yield '(get-pid))))
    (assert (fixnum? id))
    (make-pid id)))

(define (get-boot-loader)
  ($process-yield '(boot-loader)))

(define (new-process)
  (let ((status ($process-yield (vector 'new-process #f))))
    (assert (eq? status 'ok))
    (make-pid (vector-ref status 1))))

(define (process-exit status)
  ($process-yield `(exit ,(pid-value (get-pid))
                         ,status)))

(define (pc-current-ticks)
  ($process-yield '(current-ticks)))

(define (allocate type size mask)
  (assert (eq? type 'dma))
  (assert (fx=? size 4096))
  (assert (fixnum? mask))             ;#xfffff000 is a nice mask
  (let* ((v `#(allocate ,type ,size ,mask #f #f))
         (s ($process-yield v)))
    (unless (eq? s 'ok)
      (error 'allocate "Memory allocation failed"
             type size mask))
    (let ((cpu-addr (vector-ref v 4))
          (dma-addr (vector-ref v 5)))
      (values cpu-addr dma-addr))))

(define (scheduler-wait ns-timeout)
  ;; TODO: this message should take a number back from the scheduler
  ;; that indicates for how long it slept. This is so that if the
  ;; process is awoken by an uninteresting message it can go back to
  ;; sleeping with the original timeout without asking the scheduler
  ;; for the current time.
  (when ns-timeout
    (assert (fxpositive? ns-timeout)))
  (let ((vec (vector 'wait ns-timeout #f)))
    (handle-scheduler-wait-reply vec ($process-yield vec)
                                 'scheduler-wait)))

;;; IRQ support

(define *interrupt-cvars* (make-vector 256 #f))

(define (handle-scheduler-wait-reply vec reply who)
  ;; The scheduler will update vec.
  (case reply
    ((timeout)
     #f)
    ((message)
     (let ((msg (vector-ref vec 2)))
       ;; This is currently just IRQ vector numbers, but can be used to
       ;; implement message passing between processes.
       (cond ((and (fixnum? msg) (vector-ref *interrupt-cvars* msg))
              => signal-cvar!))))
    (else
     (error who "Unknown reply from scheduler" reply))))

(define (enable-irq irq)
  (assert (fx<=? 0 irq 15))
  ($process-yield `#(enable-irq ,irq)))

(define (acknowledge-irq irq)
  (define timeout 0)
  (assert (fx<=? 0 irq 15))
  (when timeout
    (assert (not (fxnegative? timeout))))
  (let ((vec (vector 'wait timeout #f)))
    (vector-set! *interrupt-cvars* irq (make-cvar))
    (handle-scheduler-wait-reply vec ($process-yield `#(acknowledge-irq ,irq ,vec))
                                 'acknowledge-irq)))

(define (wait-irq-operation irq)
  (wait-operation (vector-ref *interrupt-cvars* irq)))

;; Fiber based UART driver that uses IRQs
(define (uart-driver i/o-base irq read-ch write-ch)
  (define RBR (fx+ i/o-base 0))
  (define THB (fx+ i/o-base 0))
  (define IER (fx+ i/o-base 1))
  (define IIR (fx+ i/o-base 2))
  (define FCR (fx+ i/o-base 2))
  (define LCR (fx+ i/o-base 3))
  (define MCR (fx+ i/o-base 4))
  (define LSR (fx+ i/o-base 5))
  (define MSR (fx+ i/o-base 6))
  (define SCR (fx+ i/o-base 7))
  ;; Accessible when LCR[7]=1
  (define DL (fx+ i/o-base 0))
  (define DH (fx+ i/o-base 1))
  (define IIR-INTERRUPT-PENDING #b1)
  (define IER-READ #b01)
  (define IER-R/W #b11)
  (define IER-LINE-STATUS #b100)
  (define IER-MODEM-STATUS #b1000)
  (define LSR-DATA-READY #b0000001)
  (define LSR-RBR-EMPTY  #b1000000)
  (define LSR-THR-EMPTY  #b0100000)
  (define MCR-DTR  #b00000001)
  (define MCR-RTS  #b00000010)
  (define MCR-OUT2 #b00001000)
  ;; FIFO control
  (define FCR-ENABLE           #b00000001)
  (define FCR-CLEAR-RX         #b00000010)
  (define FCR-CLEAR-TX         #b00000100)
  (define FCR-TRIGGER-LEVEL-1  #b00000000)
  (define FCR-TRIGGER-LEVEL-4  #b01000000)
  (define FCR-TRIGGER-LEVEL-8  #b10000000)
  (define FCR-TRIGGER-LEVEL-14 #b11000000)
  (define (ready-to-rx?)
    (eqv? (fxand (get-i/o-u8 LSR) LSR-DATA-READY) LSR-DATA-READY))
  (define (ready-to-tx?)
    (eqv? (fxand (get-i/o-u8 LSR) LSR-THR-EMPTY) LSR-THR-EMPTY))
  (define (uart-set-baudrate rate)
    (let ((latch (fxdiv 115200 rate)))
      (unless (fx<=? 1 latch #xffff)
        (error 'uart-set-baudrate "Invalid baud rate" rate))
      (let ((lcr (get-i/o-u8 LCR)))
        ;; Set the Divisor Latch Access Bit
        (put-i/o-u8 LCR (fxior lcr #x80))
        ;; Latch the new baudrate
        (put-i/o-u8 DL (fxbit-field latch 0 8))
        (put-i/o-u8 DH (fxbit-field latch 8 16))
        ;; Clear the DLAB
        (put-i/o-u8 LCR (fxand lcr #x7f)))))
  (define (uart-set-protocol bits parity stop)
    (let ((par (cdr (assq parity '((none . #b000)
                                   (odd . #b001)
                                   (even . #b011)
                                   (mark . #b101)
                                   (space . #b111)))))
          (bits (cdr (assq bits '((5 . #b00)
                                  (6 . #b01)
                                  (7 . #b10)
                                  (8 . #b11)))))
          (stop (if (eqv? stop 1) 0 1)))
      (put-i/o-u8 LCR (fxior (fxarithmetic-shift-left par 3)
                             (fxarithmetic-shift-left stop 2)
                             bits))))
  (define (uart-handle-irq)
    (let ((iir (get-i/o-u8 IIR)))
      (put-i/o-u8 IER 0)
      (when (eqv? (fxand iir IIR-INTERRUPT-PENDING) 0)
        (case (fxand (fxarithmetic-shift-right iir 1) #b111)
          [(#b010 #b110)
           ;; There is data to read. #b110 = time-out
           (let loop ()
             (when (ready-to-rx?)
               (if (not rx-head)
                   (set! rx-head (get-i/o-u8 RBR))
                   (set! rx-buf (enqueue rx-buf (get-i/o-u8 RBR))))
               (loop)))]
          [(#b000)
           (get-i/o-u8 MSR)]
          [(#b001)
           ;; It's ok to write now
           (let loop ()
             (when (and (not (queue-empty? tx-buf))
                        (= (fxand (get-i/o-u8 LSR) #b100000) #b100000))
               (let-values ([(b q) (dequeue tx-buf)])
                 (put-i/o-u8 THB b)
                 (set! tx-buf q))
               (loop)))]
          [(#b011)
           ;; Receiver Line Status Interrupt
           (get-i/o-u8 LSR)]
          (else #f)))))
  (define (uart-init)
    (uart-set-protocol 8 'none 1)
    (uart-set-baudrate 9600)
    (put-i/o-u8 MCR (fxior MCR-DTR MCR-RTS MCR-OUT2))
    (put-i/o-u8 FCR (fxior FCR-TRIGGER-LEVEL-4 FCR-CLEAR-RX FCR-CLEAR-TX FCR-ENABLE))
    (put-i/o-u8 IER 0))
  (define tx-buf (make-queue))
  (define rx-buf (make-queue))
  (define rx-head #f)                   ;not the most elegant solution

  (uart-init)
  (enable-irq irq)
  (uart-handle-irq)
  (acknowledge-irq irq)

  (do () ((not 'ever-stop))
    (if (queue-empty? tx-buf)
        (put-i/o-u8 IER (fxior IER-READ IER-LINE-STATUS IER-MODEM-STATUS))
        (put-i/o-u8 IER (fxior IER-R/W IER-LINE-STATUS IER-MODEM-STATUS)))
    (match (perform-operation
            (choice-operation
             (wrap-operation (wait-irq-operation irq)
                             (lambda _ 'int))
             (if (fx>=? (queue-length tx-buf) 64)
                 (choice-operation)
                 (wrap-operation (get-operation write-ch)
                                 (lambda (x) (cons 'tx x))))
             (if (not rx-head)
                 (choice-operation)
                 (wrap-operation (put-operation read-ch rx-head)
                                 (lambda _ 'rx)))))
      ['int
       (uart-handle-irq)
       (acknowledge-irq irq)]
      ['rx
       ;; We've passed on a byte received from the UART
       (cond ((queue-empty? rx-buf)
              (set! rx-head #f))
             (else
              (let-values ([(b q) (dequeue rx-buf)])
                (set! rx-head b)
                (set! rx-buf q))))]
      [('tx . byte)
       ;; We've received a byte to transmit to the UART
       (cond ((ready-to-tx?)
              (put-i/o-u8 THB byte))
             (else
              (set! tx-buf (enqueue tx-buf byte))))])))

(define (pc-com0-setup)
  ;; Start standard input/output on COM1
  (define com1 #x3f8)
  (define com1-irq 4)
  (define com2 #x2f8)
  (define com2-irq 3)
  (let ((read-ch (make-channel))
        (write-ch (make-channel)))
    (spawn-fiber (lambda ()
                   (uart-driver com1 com1-irq read-ch write-ch)))
    (let ((read (lambda (bv start count)
                  (assert (fx>=? count 1))
                  (let ((b (get-message read-ch)))
                    (bytevector-u8-set! bv start b))
                  1))
          (write (lambda (bv start count)
                   (do ((end (fx+ start count))
                        (i start (fx+ i 1)))
                       ((fx=? i end) count)
                     ;; TODO: Send all bytes in one put
                     (put-message write-ch (bytevector-u8-ref bv i))))))
      ($init-standard-ports read write write (eol-style crlf)))))

;; Hook up a minimal /boot filesystem consisting of the multiboot
;; modules.
(define (pc-setup-boot-filesystem)
  (define boot-modules (get-boot-modules))
  (define (find-module filename)
    (find (lambda (mod)
            (equal? (string-append "/boot/" (car mod))
                    filename))
          boot-modules))
  (define (file-exists? filename)
    (cond ((find-module filename) #t)
          ((member filename '("/" "/boot")) #t)
          (else #f)))
  (define (open-file-input-port filename file-options buffer-mode maybe-transcoder)
    (define who 'open-file-input-port)
    (cond
      ((find-module filename) =>
       (lambda (mod)
         (let ((base (caddr mod)) (size (cadddr mod)) (position 0))
           (define (read! bv start count)
             (define remaining (fx- size position))
             (do ((n (fxmin count remaining))
                  (addr (fx+ base position) (fx+ addr 1))
                  (i start (fx+ i 1)))
                 ((fx=? i n)
                  (set! position (fx+ position n))
                  n)
               (bytevector-u8-set! bv i (get-mem-u8 addr))))
           (define (get-position)
             position)
           (define (set-position! off)
             (set! position (fxmin off size)))
           (define (close)
             (if #f #f))
           (let ((p (make-custom-binary-input-port
                     filename read! get-position set-position! close)))
             ($port-buffer-mode-set! p buffer-mode)
             (if maybe-transcoder
                 (transcoded-port p maybe-transcoder)
                 p)))))
      (else
       (raise (condition
               (make-who-condition who)
               (make-i/o-file-does-not-exist-error filename)
               (make-message-condition "Could not open boot module")
               (make-irritants-condition (list filename)))))))
  (init-set! 'file-exists? file-exists?)
  (init-set! 'open-file-input-port open-file-input-port))

(define (pc-open-i/o-poller)
  (define pc-poll
    (case-lambda
      (()
       ;; TODO: Count the number of IRQs being waited on
       1)
      ((wakeup)                ;wakeup = no-wait / forever / <timeout>
       (if (eq? wakeup 'no-wait)
           0                            ;nothing to do
           (let ((timeout
                  (cond ((eq? wakeup 'no-wait) 0)
                        ((eq? wakeup 'forever) 10000)
                        (else
                         ;; Let's wait until the wakeup time. It
                         ;; doesn't matter if we wait shorter.
                         (max 0 (min (- wakeup (pc-current-ticks))
                                     10000))))))
             (if (not (fx>? timeout 0))
                 0                      ;even more nothing to do
                 (scheduler-wait (* #e1e6 timeout)))))
       '())
      ((cmd . x)
       (unless (eq? cmd 'close)
         (apply error 'pc-poll "Unhandled command" x)))))
  pc-poll)

(define (linux-process-setup)
  ;; TODO: The syscalls that never return EAGAIN should be put on a
  ;; worker thread.
  (define NULL 0)
  (define (filename->c-string who fn)
    (string-for-each
     (lambda (c)
       (when (eqv? c #\nul)
         ;; The filename is not representable on Linux
         (raise (condition
                 (make-who-condition who)
                 (make-i/o-filename-error fn)))))
     fn)
    (string->utf8 (string-append fn "\x0;")))
  (define (make-syscall-i/o-error errno filename attempted-write?)
    (let ((filename (string-copy filename)))
      (cond ((or (eqv? errno ENOENT) (eqv? errno ENOTDIR))
             (make-i/o-file-does-not-exist-error filename))
            ((or (eqv? errno EROFS) (eqv? errno ETXTBSY))
             (make-i/o-file-is-read-only-error filename))
            ((eqv? errno EACCES)
             (if attempted-write?
                 (make-i/o-file-is-read-only-error filename)
                 (make-i/o-file-protection-error filename)))
            ((eqv? errno EEXIST)
             (make-i/o-file-already-exists-error filename))
            (else
             (make-i/o-filename-error filename)))))
  (define (file-exists? filename)
    ;; XXX: This follows symlinks.
    (define F_OK 0)
    (let* ((fn (filename->c-string 'file-exists? filename))
           (status
            (let retry ()
              (sys_faccessat AT_FDCWD
                             (bytevector-address fn) F_OK
                             (lambda (errno)
                               (cond ((eqv? errno ENOENT) #f)
                                     ;; These are arguable. They hide errors in
                                     ;; some way.
                                     ((eqv? errno EACCES) #f)
                                     ((eqv? errno ENOTDIR) #f)
                                     ((eqv? errno EINTR) (retry))
                                     (else
                                      (raise
                                        (condition
                                         (make-who-condition 'file-exists?)
                                         (make-message-condition "Error while checking if the file exists")
                                         (make-irritants-condition (list filename))
                                         (make-syscall-i/o-error errno filename #f)
                                         (make-syscall-error 'faccessat errno))))))))))
      (eqv? 0 status)))
  (define (open-file-input-port filename file-options buffer-mode maybe-transcoder)
    ;; TODO: what of file-options needs to be used?
    (define who 'open-file-input-port)
    (assert (buffer-mode? buffer-mode))
    (let* ((fn (filename->c-string 'open-file-input-port filename))
           (fd (let retry ()
                 (sys_open (bytevector-address fn)
                           (bitwise-ior O_NOCTTY O_LARGEFILE O_NONBLOCK)
                           0
                           (lambda (errno)
                             (if (eqv? errno EINTR)
                                 (retry)
                                 (raise (condition
                                         (make-who-condition who)
                                         (make-message-condition "Could not open file")
                                         (make-irritants-condition (list filename))
                                         (make-syscall-i/o-error errno filename #f)
                                         (make-syscall-error 'open errno))))))))
           (position 0))
      (define (handle-read-error errno)
        (raise
          (condition
           (make-syscall-i/o-error errno filename #f)
           (make-syscall-error 'read errno))))
      (define (read! bv start count)
        (assert (fx<=? (fx+ start count) (bytevector-length bv)))
        (let ((status (let retry ()
                        (sys_read fd (fx+ (bytevector-address bv) start) count
                                  (lambda (errno)
                                    (cond ((eqv? errno EAGAIN)
                                           (wait-for-readable fd)
                                           (retry))
                                          ((eqv? errno EINTR)
                                           (retry))
                                          (else
                                           (handle-read-error errno))))))))
          (set! position (+ position status))
          status))
      ;; TODO! pwrite/pread
      (define (get-position)
        position)
      (define (set-position! off)
        (sys_lseek fd off SEEK_SET)
        (set! position off))
      (define (close)
        ;; TODO: Obviously closing should be done in some automated
        ;; manner as well. And closing can block, so it should be
        ;; done in another thread. Linux always closes the fd even
        ;; if this fails with EINTR.
        (sys_close fd (lambda (errno)
                        (unless (eqv? errno EINTR)
                          (raise
                            (make-who-condition 'close-port)
                            (make-message-condition "Error while closing the file")
                            (make-syscall-i/o-error errno filename #f)
                            (make-syscall-error 'close errno))))))
      (let ((p (make-custom-binary-input-port
                filename read! get-position set-position! close)))
        ($port-buffer-mode-set! p buffer-mode)
        (port-file-descriptor-set! p fd)
        (if maybe-transcoder
            (transcoded-port p maybe-transcoder)
            p))))
  (define (open-file-output-port filename file-options buffer-mode maybe-transcoder)
    ;; TODO: what of file-options needs to be used?
    (define who 'open-file-input-port)
    (define no-create (enum-set-member? 'no-create file-options))
    (define no-fail (enum-set-member? 'no-fail file-options))
    (define no-truncate (enum-set-member? 'no-truncate file-options))
    (assert (buffer-mode? buffer-mode))
    (let* ((fn (filename->c-string 'open-file-output-port filename))
           (fd (let retry ()
                 (sys_open (bytevector-address fn)
                           (bitwise-ior O_NOCTTY O_LARGEFILE O_WRONLY O_NONBLOCK
                                        (if no-create 0 O_CREAT)
                                        (if (and no-fail (not no-truncate)) O_TRUNC 0))
                           #o644
                           (lambda (errno)
                             (if (eqv? errno EAGAIN)
                                 (retry)
                                 (raise (condition
                                         (make-who-condition who)
                                         (make-message-condition "Could not open file")
                                         (make-irritants-condition (list filename))
                                         (make-syscall-i/o-error errno filename #f)
                                         (make-syscall-error 'open errno))))))))
           (position 0))
      (define (handle-write-error errno)
        (if (eqv? errno EINTR)
            'retry
            (raise
              (condition
               (make-syscall-i/o-error errno filename #f)
               (make-syscall-error 'write errno)))))
      (define (write! bv start count)
        (assert (fx<=? (fx+ start count) (bytevector-length bv)))
        (let ((status
               (let retry ()
                 (sys_write fd (fx+ (bytevector-address bv) start) count
                            (lambda (errno)
                              (cond ((eqv? errno EAGAIN)
                                     (wait-for-writable fd)
                                     (retry))
                                    ((eqv? errno EINTR)
                                     (retry))
                                    (else
                                     (handle-write-error errno))))))))
          (set! position (+ position status))
          status))
      (define (get-position)
        position)
      #;(define (set-position! off)
          #f)
      (define (close)
        (sys_close fd (lambda (errno)
                        (unless (eqv? errno EINTR)
                          (raise
                            (make-who-condition 'close-port)
                            (make-message-condition "Error while closing the file")
                            (make-syscall-i/o-error errno filename #f)
                            (make-syscall-error 'close errno))))))
      (let ((p (make-custom-binary-output-port
                filename write! get-position #f close)))
        ($port-buffer-mode-set! p buffer-mode)
        (port-file-descriptor-set! p fd)
        (if maybe-transcoder
            (transcoded-port p maybe-transcoder)
            p))))
  (define (linux-open-i/o-poller)
    (define (poll-type->events poll-type)
      ;; TODO: EPOLLPRI     ;out of band data
      (fxior (case poll-type
               ((read) EPOLLIN)
               ((write) EPOLLOUT)
               (else 0))
             (fxior EPOLLRDHUP           ;peer closed
                    EPOLLERR EPOLLHUP    ;can't be unset
                    EPOLLET              ;event-triggered
                    EPOLLONESHOT)))      ;one event only
    (define (epoll_ctl epfd op fd events user-data)
      (if (eqv? op EPOLL_CTL_DEL)
          (sys_epoll_ctl epfd EPOLL_CTL_DEL fd NULL)
          (let ((event (make-bytevector sizeof-epoll_event)))
            (bytevector-u32-native-set! event 0 events)
            (bytevector-u32-native-set! event 4 user-data)
            (sys_epoll_ctl epfd op fd (bytevector-address event)))))
    (let* ((epfd (sys_epoll_create1 EPOLL_CLOEXEC))
           (maxevents 10)
           (events (make-bytevector (* maxevents sizeof-epoll_event)))
           (fds (make-eqv-hashtable))
           (num-waiting 0)
           (who 'linux-i/o-poller))
      (define epoller
        (case-lambda
          (()
           num-waiting)
          ((wakeup)            ;wakeup = no-wait / forever / <timeout>
           (let ((n (if (and (eqv? (hashtable-size fds) 0)
                             (eq? wakeup 'no-wait))
                        0               ;nothing to do
                        (let ((timeout
                               (cond ((eq? wakeup 'no-wait) 0)
                                     ((eq? wakeup 'forever) -1)
                                     (else
                                      ;; Let's wait until the wakeup
                                      ;; time. It doesn't matter if we
                                      ;; wait shorter.
                                      (max 0 (min (- wakeup (linux-current-ticks))
                                                  60000))))))
                          (if (and (eqv? (hashtable-size fds) 0)
                                   (not (fx>=? timeout 0)))
                              0  ;even more nothing to do
                              ;; TODO: Ask pid 0 to do the polling.
                              ;; Pid 0 can have its own epoll that we
                              ;; put this epoll inside.
                              (sys_epoll_pwait epfd (bytevector-address events) maxevents
                                               timeout NULL 0))))))
             (do ((i 0 (fx+ i 1))
                  (offset 0 (fx+ offset sizeof-epoll_event))
                  (ret '()
                       (let ((events (bytevector-u32-native-ref events (fx+ offset offsetof-epoll_event-events)))
                             (fd (bytevector-u32-native-ref events (fx+ offset offsetof-epoll_event-data))))
                         (match (hashtable-ref fds fd #f)
                           [(and #(readers writers) fd-data)
                            ;; XXX: This never deletes fds from the
                            ;; epoll set.
                            (let ((wakeup
                                   (cond ((not (eqv? 0 (fxand events (fxior EPOLLRDHUP EPOLLERR EPOLLHUP))))
                                          ;; Notify everyone about errors
                                          (vector-set! fd-data 0 '())
                                          (vector-set! fd-data 1 '())
                                          (append readers writers))
                                         ((not (eqv? 0 (fxand events (fxior EPOLLIN EPOLLPRI))))
                                          (vector-set! fd-data 0 '())
                                          readers)
                                         ((not (eqv? 0 (fxand events EPOLLOUT)))
                                          (vector-set! fd-data 1 '())
                                          writers)
                                         (else
                                          (error who "Unknown event on fd" epfd fd events)))))
                              (cond ((and (null? (vector-ref fd-data 0))
                                          (null? (vector-ref fd-data 1)))
                                     #f)
                                    ((pair? (vector-ref fd-data 1))
                                     (epoll_ctl epfd EPOLL_CTL_MOD fd (poll-type->events 'write) fd))
                                    ((pair? (vector-ref fd-data 0))
                                     (epoll_ctl epfd EPOLL_CTL_MOD fd (poll-type->events 'read) fd)))
                              (append wakeup ret))]
                           [else
                            (error who "Event on unknown fd" epfd fd)]))))
                 ((fx=? i n)
                  (set! num-waiting (fx- num-waiting (length ret)))
                  ret))))
          ((cmd fd poll-type user-value)
           (case cmd
             ((add)
              ;; Add a file descriptor to the epoll set. Need to be
              ;; careful if the fd is already in there. Two fibers can
              ;; be waiting for the same fd.
              (let ((old-fd-data (hashtable-ref fds fd #f)))
                (match (or old-fd-data (vector '() '()))
                  [(and #(readers writers) fd-data)
                   (set! num-waiting (+ num-waiting 1))
                   (if (eq? poll-type 'read)
                       (vector-set! fd-data 0 (cons user-value readers))
                       (vector-set! fd-data 1 (cons user-value writers)))
                   (let* ((read-events (if (null? (vector-ref fd-data 0))
                                           0
                                           (poll-type->events 'read)))
                          (write-events (if (null? (vector-ref fd-data 1))
                                            0
                                            (poll-type->events 'write)))
                          (events (fxior read-events write-events)))
                     (cond ((not old-fd-data)
                            ;; XXX: closed fds are automatically
                            ;; removed from the epoll set.
                            (add-fdes-finalizer! fd (lambda (fd)
                                                      (hashtable-delete! fds fd)))
                            (hashtable-set! fds fd fd-data)
                            (epoll_ctl epfd EPOLL_CTL_ADD fd events fd))
                           ((or (not (boolean=? (null? readers) (null? (vector-ref fd-data 0))))
                                (not (boolean=? (null? writers) (null? (vector-ref fd-data 1)))))
                            ;; The events mask has changed
                            (epoll_ctl epfd EPOLL_CTL_MOD fd events fd))))])))
             ((close)
              (sys_close epfd))
             (else
              (error who "Unhandled command" cmd))))))
      epoller))
  (define (linux-current-ticks)
    ;; TODO: See if CLOCK_MONOTONIC_COARSE and the vDSO is better for this
    (let* ((x (make-bytevector sizeof-timespec))
           (_ (sys_clock_gettime CLOCK_BOOTTIME (bytevector-address x)))
           (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
           (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
      (fx+ (fx* seconds 1000)
           (fxdiv nanoseconds #e1e+6))))
  (define (linux-get-time clock)
    (let* ((x (make-bytevector sizeof-timespec))
           (_ (sys_clock_gettime clock (bytevector-address x)))
           (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
           (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
      (values seconds nanoseconds)))
  (define (linux-get-time-resolution clock)
    (let* ((x (make-bytevector sizeof-timespec))
           (_ (sys_clock_getres clock (bytevector-address x)))
           (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
           (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
      (values seconds nanoseconds)))
  (define (sys$read fd bv start count)
    (assert (fx<=? (fx+ start count) (bytevector-length bv)))
    (let retry ()
      (sys_read fd (fx+ (bytevector-address bv) start) count
                (lambda (errno)
                  (cond ((eqv? errno EAGAIN)
                         (wait-for-readable fd)
                         (retry))
                        ((eqv? errno EINTR)
                         (retry))
                        (else
                         (raise
                           (condition (make-syscall-error 'read errno)
                                      (make-irritants-condition
                                       (list fd 'bv start count))))))))))
  (define (sys$write fd bv start count)
    (assert (fx<=? (fx+ start count) (bytevector-length bv)))
    (let retry ()
      (sys_write fd (fx+ (bytevector-address bv) start) count
                 (lambda (errno)
                   (cond ((eqv? errno EAGAIN)
                          (wait-for-writable fd)
                          (retry))
                         ((eqv? errno EINTR)
                          (retry))
                         (else
                          (raise
                            (condition (make-syscall-error 'write errno)
                                       (make-irritants-condition
                                        (list fd 'bv start count))))))))))
  ;; XXX: Potential trouble here: https://cr.yp.to/unix/nonblock.html
  (define (set-fd-nonblocking fd)
    (let ((prev (sys_fcntl fd F_GETFL 0)))
      (when (eqv? 0 (fxand O_NONBLOCK prev))
        (sys_fcntl fd F_SETFL (fxior O_NONBLOCK prev)))))
  (set-fd-nonblocking STDIN_FILENO)
  (set-fd-nonblocking STDOUT_FILENO)
  (set-fd-nonblocking STDERR_FILENO)
  ($init-standard-ports (lambda (bv start count)
                          ;; TODO: For Linux 4.14+, use preadv2() with
                          ;; RWF_NOWAIT.
                          (sys$read STDIN_FILENO bv start count))
                        (lambda (bv start count)
                          (sys$write STDOUT_FILENO bv start count))
                        (lambda (bv start count)
                          (sys$write STDERR_FILENO bv start count))
                        (eol-style lf))
  (port-file-descriptor-set! (current-input-port) STDIN_FILENO)
  (port-file-descriptor-set! (current-output-port) STDOUT_FILENO)
  (port-file-descriptor-set! (current-error-port) STDERR_FILENO)
  (init-set! 'file-exists? file-exists?)
  (init-set! 'open-file-input-port open-file-input-port)
  (init-set! 'open-file-output-port open-file-output-port)
  (init-set! 'open-i/o-poller linux-open-i/o-poller)
  (time-init-set! 'cumulative-process-time
                  (lambda () (linux-get-time CLOCK_THREAD_CPUTIME_ID)))
  (time-init-set! 'cumulative-process-time-resolution
                  (lambda () (linux-get-time-resolution CLOCK_THREAD_CPUTIME_ID)))
  (time-init-set! 'current-time (lambda () (linux-get-time CLOCK_REALTIME)))
  (time-init-set! 'current-time-resolution (lambda () (linux-get-time-resolution CLOCK_REALTIME)))
  (time-init-set! 'current-ticks linux-current-ticks))

(define (process-init)
  ;; (define (nanosleep seconds)
  ;;   (assert (fx>=? seconds 0))
  ;;   (scheduler-wait seconds)
  ;;   (if #f #f))
  (init-set! 'exit process-exit)
  ;; (time-init-set! 'nanosleep nanosleep)
  (init-set! 'allocate allocate)
  (init-set! 'command-line (get-command-line))
  (init-set! 'environment-variables (get-environment))
  (case (get-boot-loader)
    ((linux)
     (init-set! 'machine-type '#(amd64 linux))
     (let ((pid (get-pid)))
       (case (pid-value pid)
         ((1)
          (linux-process-setup))
         (else
          (error '$init-process "Internal error: no code for this pid" pid)))))
    ((multiboot)
     (init-set! 'machine-type '#(amd64 loko))
     (init-set! 'open-i/o-poller pc-open-i/o-poller)
     (time-init-set! 'current-ticks pc-current-ticks)
     (let ((pid (get-pid)))
       (case (pid-value pid)
         ((1)
          (pc-com0-setup)
          (pc-setup-boot-filesystem))
         (else
          (error '$init-process "Internal error: no code for this pid" pid)))))
    (else
     (error '$init-process "Internal error: wrong value for get-boot-loader"
            (get-boot-loader)))))

(when (eq? ($boot-loader-type) 'scheme)
  (init-set! 'init process-init)))
