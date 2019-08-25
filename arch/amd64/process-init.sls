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
;; schedulers). This is equivalent to user-space processes. It
;; communicates with other processes (and its scheduler) by using
;; $process-yield.

;; Blocking syscalls should be kept out of here.

(library (loko arch amd64 process-init)
  (export)
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
    (except (loko system $host) allocate)
    (loko system $primitives)
    (loko arch amd64 linux-numbers)
    (loko arch amd64 linux-syscalls))

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
  (make-pid ($process-yield '(new-process))))

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

(define (handle-scheduler-wait-reply vec reply who)
  ;; The scheduler will update vec.
  (case reply
    ((timeout)
     #f)
    ((message)
     ;; TODO: put the message in the queue
     ;; (display "it's a message: ")
     ;; (write vec)
     ;; (newline)
     #f
     )
    (else
     (error who "Unknown reply from scheduler" reply))))

;; (define (wait/irq irq timeout)
;;   ;; XXX: compound message?
;;   (listen irq #f)
;;   (scheduler-wait timeout))

(define (enable-irq irq)
  (assert (fx<=? 0 irq 15))
  ($process-yield `(enable-irq ,irq)))

(define (acknowledge-irq/wait irq timeout)
  (assert (fx<=? 0 irq 15))
  (when timeout
    (assert (fxpositive? timeout)))
  (let ((vec (vector 'wait timeout #f)))
    (handle-scheduler-wait-reply vec ($process-yield `(acknowledge-irq ,irq ,vec))
                                 'acknowledge-irq/wait)))

(define (send-message target message)
  (assert (fixnum? message))
  (cond ((pid? target)
         (let ((ok? ($process-yield `#(send ,(pid-value target)
                                            ,message))))
           (unless ok?
             (error 'send-message "The target PID is dead" target message))))
        (else
         (error 'send-message "Unknown target type" target message))))

(define (receive-message channel timeout match?)
  ;; Consume messages from the given channel if they match the
  ;; predicate. If no message matches, then wait for a message.
  #f
  #;
  (let lp ((point (channel-start channel)))
    (cond ((channel-end? channel point)
           (scheduler-wait timeout))
          ((match? (channel-peek channel point))
           (channel-dequeue channel point))
          (else
           (lp (channel-next channel point)))))
  )

(define (com0-setup)
  ;; Start standard input/output on com0
  (define com0 #x3f8)
  (define com0-irq 4)
  (define rbr (+ com0 0))
  (define thb (+ com0 0))
  (define ier (+ com0 1))
  (define fcr (+ com0 2))
  (define mcr (+ com0 4))
  (define lsr (+ com0 5))
  (define IER-READ #b01)
  (define IER-R/W #b11)
  (define LSR-DATA-READY #b1)
  (define MCR-DTR  #b00000001)
  (define MCR-RTS  #b00000010)
  (define MCR-OUT2 #b00001000)
  (define FCR-ENABLE   #b00000001)
  (define FCR-CLEAR-RX #b00000010)
  (define FCR-CLEAR-TX #b00000100)
  (define (put-u8 b)
    (let lp ()
      (when (fxzero? (fxand (get-i/o-u8 lsr) #b100000))
        (lp)))
    (put-i/o-u8 thb b)
    (when (eqv? b (char->integer #\linefeed))
      (put-i/o-u8 thb (char->integer #\return))))
  (define (put bv start count)
    (do ((end (fx+ start count))
         (i start (fx+ i 1)))
        ((fx=? i end) count)
      (put-u8 (bytevector-u8-ref bv i))))
  (define timeout (expt 10 9))
  (define (data-ready?)
    (not (fxzero? (fxand (get-i/o-u8 lsr) LSR-DATA-READY))))

  (enable-irq com0-irq)
  (put-i/o-u8 ier 0)
  (put-i/o-u8 mcr (fxior MCR-DTR MCR-RTS MCR-OUT2))
  (put-i/o-u8 fcr FCR-ENABLE)
  ($init-standard-ports
   (lambda (bv start count)
     ;; XXX: this should not be inside the port, this should be the
     ;; main loop. The response to the IRQ really needs to be fast,
     ;; because if the FIFO is filled, then data is going to be
     ;; lost. There also should be an IRQ on TX, and the code should
     ;; look at the interrupt cause. Also, the eol-style is cr
     (let lp ()
       (put-i/o-u8 ier IER-READ)
       (let ((msg (acknowledge-irq/wait com0-irq timeout)))
         (put-i/o-u8 ier 0)
         (do ((start start (fx+ start 1))
              (count count (fx- count 1))
              (k 0 (fx+ k 1)))
             ((or (fxzero? count)
                  (not (data-ready?)))
              (if (fxzero? k)
                  (lp)                ;no data, try again
                  k))
           (bytevector-u8-set! bv start (get-i/o-u8 rbr))))))
   put put (eol-style crlf)))

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
       0)
      ((wakeup)                ;wakeup = no-wait / forever / <timeout>
       (if (eq? wakeup 'no-wait)
           0                            ;nothing to do
           (let ((timeout
                  (cond ((eq? wakeup 'no-wait) 0)
                        ((eq? wakeup 'forever) 60000)
                        (else
                         ;; Let's wait until the wakeup time. It
                         ;; doesn't matter if we wait shorter.
                         (max 0 (min (- wakeup (pc-current-ticks))
                                     60000))))))
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
  (time-init-set! 'current-ticks linux-current-ticks)
  ;; (display "PID: ")
  ;; (write (pid-value (get-pid)))
  ;; (newline)
  (let ((bg-process (new-process)))
    (when (and bg-process (pid-value bg-process))
      (display "Started in the background: ")
      (write (pid-value bg-process))
      (newline))))

(define (process-init)
  (define (nanosleep seconds)
    (assert (fx>=? seconds 0))
    (scheduler-wait seconds)
    (if #f #f))
  (init-set! 'exit process-exit)
  (time-init-set! 'nanosleep nanosleep)
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
          (com0-setup)
          (pc-setup-boot-filesystem))
         (else
          (error '$init-process "Internal error: no code for this pid" pid)))))
    (else
     (error '$init-process "Internal error: wrong value for get-boot-loader"
            (get-boot-loader)))))

(when (eq? ($boot-loader-type) 'scheme)
  (init-set! 'init process-init)))
