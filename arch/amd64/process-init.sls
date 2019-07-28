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

;; It is important that a Loko process never performs a blocking
;; system call (i.e. a process should always be preemptable). So a
;; process can't do open/read/write itself, but must use message
;; passing to get that functionality.

;; TODO: almost all the code here should be someplace else, e.g. in
;; modules. The online compiler must be working first though.

(library (loko arch amd64 process-init)
  (export)
  (import
    (except (rnrs) command-line exit)
    (srfi :98 os-environment-variables)
    (loko system unsafe)
    (only (loko system $io)
          $init-standard-ports
          $port-buffer-mode-set!)
    (except (loko system $host) allocate)
    (only (loko system $bytevectors)
          $bytevector-location)
    (loko system $processes)
    (only (loko system $repl) banner repl)
    (loko arch amd64 linux-numbers)
    (loko arch amd64 linux-syscalls)
    (text-mode console)
    (text-mode console events)
    (text-mode console model)
    (text-mode platform)
    (only (loko system $init) run-user-interface)
    (loko main))

(define-record-type pid
  (sealed #t) (opaque #t)
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

(define (wait timeout)
  ;; TODO: this message should take a number back from the scheduler
  ;; that indicates for how long it slept. This is so that if the
  ;; process is awoken by an uninteresting message it can go back to
  ;; sleeping with the original timeout without asking the scheduler
  ;; for the current time.
  (when timeout
    (assert (fxpositive? timeout)))
  (let ((vec (vector 'wait timeout #f)))
    (handle-wait-reply vec ($process-yield vec))))

(define (handle-wait-reply vec reply)
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
     (error 'wait "Unknown reply from scheduler" reply)))
  )

#;
(define (listen fd mask)
  ($process-yield `(listen ,fd ,mask)))

;; (define (wait/irq irq timeout)
;;   ;; XXX: compound message?
;;   (listen irq #f)
;;   (wait timeout))

(define (enable-irq irq)
  (assert (fx<=? 0 irq 15))
  ($process-yield `(enable-irq ,irq)))

(define (acknowledge-irq/wait irq timeout)
  (assert (fx<=? 0 irq 15))
  (when timeout
    (assert (fxpositive? timeout)))
  (let ((vec (vector 'wait timeout #f)))
    (handle-wait-reply vec ($process-yield `(acknowledge-irq ,irq ,vec)))))

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
           (wait timeout))
          ((match? (channel-peek channel point))
           (channel-dequeue channel point))
          (else
           (lp (channel-next channel point)))))
  )

(define (com0-setup)
  ;; Start a REPL on com0
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

(define (make-keyboard-reader)
  ;; 1st level entries:
  ;; (<code> <no-mods> <shift> <alt-graph>)
  ;; 2nd level entries: <char> | <symbol>
  (define swedish-keymap
    '((1 Escape Escape)
      (41 #\§ #\½)
      (2 #\1 #\!) (3 #\2 #\") (4 #\3 #\# #\£) (5 #\4 #\¤ #\$) (6 #\5 #\% #\€)
      (7 #\6 #\& #\¥) (8 #\7 #\/ #\{) (9 #\8 #\( #\[) (10 #\9 #\) #\]) (11 #\0 #\= #\})
      (12 #\+ #\?) (14 Backspace Backspace)
      (15 Tab Tab)
      (16 #\q #\Q) (17 #\w #\W) (18 #\e #\E) (19 #\r #\R) (20 #\t #\T) (21 #\y #\Y)
      (22 #\u #\U) (23 #\i #\I) (24 #\o #\O) (25 #\p #\P) (26 #\å #\Å)

      (28 Enter Enter)

      (30 #\a #\A) (31 #\s #\S) (32 #\d #\D) (33 #\f #\F) (34 #\g #\G) (35 #\h #\H)
      (36 #\j #\J) (37 #\k #\K) (38 #\l #\L) (39 #\ö #\Ö) (40 #\ä #\Ä)
      ;; (#x1B dead_key)

      (43 #\' #\*)

      (44 #\z #\Z) (45 #\x #\X) (46 #\c #\C) (47 #\v #\V) (48 #\b #\B) (49 #\n #\N)
      (50 #\m #\M) (51 #\, #\;) (52 #\. #\:) (53 #\- #\_)

      (57 #\space #\space)

      (86 #\< #\>)

      ;; (29 ctrl)                           ; left ctrl
      ;; (42 shift)
      ;; (54 shift)
      ;; (56 alt)

      ;; (#xE01D ctrl)                       ; right ctrl
      ;; (#xE038 altgr)
      ;; (#xE05B meta)
      ;; (#xE05D compose)

      ;; (58 ctrl)                           ; (58 caps-lock)
      ;; (69 num-lock)
      ;; (70 scroll-lock)

      (59 F1 F1) (60 F2 F2) (61 F3 F3) (62 F4 F4)

      (#xE052 Insert Insert)
      (#xE047 Home Home)
      (#xE049 PageUp PageUp)
      (#xE053 Delete Delete)
      (#xE04F End End)
      (#xE051 PageDown PageDown)

      (#xE04B ArrowLeft ArrowLeft)
      (#xE04D ArrowRight ArrowRight)
      (#xE048 ArrowUp ArrowUp)
      (#xE050 ArrowDown ArrowDown)))
  (define kbd-irq 1)
  (define kbd-data #x60)
  (define kbd-sts/cmd #x64)
  (define (read-scancode) (get-i/o-u8 kbd-data))
  (define (read-status) (get-i/o-u8 kbd-sts/cmd))
  (define mods (modifier-set))
  (enable-irq kbd-irq)
  (lambda ()
    (let lp-ext ((extended 0))
      (let lp ()
        (let ((msg (acknowledge-irq/wait kbd-irq #f)))
          (if (fxzero? (fxand (read-status) #b1))
              (lp)                        ;no data
              (let ((scancode (fxior extended (read-scancode))))
                (case scancode
                  ((#x38)
                   (set! mods (enum-set-union mods (modifier-set alt)))
                   (lp-ext 0))
                  ((#xB8)
                   (set! mods (enum-set-difference mods (modifier-set alt)))
                   (lp-ext 0))

                  ((#xE038)
                   (set! mods (enum-set-union mods (modifier-set alt-graph)))
                   (lp-ext 0))
                  ((#xE0B8)
                   (set! mods (enum-set-difference mods (modifier-set alt-graph)))
                   (lp-ext 0))

                  ((#x1D #xE01D #x3A)
                   (set! mods (enum-set-union mods (modifier-set ctrl)))
                   (lp-ext 0))
                  ((#x9D #xE09D #xBA)
                   (set! mods (enum-set-difference mods (modifier-set ctrl)))
                   (lp-ext 0))

                  ((42 54)
                   (set! mods (enum-set-union mods (modifier-set shift)))
                   (lp-ext 0))
                  ((170 182)
                   (set! mods (enum-set-difference mods (modifier-set shift)))
                   (lp-ext 0))

                  ((#xE0)
                   ;; Extended scancodes
                   (lp-ext #xE000))

                  (else
                   (cond
                     ((fxbit-set? scancode 7)
                      ;; Released key
                      (lp-ext 0))
                     ((assv scancode swedish-keymap) =>
                      (lambda (binding)
                        (let* ((binding (cdr binding))
                               (level2
                                (cond ((enum-set-member? (modifier shift) mods)
                                       (cadr binding))
                                      ((and (enum-set-member? (modifier alt-graph) mods)
                                            (fx>=? (length binding) 3))
                                       (caddr binding))
                                      (else
                                       (car binding)))))
                          (if (char? level2)
                              (make-key-press-event mods level2 level2 'standard)
                              (make-key-press-event mods #f level2 'standard)))))
                     (else
                      (make-unknown-event 'keyboard scancode))))))))))))

(define (vga-textmode-backend)
  (define pid (get-pid))
  (define mem-base #xb8000)
  (define reg-base #x3c0)
  (define rows 25)
  (define cols 80)
  ;; VGA regs
  (define crtc-addr 20)
  (define crtc-data 21)
  ;; Registers in the CRT Controller:
  (define cursor-start #x0a)
  (define cursor-end #x0b)
  (define cursor-location-high #x0e)
  (define cursor-location-low #x0f)
  (define (crtc-read addr)
    (put-i/o-u8 (fx+ reg-base crtc-addr) addr)
    (get-i/o-u8 (fx+ reg-base crtc-data)))
  (define (crtc-write addr byte)
    (put-i/o-u8 (fx+ reg-base crtc-addr) addr)
    (put-i/o-u8 (fx+ reg-base crtc-data) byte))
  (define (vga-cursor-move x y)
    (let ((offset (+ x (* y cols))))
      (crtc-write cursor-location-low (fxbit-field offset 0 8))
      (crtc-write cursor-location-high (fxbit-field offset 8 16))))
  (define vga-colors
    (vector Black Blue Green Cyan Red Magenta Brown Gray
            DarkGray LightBlue LightGreen LightCyan LightRed
            LightMagenta Yellow White))
  (define (closest-color col fallback)
    (if (eqv? col Default)
        fallback
        (let lp ((i 0))
          (cond ((fx=? i (vector-length vga-colors)) fallback)
                ((fx=? col (vector-ref vga-colors i)) i)
                (else (lp (fx+ i 1)))))))
  (let ((keyboard (make-keyboard-reader)))
    (lambda (cmd arg)
      (case cmd
        [(get-size)
         (values cols rows 0 0)]
        [(init)
         #f]
        [(update redraw)
         (let ((c arg))
           (assert (fx=? cols (console-full-cols c)))
           (assert (fx=? rows (console-full-rows c)))
           (do ((y 0 (fx+ y 1))) ((fx=? y rows))
             (let ((mem-row (fx* 2 (fx* y cols))))
               (when (console-row-dirty? c y 'absolute)
                 (let-values ([(buf mbuf fgbuf bgbuf abuf idx) (%index/nowin c 0 y)])
                   (do ((x 0 (fx+ x 1))) ((fx=? x cols))
                     (let ((mem-offset (fx+ (fx* 2 x) mem-row)))
                       (let ((ch (text-ref buf (fx+ idx x))))
                         (unless (textcell-unused? ch)
                           (put-mem-u8 (fx+ mem-base mem-offset)
                                        (char->integer ch))
                           (let ((fg (closest-color (fg-ref fgbuf (fx+ idx x)) 7))
                                 (bg (closest-color (bg-ref bgbuf (fx+ idx x)) 0)))
                             (put-mem-u8 (fx+ mem-base (fx+ mem-offset 1))
                                         (fxior (fxarithmetic-shift-left bg 4)
                                                fg)))))))))))
           (clear-console-dirty! c)
           (vga-cursor-move (fx+ (console-x c) (console-x1 c))
                            (fx+ (console-y c) (console-y1 c))))]
        [(read-event)
         #; (let lp () (wait (expt 10 9)) (lp))
         (keyboard)]
        [else
         #f]))))

(define (linux-process-setup)
  ;; TODO: this is NOT what should happen. There should be message
  ;; passing and things like that. Basically none of these syscalls
  ;; should be here, they should be implemented by passing messages
  ;; to the scheduler.
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
    (let ((fn (filename->c-string 'file-exists? filename)))
      (eqv? 0
            (sys_faccessat AT_FDCWD
                           ($bytevector-location fn) F_OK
                           (lambda (errno)
                             (cond ((eqv? errno ENOENT) #f)
                                   ;; These are arguable. They hide errors in
                                   ;; some way.
                                   ((eqv? errno EACCES) #f)
                                   ((eqv? errno ENOTDIR) #f)
                                   (else
                                    (raise
                                      (condition
                                       (make-who-condition 'file-exists?)
                                       (make-message-condition "Error while checking if the file exists")
                                       (make-irritants-condition (list filename))
                                       (make-syscall-i/o-error errno filename #f)
                                       (make-syscall-error 'faccessat errno))))))))))
  (define (open-file-input-port filename file-options buffer-mode maybe-transcoder)
    ;; TODO: what of file-options needs to be used?
    (define who 'open-file-input-port)
    (assert (buffer-mode? buffer-mode))
    (let* ((fn (filename->c-string 'open-file-input-port filename))
           (fd (sys_open ($bytevector-location fn)
                         (bitwise-ior O_NOCTTY O_LARGEFILE)
                         0
                         (lambda (errno)
                           (raise (condition
                                   (make-who-condition who)
                                   (make-message-condition "Could not open file")
                                   (make-irritants-condition (list filename))
                                   (make-syscall-i/o-error errno filename #f)
                                   (make-syscall-error 'open errno))))))
           (position 0))
      (define (handle-read-error errno)
        (if (eqv? errno EINTR)
            'retry
            (raise
              (condition
               (make-syscall-i/o-error errno filename #f)
               (make-syscall-error 'read errno)))))
      (define (read! bv start count)
        ;; Reading should be done in another thread...
        (assert (fx<=? (fx+ start count) (bytevector-length bv)))
        (let ((status (sys_read fd (fx+ ($bytevector-location bv) start) count
                                handle-read-error)))
          (cond ((eqv? status 'retry)
                 (read! bv start count))
                (else
                 (set! position (+ position status))
                 status))))
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
                        (unless (eqv? errno (- EINTR))
                          (raise
                            (make-who-condition 'close-port)
                            (make-message-condition "Error while closing the file")
                            (make-syscall-i/o-error errno filename #f)
                            (make-syscall-error 'close errno))))))
      (let ((p (make-custom-binary-input-port
                filename read! get-position set-position! close)))
        ($port-buffer-mode-set! p buffer-mode)
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
           (fd (sys_open ($bytevector-location fn)
                         (bitwise-ior O_NOCTTY O_LARGEFILE O_WRONLY
                                      (if no-create 0 O_CREAT)
                                      (if (and no-fail (not no-truncate)) O_TRUNC 0))
                         #o644
                         (lambda (errno)
                           (raise (condition
                                   (make-who-condition who)
                                   (make-message-condition "Could not open file")
                                   (make-irritants-condition (list filename))
                                   (make-syscall-i/o-error errno filename #f)
                                   (make-syscall-error 'open errno))))))
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
        (let ((status (sys_write fd (fx+ ($bytevector-location bv) start) count
                                 handle-write-error)))
          (cond ((eqv? status 'retry)
                 (write! bv start count))
                (else
                 (set! position (+ position status))
                 status))))
      (define (get-position)
        position)
      #;(define (set-position! off)
          #f)
      (define (close)
        (sys_close fd (lambda (errno)
                        (unless (eqv? errno (- EINTR))
                          (raise
                            (make-who-condition 'close-port)
                            (make-message-condition "Error while closing the file")
                            (make-syscall-i/o-error errno filename #f)
                            (make-syscall-error 'close errno))))))
      (let ((p (make-custom-binary-output-port
                filename write! get-position #f close)))
        ($port-buffer-mode-set! p buffer-mode)
        (if maybe-transcoder
            (transcoded-port p maybe-transcoder)
            p))))
  (define (current-processor-time)
    ;; TODO: Loko's scheduler must be involved in this so that it
    ;; does not measure time spent in other processes, and so that
    ;; process migration does not mess it up. Apparently Linux's
    ;; scheduler must also be involved (see clock_getres(2)) because
    ;; it does not work properly when threads are moved between
    ;; processors...
    (let* ((x (make-bytevector sizeof-timespec))
           (status (sys_clock_gettime CLOCK_THREAD_CPUTIME_ID
                                      ($bytevector-location x)))
           (nanoseconds
            (+ (* (expt 10 9) (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
               (bytevector-u64-native-ref x offsetof-timespec-tv_nsec))))
      nanoseconds))
  ($init-standard-ports (lambda (bv start count)
                          (assert (fx<=? (fx+ start count) (bytevector-length bv)))
                          (sys_read STDIN_FILENO (fx+ ($bytevector-location bv) start) count))
                        (lambda (bv start count)
                          (assert (fx<=? (fx+ start count) (bytevector-length bv)))
                          (sys_write STDOUT_FILENO (fx+ ($bytevector-location bv) start) count))
                        (lambda (bv start count)
                          (assert (fx<=? (fx+ start count) (bytevector-length bv)))
                          (sys_write STDERR_FILENO (fx+ ($bytevector-location bv) start) count))
                        (eol-style lf))
  (init-set! 'file-exists? file-exists?)
  (init-set! 'open-file-input-port open-file-input-port)
  (init-set! 'open-file-output-port open-file-output-port)
  (init-set! 'current-processor-time current-processor-time)
  ;; (display "PID: ")
  ;; (write (pid-value (get-pid)))
  ;; (newline)
  (let ((bg-process (new-process)))
    (when (and bg-process (pid-value bg-process))
      (display "Started in the background: ")
      (write (pid-value bg-process))
      (newline))))

(define (process-init)
  ;; XXX: this is temporary. It should actually use message passing
  ;; to get a closure or something for startup.
  (define (print . x) (for-each display x) (newline))
  (define (nanosleep seconds)
    (assert (fx>=? seconds 0))
    (wait seconds)
    (if #f #f))
  (init-set! 'exit process-exit)
  (init-set! 'nanosleep nanosleep)
  (init-set! 'allocate allocate)
  (init-set! 'command-line (get-command-line))
  (init-set! 'environment-variables (get-environment))
  (case (get-boot-loader)
    ((linux)
     (init-set! 'machine-type '#(amd64 linux))
     ;; FIXME: This needs to receive a procedure to start
     (let ((pid (get-pid)))
       (case (pid-value pid)
         ((1)
          ;; FIXME: Needs to use IPC for port communication
          (linux-process-setup)
          (current-console (default-console))
          (main))
         (else
          (error '$init-process "Internal error: no code for this pid" pid)))))
    ((multiboot)
     (init-set! 'machine-type '#(amd64 loko))
     (let ((pid (get-pid)))
       (case (pid-value pid)
         ((1)
          (new-process)                 ;starts the serial console
          (current-console (make-console (vga-textmode-backend)))
          (let lp ()
            (main)
            (lp)))
         ((2)
          ;; FIXME: Needs more IPC
          (com0-setup)
          (current-console (default-console))
          (let lp ()
            (main)
            (lp)))
         (else
          (error '$init-process "Internal error: no code for this pid" pid)))))
    (else
     (error '$init-process "Internal error: wrong value for get-boot-loader"
            (get-boot-loader))))
  (process-exit 0))

(when (eq? ($boot-loader-type) 'scheme)
  (init-set! 'init process-init)))
