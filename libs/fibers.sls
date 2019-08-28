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

;;; Multiprocessing based on channels

;; The API should be compatible with Guile's fibers, except for
;; (fibers internal):

;; https://github.com/wingo/fibers/wiki/Manual

;; The current implementation is heavily dumbed down from Wingo's
;; implementation. It is single-threaded, non-preemptive and based on
;; simple call/cc.

;; There is also a CML implementation in Scheme 48 that can be used
;; for inspiration, but the _Parallel Concurrent ML_ paper is closer
;; to this implementation.

;; Here's CML documentation that can give a clue about what features
;; are missing: http://cml.cs.uchicago.edu/pages/cml.html

(library (loko libs fibers)
  (export
    ;; Public interface
    run-fibers spawn-fiber
    wrap-operation choice-operation perform-operation
    make-base-operation
    make-channel channel? put-operation get-operation
    put-message get-message
    sleep-operation timer-operation sleep
    make-cvar cvar? signal-cvar! wait-operation wait

    yield-current-task
    exit-current-task

    ;; Seemingly not exported in Guile. It has them, but the arguments
    ;; are ports.
    wait-for-readable wait-for-writable

    ;; Internals
    run-fiber-scheduler)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (pfds heaps)
    (loko match)
    (only (loko libs control) print-condition)
    (only (loko libs time) current-ticks)
    (only (loko init) open-i/o-poller))

;; Public domain queue code based on SLIB, by Andrew Wilcox.
(define (make-queue)
  (cons '() '()))

(define (queue-push! q datum)
  (let* ((old-first-pair (car q))
         (new-first-pair (cons datum old-first-pair)))
    (set-car! q new-first-pair)
    (when (null? old-first-pair)
      (set-cdr! q new-first-pair))))

(define (enqueue! q datum)
  (let ((new-pair (cons datum '())))
    (if (null? (car q))
        (set-car! q new-pair)
        (set-cdr! (cdr q) new-pair))
    (set-cdr! q new-pair)))

(define (dequeue! q)
  (let ((first-pair (car q)))
    (if (null? first-pair)
        (error 'dequeue! "attempt to dequeue an empty queue"))
    (let ((first-cdr (cdr first-pair)))
      (set-car! q first-cdr)
      (when (null? first-cdr)
        (set-cdr! q '()))
      (car first-pair))))

(define (queue-empty? q)
  (null? (car q)))

(define (queue-front q)
  (let ((first-pair (car q)))
    (if (null? first-pair)
        (error 'queue-front "queue is empty" q)
        (car first-pair))))
;;-

;; A single global scheduler per Loko process
(define *scheduler-k* 'sched-uninit)
(define *scheduler-inbox* #f)
(define *scheduler-next* #f)
(define *scheduler-i/o* #f)
(define *scheduler-timers* #f)
(define *scheduler-running* #f)

(define-syntax debug
  (syntax-rules ()
    [(_ x* ...)
     (values)]
    [(_ x* ...)
     (begin
       (write (cons 'fiber: (list x* ...))
              (current-error-port))
       (newline (current-error-port))
       (values))]))

(define (init-scheduler)
  (unless *scheduler-i/o*
    (set! *scheduler-k* 'sched-inited)
    (set! *scheduler-inbox* #f)
    (set! *scheduler-next* (make-queue))
    (set! *scheduler-i/o* (open-i/o-poller))
    (set! *scheduler-timers* (make-heap (lambda (x y)
                                          (fx<? (car x) (car y)))))))

(define (run-fibers init-thunk)
  (init-scheduler)
  (cond
    ((not *scheduler-running*)
     (set! *scheduler-running* #t)
     (schedule init-thunk)
     (run-fiber-scheduler)
     (set! *scheduler-running* #f)
     (*scheduler-i/o* 'close #f #f #f)
     (set! *scheduler-i/o* #f))
    (else
     (init-thunk))))

(define (make-xorshift32 seed)
  ;; http://www.jstatsoft.org/v08/i14/paper
  (let ((state seed))
    (lambda ()
      (let* ((y state)
             (y (fxxor y (fxarithmetic-shift y 13)))
             (y (fxxor y (fxarithmetic-shift y -17)))
             (y (fxxor y (fxarithmetic-shift y 5)))
             (y (fxand y #xffffffff)))
        (set! state y)
        y))))
(define random-u32 (make-xorshift32 2463534242))

(define (spawn-fiber thunk)
  (unless *scheduler-i/o*
    (init-scheduler))
  (debug 'SPAWN thunk)
  (schedule
   (lambda ()
     (guard (exn
             (else
              (display "Exception from fiber: " (current-error-port))
              (write thunk (current-error-port))
              (newline (current-error-port))
              (print-condition exn (current-error-port))))
       (thunk))))
  (debug 'SPAWNED))

(define (i/o-poll i/o wakeup-time)
  (i/o wakeup-time))

(define (i/o-empty? i/o)
  (eqv? 0 (i/o)))

(define (schedule x)
  (debug 'SCHEDULE x)
  (enqueue/push! *scheduler-next* x))

(define (enqueue/push! q x)
  (cond ((and (not (queue-empty? q)) (fxodd? (random-u32)))
         (queue-push! q x))
        (else
         (enqueue! q x))))

(define (schedule-at-time expiry thunk)
  (set! *scheduler-timers* (heap-insert *scheduler-timers* (cons expiry thunk))))

(define (schedule-for-poll i/o fd poll-type thunk)
  (i/o 'add fd poll-type thunk))

(define (yield-current-task)
  (suspend
   (lambda (resume)
     (schedule (lambda ()
                 (resume (lambda () #t)))))))

(define (exit-current-task return-value)
  (when (not *scheduler-k*)
    (error 'exit-current-task "Expected to be called from inside a fiber"))
  (*scheduler-k* 'exit return-value #f))

(define (seconds->ticks s)
  (exact (round (* 1000 s))))

(define (run-fiber-scheduler)
  (define (dequeue-tasks)
    (debug 'DEQUEUING *scheduler-inbox*)
    (let* ((wakeup
            (cond ((not (queue-empty? *scheduler-inbox*))
                   ;; We have work pending, so we should
                   ;; not wait if no I/O is available.
                   'no-wait)
                  ((not (heap-empty? *scheduler-timers*))
                   ;; We have no work, but we have a timer. Get the
                   ;; earliest wakeup time. That's how long we will
                   ;; wait for I/O.
                   (car (heap-min *scheduler-timers*)))
                  (else
                   ;; No timeout and no work pending. Let's
                   ;; just wait for some I/O.
                   'forever)))
           (_ (debug 'WAKEUP (current-ticks) wakeup))
           (ready-for-i/o (i/o-poll *scheduler-i/o* wakeup)))
      (let ((now (current-ticks)))
        (debug 'READY-FOR-I/O ready-for-i/o)
        (for-each (lambda (x) (enqueue/push! *scheduler-inbox* x)) ready-for-i/o)
        ;; Schedule expired timers.
        (let lp ((heap *scheduler-timers*))
          (cond
            ((or (heap-empty? heap)
                 (fx>? (car (heap-min heap)) now))
             (set! *scheduler-timers* heap))
            (else
             (debug 'EXPIRED (heap-min heap))
             (match (heap-min heap)
               [(_wakeup-time . thunk)
                (enqueue! *scheduler-inbox* thunk)
                (lp (heap-delete-min heap))]))))
        (debug 'PENDING *scheduler-timers*))))
  (debug 'SCHEDULER-STARTING)
  (do ()
      ((and (queue-empty? *scheduler-next*)
            (i/o-empty? *scheduler-i/o*)
            (heap-empty? *scheduler-timers*)))
    (set! *scheduler-inbox* *scheduler-next*)
    (set! *scheduler-next* (make-queue))
    (debug 'SCHEDULER-TURN)
    (dequeue-tasks)
    (do ()
        ((queue-empty? *scheduler-inbox*))
      (debug 'QUEUE-REMAINING *scheduler-inbox*)
      (let ((task (dequeue! *scheduler-inbox*)))
        (debug 'CALLING-TASK task)
        (let-values ([(cmd arg0 arg1)
                      (call/cc
                        (lambda (k)
                          (set! *scheduler-k* k)
                          (task)
                          (set! *scheduler-k* 'sched-no-k)
                          (values 'return #f #f)))])
          (debug 'RETURN-FROM-TASK task '=> cmd arg0 arg1)
          (case cmd
            ((suspend)
             (let ((after-suspend arg0)
                   (susp-k arg1))
               (debug 'SUSPENDING-TASK after-suspend susp-k)
               (after-suspend susp-k)))
            ((return)
             (debug 'ENDING-TASK task))
            ((exit)
             (debug 'EXITING-TASK task))
            (else
             (error 'run-fiber-scheduler "Unrecognized return to scheduler"
                    cmd arg0 arg1)))))
      (debug 'SCHEDULER 'TURN-OVER
             'inbox *scheduler-inbox*
             'next *scheduler-next*
             'i/o-fds (*scheduler-i/o*)
             'timers *scheduler-timers*))))

;;; Operations

(define-record-type op
  (fields wrap          ;procedure to wrap the return value
          try           ;procedure to optimistically try the operation
          block))       ;procedure to run after blocking

(define make-base-operation make-op)

(define (compose g f)
  (lambda x*
    (call-with-values (lambda () (apply f x*))
                      g)))

(define (wrap-operation op f)
  (if (vector? op)
      (vector-map (lambda (op) (wrap-operation op f)) op)
      (make-op (if (op-wrap op) (compose f (op-wrap op)) f)
               (op-try op)
               (op-block op))))

(define (choice-operation . ops)
  ;; Some kind of vector-flatten
  (let* ((len (do ((ops ops (cdr ops))
                   (len 0 (cond ((vector? (car ops))
                                 (fx+ len (vector-length (car ops))))
                                (else (fx+ len 1)))))
                  ((null? ops) len)))
         (ret (make-vector len #f)))
    (let lp ((ops ops) (i 0))
      (cond ((null? ops) ret)
            ((vector? (car ops))
             (do ((vec (car ops))
                  (i i (fx+ i 1))
                  (j 0 (fx+ j 1)))
                 ((fx=? j (vector-length vec))
                  (lp (cdr ops) i))
               (vector-set! ret i (vector-ref vec j))))
            (else
             (vector-set! ret i (car ops))
             (lp (cdr ops) (fx+ i 1)))))))

(define (suspend callback)
  (debug 'SUSPEND callback)
  (call/cc
    (lambda (susp-k)
      (*scheduler-k* 'suspend callback susp-k))))

(define (perform-operation op)
  (define (block start-idx resume ops)
    (let ((state (vector 'WAITING)))
      (let lp ((i start-idx) (j 0))
        (cond ((fx=? j (vector-length ops))
               (values))
              ((fx=? i (vector-length ops))
               (lp 0 j))
              (else
               (let ((base-op (vector-ref ops i)))
                 (let ((block-fn (op-block base-op))
                       (wrap-fn (op-wrap base-op)))
                   (block-fn (if wrap-fn
                                 (lambda (thunk)
                                   (resume (lambda ()
                                             (call-with-values thunk wrap-fn))))
                                 resume)
                             state)
                   (lp (fx+ i 1) (fx+ j 1)))))))))
  (debug 'PERFORM op)
  (let* ((ops (if (op? op) (vector op) op))
         ;; random start element
         (start-idx (if (fx<=? (vector-length ops) 1)
                        0
                        (fxmod (random-u32) (vector-length ops)))))
    (let lp ((i start-idx) (j 0))
      (cond
        ((fx=? j (vector-length ops))
         (debug 'BLOCKING)
         ((suspend
           (lambda (k)
             (define (resume values-thunk)
               (debug 'RESUMING)
               (schedule (lambda () (k values-thunk))))
             (block i resume ops)))))
        ((fx=? i (vector-length ops))
         (lp 0 j))                      ;wraparound
        (else
         (let ((base-op (vector-ref ops i)))
           (let ((try-fn (op-try base-op))
                 (wrap-fn (op-wrap base-op)))
             (cond ((try-fn) =>
                    (lambda (thunk)
                      (debug 'TRY-SUCCESSFUL)
                      (if wrap-fn
                          ((compose wrap-fn thunk))
                          (thunk))))
                   (else (lp (fx+ i 1) (fx+ j 1)))))))))))

;;; Channels

(define-record-type channel
  (nongenerative)
  (fields recvq sendq)
  (protocol
   (lambda (new)
     (lambda ()
       (new (make-queue) (make-queue))))))

(define (channel-cleanup ch)
  ;; A channel operation can be part of a choice operation where
  ;; another base operation is sync'd. That means that the channel
  ;; code never sees the sync. TODO: Implement this with nack instead.
  (define (queue-cleanup q)
    (let lp ()
      (when (not (queue-empty? q))
        (match (queue-front q)
          [#(_ #('SYNCHED))
           (dequeue! q)
           (lp)]
          [#(_ _ #('SYNCHED))
           (dequeue! q)
           (lp)]
          [x x]))))
  (queue-cleanup (channel-sendq ch))
  (queue-cleanup (channel-recvq ch)))

(define (put-operation ch message)
  ;; Try to send a message on a channel without blocking. Succeeds if
  ;; there is a receiver waiting.
  (define (send-try-fn)
    (channel-cleanup ch)
    (and (not (queue-empty? (channel-recvq ch)))
         (match (dequeue! (channel-recvq ch))
           [#(resume-recv recv-state)
            (cond ((eq? (vector-ref recv-state 0) 'SYNCHED)
                   (send-try-fn))
                  (else
                   (debug 'SENDER 'TRY 'SUCCESS message)
                   (vector-set! recv-state 0 'SYNCHED)
                   (resume-recv (lambda () message))
                   values))])))
  (define (send-block-fn resume-send send-state)
    ;; Enqueue this fiber. The receiver or the cleanup procedure will
    ;; remove us from the queue.
    (debug 'SENDER 'ENQUEUED)
    (enqueue! (channel-sendq ch) `#(,message ,resume-send ,send-state))
    ;; See if someone showed up in the meantime.
    (let retry ()
      (debug 'SENDER 'RETRYING (channel-recvq ch))
      (and (not (queue-empty? (channel-recvq ch)))
           ;; FIXME: find the first not from us
           (match (dequeue! (channel-recvq ch))
             [#(resume-recv recv-state)
              (cond ((eq? (vector-ref recv-state 0) 'SYNCHED)
                     (retry))
                    (else
                     (debug 'SENDER 'RETRY 'SUCCESS message)
                     (vector-set! send-state 0 'SYNCHED)
                     (resume-recv (lambda () message))
                     (resume-send values)
                     (values)))]))))
  (make-base-operation #f send-try-fn send-block-fn))

(define (get-operation ch)
  ;; Try to receive a message on a channel without blocking.
  (define (recv-try-fn)
    (channel-cleanup ch)
    (and (not (queue-empty? (channel-sendq ch)))
         (match (dequeue! (channel-sendq ch))
           [#(val resume-sender state)
            ;; We have a sender in the queue. Resume the sender, delete
            ;; them from the queue and return the value they sent.
            (cond ((eq? (vector-ref state 0) 'SYNCHED)
                   (recv-try-fn))
                  (else
                   (debug 'RECEIVE 'TRY 'SUCCESS val)
                   (vector-set! state 0 'SYNCHED)
                   (resume-sender values)
                   (lambda () val)))])))
  ;; Receive a message on a channel. The blocking case.
  (define (recv-block-fn resume-recv recv-state)
    ;; There was no sender ready, so this fiber is blocked. Put
    ;; ourselves in the recv queue.
    (debug 'RECEIVER 'ENQUEUED)
    (enqueue! (channel-recvq ch) `#(,resume-recv ,recv-state))
    (let retry ()
      (debug 'RECEIVER 'RETRYING (channel-recvq ch))
      ;; FIXME: (choice-operation (put-operation A v) (get-operation
      ;; A)) must not send v to itself.
      (and (not (queue-empty? (channel-sendq ch)))
           ;;FIXME: find the first not from us
           (match (dequeue! (channel-sendq ch))
             [#(val resume-send send-state)
              (cond ((eq? (vector-ref send-state 0) 'SYNCHED)
                     (retry))
                    (else
                     (debug 'RECEIVER 'RETRY 'SUCCESS val)
                     (vector-set! recv-state 0 'SYNCHED)
                     (resume-send values)
                     (resume-recv (lambda () val))
                     (values)))]))))
  (make-base-operation #f recv-try-fn recv-block-fn))

(define (put-message ch message)
  (perform-operation (put-operation ch message)))

(define (get-message ch)
  (perform-operation (get-operation ch)))

;;; Timers

(define (sleep-operation seconds)
  (timer-operation (fx+ (current-ticks) (seconds->ticks seconds))))

(define (timer-operation expiry)
  (define (sleep-try-fn)
    (cond ((fx<=? expiry (current-ticks))
           values)
          (else #f)))
  (define (sleep-block-fn resume-sleep state)
    (define (cthulhu)
      (cond ((eq? (vector-ref state 0) 'SYNCHED)
             #f)
            (else
             (vector-set! state 0 'SYNCHED)
             (resume-sleep values))))
    (schedule-at-time expiry cthulhu))
  (make-base-operation #f sleep-try-fn sleep-block-fn))

(define (sleep seconds)
  (perform-operation (sleep-operation seconds)))

;;; File descriptors

(define (wait-for-readable fd)
  (debug 'wait-for-readable fd)
  (assert (fx>=? fd 0))
  (suspend
   (lambda (resume)
     (schedule-for-poll *scheduler-i/o* fd 'read
                        (lambda ()
                          (debug 'READABLE fd)
                          (resume (lambda () (values))))))))

(define (wait-for-writable fd)
  (debug 'wait-for-writable fd)
  (assert (fx>=? fd 0))
  (suspend
   (lambda (resume)
     (schedule-for-poll *scheduler-i/o* fd 'write
                        (lambda ()
                          (debug 'WRITABLE fd)
                          (resume (lambda () (values))))))))

;;; Condition variables

(define-record-type cvar
  (fields (mutable state)
          (mutable waiting))
  (protocol
   (lambda (new)
     (lambda ()
       (new #f '())))))

(define (call-resume waiting)
  (do ((waiting waiting (cdr waiting)))
      ((null? waiting)
       #f)
    (match (car waiting)
      (#(their-resume their-state)
       (cond ((eq? (vector-ref their-state 0) 'SYNCHED)
              #f)
             (else
              (vector-set! their-state 0 'SYNCHED)
              (their-resume values)))))))

(define (wait-operation cvar)
  (define (wait-try-fn)
    ;; XXX: don't do this as often
    (cvar-waiting-set! cvar (filter (match-lambda
                                     [#(_resume #('SYNCHED)) #f]
                                     [else #t])
                                    (cvar-waiting cvar)))
    (cond ((cvar-state cvar) values)
          (else #f)))
  (define (wait-block-fn resume-wait state)
    (cvar-waiting-set! cvar (cons `#(,resume-wait ,state)
                                  (filter (match-lambda
                                           [#(_resume #('SYNCHED)) #f]
                                           [else #t])
                                          (cvar-waiting cvar))))
    (when (cvar-state cvar)
      (let ((waiting (cvar-waiting cvar)))
        (cvar-waiting-set! cvar '())
        (call-resume waiting)))
    (values))
  (make-base-operation #f wait-try-fn wait-block-fn))

(define (signal-cvar! cvar)
  (unless (cvar-state cvar)
    (cvar-state-set! cvar #t)
    (let ((waiting (cvar-waiting cvar)))
      (cvar-waiting-set! cvar '())
      (call-resume waiting))))

(define (wait cvar)
  (perform-operation (wait-operation cvar))))
