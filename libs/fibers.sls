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

(library (loko libs fibers)
  (export
    ;; Public interface
    run-fibers spawn-fiber
    wrap-operation choice-operation perform-operation
    make-base-operation
    make-channel channel? put-operation get-operation
    put-message get-message
    sleep-operation timer-operation sleep
    yield-current-task                  ;TODO: is this public?

    ;; Seemingly not exported in Guile. It has them, but the arguments
    ;; are ports.
    wait-for-readable wait-for-writable

    ;; Internals
    run-fiber-scheduler)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (loko match)
    (only (loko libs control) print-condition)
    (only (loko libs time) current-ticks)
    (only (loko init) open-i/o-poller))

;; Public domain queue code based on SLIB.
(define (make-queue)
  (cons '() '()))

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
      (if (null? first-cdr)
          (set-cdr! q '()))
      (car first-pair))))

(define (queue-empty? q)
  (null? (car q)))
;;-

;; A single global scheduler per Loko process
(define *scheduler-k* 'sched-uninit)
(define *scheduler-inbox* #f)
(define *scheduler-next* #f)
(define *scheduler-i/o* #f)
(define *scheduler-timers* '())

(define (debug x)
  ;; (write x)
  ;; (newline)
  (values))

(define (run-fibers init-thunk)
  (cond
    ((not *scheduler-i/o*)
     (set! *scheduler-k* 'sched-inited)
     (set! *scheduler-inbox* #f)
     (set! *scheduler-next* (make-queue))
     (set! *scheduler-i/o* (open-i/o-poller))
     (set! *scheduler-timers* '())
     (schedule init-thunk)
     (run-fiber-scheduler)
     (*scheduler-i/o* 'close #f #f #f)
     (set! *scheduler-i/o* #f))
    (else
     (init-thunk))))

(define (spawn-fiber thunk)
  (debug (list 'SPAWN-FIBER thunk))
  (schedule
   (lambda ()
     (guard (exn
             (else
              (display "Exception from fiber: " (current-error-port))
              (write thunk (current-error-port))
              (newline (current-error-port))
              (print-condition exn (current-error-port))))
       (thunk))))
  (debug (list 'SPAWNED-FIBER)))

(define (i/o-poll i/o wakeup-time)
  (i/o wakeup-time))

(define (i/o-empty? i/o)
  (eqv? 0 (i/o)))

(define (schedule x)
  (debug (list 'SCHEDULE x))
  (enqueue! *scheduler-next* x))

(define (schedule-at-time expiry thunk)
  (set! *scheduler-timers*
        ;; TODO: priority queues
        (list-sort
         (lambda (x y)
           (< (car x) (car y)))
         (cons (cons expiry thunk) *scheduler-timers*))))

(define (schedule-for-poll fd poll-type thunk)
  (*scheduler-i/o* 'add fd poll-type thunk))

(define (yield-current-task)
  (suspend
   (lambda (resume)
     (schedule (lambda ()
                 (resume (lambda () #t)))))))

(define (seconds->ticks s)
  (exact (round (* 1000 s))))

(define (run-fiber-scheduler)
  (define (dequeue-tasks)
    (debug (list 'DEQUEUING *scheduler-inbox*))
    (let* ((wakeup
            (cond ((not (queue-empty? *scheduler-inbox*))
                   ;; We have work pending, so we should
                   ;; not wait if no I/O is available.
                   'no-wait)
                  ((pair? *scheduler-timers*)
                   ;; We have no work, but we have a timer. Get the
                   ;; earliest wakeup time. That's how long we will
                   ;; wait for I/O.
                   (fold-left min (caar *scheduler-timers*)
                              (map car (cdr *scheduler-timers*))))
                  (else
                   ;; No timeout and no work pending. Let's
                   ;; just wait for some I/O.
                   'forever)))
           (_ (debug (list 'WAKEUP (current-ticks) wakeup)))
           (ready-for-i/o (i/o-poll *scheduler-i/o* wakeup)))
      (let ((now (current-ticks)))
        (debug (list 'ready-for-i/o ready-for-i/o))
        (let-values ([(expired pending) (partition (match-lambda
                                                    [(expiry . _) (fx<=? expiry now)])
                                                   *scheduler-timers*)])
          (set! *scheduler-timers* pending)
          (for-each (lambda (x) (enqueue! *scheduler-inbox* (cdr x))) expired)
          (for-each (lambda (x) (enqueue! *scheduler-inbox* x)) ready-for-i/o)
          (debug (list 'EXPIRED expired 'PENDING pending))))))
  (debug 'SCHEDULER-STARTING)
  (do ()
      ((and (queue-empty? *scheduler-next*)
            (i/o-empty? *scheduler-i/o*)
            (null? *scheduler-timers*)))
    (set! *scheduler-inbox* *scheduler-next*)
    (set! *scheduler-next* (make-queue))
    (debug 'SCHEDULER-TURN)
    (dequeue-tasks)
    (do ()
        ((queue-empty? *scheduler-inbox*))
      (debug (list 'QUEUE-REMAINING *scheduler-inbox*))
      (let ((task (dequeue! *scheduler-inbox*)))
        (debug (list 'CALLING-TASK task))
        (let-values ([(cmd arg0 arg1)
                      (call/cc
                        (lambda (k)
                          (set! *scheduler-k* k)
                          (task)
                          (set! *scheduler-k* 'sched-inact)
                          (values 'return #f #f)))])
          (debug (list 'RETURN-FROM-TASK task '=> cmd arg0 arg1))
          (case cmd
            ((suspend)
             (let ((after-suspend arg0)
                   (susp-k arg1))
               (debug (list 'SUSPENDING-TASK after-suspend susp-k))
               (after-suspend susp-k)))
            ((return)
             (debug (list 'ENDING-TASK task)))
            (else
             (error 'run-fiber-scheduler "Unrecognized return to scheduler"
                    cmd arg0 arg1)))))
      (debug 'SCHEDULER-TURN-OVER)
      (debug `(inbox ,*scheduler-inbox*
                     next ,*scheduler-next*
                     i/o-fds ,(*scheduler-i/o*)
                     timers ,*scheduler-timers*)))))

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

;;; FIXME: Untested
(define (wrap-operation op f)
  (make-op (if (op-wrap op) (compose f (op-wrap op)) f)
           (op-try op)
           (op-block op)))

;;; FIXME: Untested
(define (choice-operation . ops)
  (define (try)
    (let lp ((ops ops))
      (match ops
        [(op . rest)
         (let ((sub-try (op-try op))
               (sub-wrap (op-wrap op)))
           (cond ((sub-try) =>
                  (lambda (thunk)
                    (compose sub-wrap thunk)))
                 (else
                  (lp rest))))]
        [_ #f])))
  (define (block resume opstate)
    (for-each
     (lambda (subop)
       (let ((sub-block (op-block subop))
             (sub-wrap (op-wrap subop)))
         (define (wrapped-resume results-thunk)
           (resume (compose sub-wrap results-thunk)))
         (sub-block wrapped-resume opstate)))
     ops))
  (make-op #f try block))

(define (suspend callback)
  (debug "suspending")
  (call/cc
    (lambda (susp-k)
      (*scheduler-k* 'suspend callback susp-k))))

(define (perform-operation op)
  (let ((try-fn (op-try op))
        (block-fn (op-block op))
        (wrap-fn (op-wrap op)))
    (define (pessimistic)
      (define (block resume)
        (block-fn (if wrap-fn
                      (compose wrap-fn resume)
                      resume)
                  (vector 'WAITING)))
      ((suspend
        (lambda (k)
          (define (resume values-thunk)
            (schedule (lambda () (k values-thunk))))
          (debug "calling block-fn\n")
          (block resume)))))
    (cond ((try-fn) =>
           (lambda (thunk)
             (debug "try succeeding\n")
             (if wrap-fn
                 ((compose wrap-fn thunk))
                 (thunk))))
          (else
           (debug "pessimistic\n")
           (pessimistic)))))

;;; Channels

(define-record-type channel
  (nongenerative)
  (fields recvq sendq)
  (protocol
   (lambda (new)
     (lambda ()
       (new (make-queue) (make-queue))))))

(define (put-operation ch message)
  ;; Try to send a message on a channel without blocking. Succeeds if
  ;; there is a receiver waiting.
  (define (send-try-fn)
    (and (not (queue-empty? (channel-recvq ch)))
         (match (dequeue! (channel-recvq ch))
           [#(resume-recv recv-state)
            (debug (list 'TRY 'SENT message))
            (resume-recv (lambda () message))
            values])))
  (define (send-block-fn resume-send send-state)
    ;; Enqueue this fiber. The receiver will remove us from the queue.
    (debug "enqueue\n")
    (enqueue! (channel-sendq ch) `#(,message ,resume-send ,send-state))
    ;; See if someone showed up in the meantime.
    (let retry ()
      (debug (list 'send-retrying (channel-recvq ch)))
      (and (not (queue-empty? (channel-recvq ch)))
           ;;FIXME: find the first not from us
           (match (dequeue! (channel-recvq ch))
             [#(resume-recv recv-state)
              (cond ((eq? (vector-ref recv-state 0) 'SYNCHED)
                     (retry))
                    (else
                     (debug (list 'BLOCK 'SENT message))
                     (vector-set! send-state 0 'SYNCHED)
                     (resume-recv (lambda () message))
                     (resume-send values)
                     (values)))]))))
  (make-base-operation #f send-try-fn send-block-fn))

(define (get-operation ch)
  ;; Try to receive a message on a channel without blocking.
  (define (recv-try-fn)
    (and (not (queue-empty? (channel-sendq ch)))
         (match (dequeue! (channel-sendq ch))
           [#(val resume-sender state)
            ;; We have a sender in the queue. Resume the sender, delete
            ;; them from the queue and return the value they sent.
            (debug (list 'TRY 'RECEIVED val))
            (resume-sender values)
            (lambda () val)])))
  ;; Receive a message on a channel. The blocking case.
  (define (recv-block-fn resume-recv recv-state)
    ;; There was no sender ready, so this fiber is blocked. Put
    ;; ourselves in the recv queue.
    (enqueue! (channel-recvq ch) `#(,resume-recv ,recv-state))
    (let retry ()
      (debug (list 'recv-retrying (channel-recvq ch)))
      ;; FIXME: (choice-operation (put-operation A v) (get-operation
      ;; A)) must not send v to itself.
      (and (not (queue-empty? (channel-sendq ch)))
           ;;FIXME: find the first not from us
           (match (dequeue! (channel-sendq ch))
             [#(val resume-send send-state)
              (cond ((eq? (vector-ref send-state 0) 'SYNCHED)
                     (retry))
                    (else
                     (debug (list 'BLOCK 'RECEIVED val))
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
  (timer-operation (+ (current-ticks) (seconds->ticks seconds))))

(define (timer-operation expiry)
  (define (sleep-try-fn)
    (cond ((fx<=? expiry (current-ticks))
           values)
          (else #f)))
  (define (sleep-block-fn resume-sleep state)
    (schedule-at-time expiry
                      (lambda ()
                        (cond ((eq? (vector-ref state 0) 'SYNCHED)
                               #f)
                              (else
                               (vector-set! state 0 'SYNCHED)
                               (resume-sleep values))))))
  (make-base-operation #f sleep-try-fn sleep-block-fn))

(define (sleep seconds)
  (perform-operation (sleep-operation seconds)))

;;; File descriptors

(define (wait-for-readable fd)
  (debug (list 'wait-for-readable fd))
  (assert (fx>=? fd 0))
  (suspend
   (lambda (resume)
     (schedule-for-poll fd 'read
                        (lambda ()
                          (debug (list 'now-readable fd))
                          (resume (lambda () (values))))))))

(define (wait-for-writable fd)
  (debug (list 'wait-for-writable fd))
  (assert (fx>=? fd 0))
  (suspend
   (lambda (resume)
     (schedule-for-poll fd 'write
                        (lambda ()
                          (debug (list 'now-writable fd))
                          (resume (lambda () (values)))))))))
