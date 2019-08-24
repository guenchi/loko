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

;;; Test suite for fibers

(import
  (rnrs)
  (loko match)
  (loko system fibers))

#;
(begin
  ;; For running the tests on Guile fibers
  (import (fibers) (fibers channels) (fibers operations) (fibers timers)
          (fibers conditions) (ice-9 match) (rnrs))
  (define make-cvar make-condition)
  (define signal-cvar! signal-condition!))

;; Error in a fiber
(run-fibers
 (lambda ()
   (spawn-fiber (lambda ()
                  (error #f "This error is expected")))))

(run-fibers
 (lambda ()
   (let ((cvar (make-cvar)))
     (do ((i 0 (+ i 1)))
         ((= i 5))
       (spawn-fiber (lambda ()
                      (display (list 'wait i))
                      (newline)
                      (wait cvar)
                      (display (list 'done i))
                      (newline))))
     (let ((ch (make-channel)))
       (spawn-fiber (lambda ()
                      (sleep 1)
                      (put-message ch 'go!)))
       (spawn-fiber (lambda ()
                      (assert (eq? (get-message ch) 'go!))
                      (signal-cvar! cvar)
                      (signal-cvar! cvar))))
     (sleep 1.5))))

;; Only one receiver gets the message
(run-fibers
 (lambda ()
   (let ((ch (make-channel))
         (n 0)
         (done (make-cvar)))
     (spawn-fiber (lambda ()
                    (put-message ch 'msg)))
     (let ((receiver (lambda ()
                       (let ((msg (get-message ch)))
                         (write (list 'received-0 msg))
                         (newline)
                         (set! n (+ n 1))
                         (assert (eq? msg 'msg))
                         (signal-cvar! done)))))
       (spawn-fiber receiver)
       (spawn-fiber receiver)
       (wait done)
       (sleep 1)
       (display (list 'n n))
       (newline)
       (assert (= n 1))))))

;; Wrap when the optimistic receive works
(run-fibers
 (lambda ()
   (let ((ch (make-channel)))
     (spawn-fiber (lambda () (put-message ch 'msg)))
     (sleep 0.1)
     (let ((msg (perform-operation
                 (wrap-operation
                  (get-operation ch)
                  (lambda (x)
                    (cons 'wrapped x))))))
       (write (list 'received-1 msg))
       (newline)
       (assert (equal? msg '(wrapped . msg)))))))

;; Wrap when the pessimistic receive is used
(run-fibers
 (lambda ()
   (let ((ch (make-channel)))
     (spawn-fiber (lambda ()
                    (sleep 0.1)
                    (put-message ch 'msg)))
     (let ((msg (perform-operation
                 (wrap-operation
                  (get-operation ch)
                  (lambda (x)
                    (cons 'wrapped x))))))
       (write (list 'received-2 msg))
       (newline)
       (assert (equal? msg '(wrapped . msg)))))))

;; Double wrap when the optimistic receive works
(run-fibers
 (lambda ()
   (let ((ch (make-channel)))
     (spawn-fiber (lambda () (put-message ch 'msg)))
     (sleep 0.1)
     (let ((msg (perform-operation
                 (wrap-operation
                  (wrap-operation
                   (get-operation ch)
                   (lambda (x)
                     (cons 'wrapped x)))
                  (lambda (x)
                    (cons 'double x))))))
       (write (list 'received-3 msg))
       (newline)
       (assert (equal? msg '(double . (wrapped . msg))))))))

;; Double wrap when the pessimistic receive is used
(run-fibers
 (lambda ()
   (let ((ch (make-channel)))
     (spawn-fiber (lambda ()
                    (sleep 0.1)
                    (put-message ch 'msg)))
     (let ((msg (perform-operation
                 (wrap-operation
                  (wrap-operation
                   (get-operation ch)
                   (lambda (x)
                     (cons 'wrapped x)))
                  (lambda (x)
                    (cons 'double x))))))
       (write (list 'received-4 msg))
       (newline)
       (assert (equal? msg '(double . (wrapped . msg))))))))

;; Choice between a channel and a sleep
(run-fibers
 (lambda ()
   (let ((ch (make-channel)))
     (spawn-fiber (lambda ()
                    (sleep 0.1)
                    (put-message ch 'msg)))
     (let ((msg (perform-operation (choice-operation (get-operation ch)
                                                     (sleep-operation 0.3)))))
       (write (list 'received-5 msg))
       (newline)
       (assert (eq? msg 'msg)))
     (sleep 1))))

;; Choice + wrap
(run-fibers
 (lambda ()
   (let ((ch (make-channel)))
     (spawn-fiber (lambda ()
                    (sleep 0.1)
                    (put-message ch 'msg)))
     (let ((msg (perform-operation
                 (choice-operation (wrap-operation
                                    (get-operation ch)
                                    (lambda (x)
                                      (cons 'op1 x)))
                                   (sleep-operation 0.5)
                                   (wrap-operation
                                    (get-operation ch)
                                    (lambda (x)
                                      (cons 'op2 x)))))))
       (write (list 'received-6 msg))
       (newline)
       (assert (member msg '((op1 . msg)
                             (op2 . msg))))))))

;; Nested choice + wrap
(run-fibers
 (lambda ()
   (let ((ch (make-channel)))
     (spawn-fiber (lambda ()
                    (sleep 0.1)
                    (put-message ch 'msg)))
     (let ((msg (perform-operation
                 (choice-operation (choice-operation (wrap-operation
                                                      (get-operation ch)
                                                      (lambda (x)
                                                        (cons 'op1 x)))
                                                     (sleep-operation 0.5))
                                   (wrap-operation
                                    (get-operation ch)
                                    (lambda (x)
                                      (cons 'op2 x)))))))
       (write (list 'received-7 msg))
       (newline)
       (assert (member msg '((op1 . msg)
                             (op2 . msg))))))))

;; Nested choice + wrap
(run-fibers
 (lambda ()
   (let ((ch (make-channel)))
     (spawn-fiber (lambda ()
                    (sleep 0.5)
                    (put-message ch 'msg)))
     (let ((msg (perform-operation
                 (choice-operation (choice-operation (wrap-operation
                                                      (sleep-operation 0.3)
                                                      (lambda x 'op1))
                                                     (wrap-operation
                                                      (sleep-operation 0.2)
                                                      (lambda x 'op2)))
                                   (wrap-operation
                                    (get-operation ch)
                                    (lambda (x)
                                      (cons 'op3 x)))))))
       (write (list 'received-8 msg))
       (newline)
       (assert (eq? msg 'op2))))))

;; Select between channels
(run-fibers
 (lambda ()
   (let ((ch0 (make-channel))
         (ch1 (make-channel))
         (ch2 (make-channel)))
     (define (sender ch msg)
       (lambda () (put-message ch msg)))
     (define (recv ch prefix)
       (wrap-operation (get-operation ch) (lambda (x) (cons prefix x))))
     (spawn-fiber (sender ch0 'msg0))
     (spawn-fiber (sender ch1 'msg1))
     (spawn-fiber (sender ch2 'msg2))
     (let ((msg (perform-operation (choice-operation (recv ch0 'ch0)
                                                     (recv ch1 'ch1)
                                                     (recv ch2 'ch2)))))
       (write (list 'received-9 msg))
       (newline)
       (assert (member msg '((ch0 . msg0) (ch1 . msg1) (ch2 . msg2))))))))

;; Non-determinism in choice
(run-fibers
 (lambda ()
   (define ht (make-hashtable equal-hash equal?))
   (do ((i 0 (+ i 1)))
       ((= i 100))
     (let ((ch0 (make-channel))
           (ch1 (make-channel))
           (ch2 (make-channel))
           (ch3 (make-channel)))
       (define (sender ch msg)
         (lambda () (put-message ch msg)))
       (define (recv ch prefix)
         (wrap-operation (get-operation ch)
                         (lambda (x) (cons prefix x))))
       (spawn-fiber (sender ch0 'msg0))
       (spawn-fiber (sender ch1 'msg1))
       (spawn-fiber (sender ch2 'msg2))
       (spawn-fiber (sender ch3 'msg3))
       (let* ((op (choice-operation (recv ch0 'ch0)
                                    (recv ch1 'ch1)
                                    (recv ch2 'ch2)
                                    (recv ch3 'ch3)))
              (msg0 (perform-operation op))
              (msg1 (perform-operation op))
              (msg2 (perform-operation op))
              (msg3 (perform-operation op)))
         (hashtable-set! ht (list msg0 msg1 msg2 msg3) #t)
         (write (list 'received-11 msg0 msg1 msg2 msg3))
         (newline))))
   (let ((keys (hashtable-keys ht)))
     (write (cons (vector-length keys) keys)) (newline)
     ;; Normally there are 19 entries, but there could be fewer
     (assert (> (vector-length keys) 10)))))
