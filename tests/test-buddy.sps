#!/usr/bin/env -S loko --program
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

;;; Test suite for the buddy allocator

(import
  (rnrs (6))
  (loko arch amd64 linux-syscalls)
  (loko arch amd64 linux-numbers)
  (loko libs buddy)
  (loko system unsafe))

(define (print . x) (for-each display x) (newline))

;; Get some memory to test with
(define testaddr
  (sys_mmap 0
            (* 1024 1024)
            (fxior PROT_READ PROT_WRITE)
            (fxior MAP_PRIVATE MAP_ANONYMOUS)
            -1 0))

(print "Test address: " (number->string testaddr 16))

;; Start with some simple tests
(let ()
  (define buddy (make-buddy testaddr (* 1024 1024) 12))
  (define addr0 (buddy-allocate! buddy (* 4 1024)))
  (define addr1 (buddy-allocate! buddy (* 4 1024)))
  (define addr2 (buddy-allocate! buddy (* 4 1024)))
  (define addr3 (buddy-allocate! buddy (* 4 1024)))
  (buddy-free! buddy addr0)
  (buddy-free! buddy addr1)
  (buddy-free! buddy addr2)
  (buddy-free! buddy addr3)
  buddy)

(let ()
  (define buddy (make-buddy testaddr (* 1024 1024) 12))
  (define addr0 (buddy-allocate! buddy (* 4 1024)))
  (define addr1 (buddy-allocate! buddy (* 256 1024)))
  (define addr2 (buddy-allocate! buddy (* 8 1024)))
  (define addr3 (buddy-allocate! buddy (* 4 1024)))
  (buddy-free! buddy addr0)
  (buddy-free! buddy addr2)
  (buddy-free! buddy addr1)
  (buddy-free! buddy addr3)
  buddy)

(let ()
  (define buddy (make-buddy testaddr (* 1024 1024) 12))
  (let* ((addr0 (buddy-allocate! buddy (* 512 1024)))
         (addr1 (buddy-allocate! buddy (* 512 1024))))
    (assert addr0)
    (assert addr1)
    (buddy-free! buddy addr0)
    (buddy-free! buddy addr1)
    (buddy-allocate! buddy (* 512 1024))
    (buddy-allocate! buddy (* 512 1024))))

(let ()
  (define buddy (make-buddy testaddr (* 1024 1024) 12))
  (let* ((addr0 (buddy-allocate! buddy (* 512 1024)))
         (addr1 (buddy-allocate! buddy 4096))
         (addr2 (buddy-allocate! buddy 4096))
         (addr3 (buddy-allocate! buddy 4096)))
    (assert addr0)
    (assert addr1)
    (assert addr2)
    (assert addr3)
    (buddy-free! buddy addr2)
    (buddy-free! buddy addr1)
    (buddy-free! buddy addr0)
    ))

;; Allocate blocks of some predetermined size, free them in an
;; unspecified order
(define (single-size-test lowest-order size)
  (define addrs (make-eqv-hashtable))
  (define buddy (make-buddy testaddr (* 1024 1024) lowest-order))
  (define (test)
    (do ((i 0 (+ i 1)))
        ((= i (/ (* 1024 1024) size))
         (do ((i 0 (fx+ i 4))
              (addr testaddr (fx+ testaddr 4)))
             ((fx=? i (* 1024 1024)))
           (put-mem-u32 addr #xdeadbeef))
         (unless (zero? (buddy-free-amount buddy))
           (error 'single-size-test "Wrong free amount" size buddy))
         (unless (hashtable-size addrs)
           (error 'single-size-test "Too few addresses returned" size buddy (hashtable-size addrs)))
         (vector-for-each
          (lambda (addr)
            (buddy-free! buddy addr))
          (hashtable-keys addrs)))
      (let ((addr (buddy-allocate! buddy size)))
        (when (not addr)
          (error 'single-size-test "Out of memory" size buddy))
        (when (hashtable-contains? addrs addr)
          (error 'single-size-test "Address returned twice" size buddy addr))
        (hashtable-set! addrs addr #t))))
  (print (list 'single-size-test lowest-order size))
  (test)
  (hashtable-clear! addrs)
  (test)
  (hashtable-clear! addrs)
  (test)
  (hashtable-clear! addrs)
  (test))

#;
(single-size-test 12 4096)

(do ((i 0 (+ i 1)))
    ((= i 9))
  (single-size-test 12 (* (expt 2 i) 4 1024)))
(print "Passed single-size-test")

;; Allocate blocks of varying sizes until no free memory remains; then
;; free all the addresses
(define (varying-size-test lowest-order sizes*)
  (define addrs (make-eqv-hashtable))
  (define buddy (make-buddy testaddr (* 1024 1024) lowest-order))
  (define (test)
    (do ((i 0 (+ i 1))
         (sizes sizes* (if (null? (cdr sizes)) sizes* (cdr sizes))))
        ((zero? (buddy-free-amount buddy))
         (do ((i 0 (fx+ i 4))
              (addr testaddr (fx+ testaddr 4)))
             ((fx=? i (* 1024 1024)))
           (put-mem-u32 addr #xdeadbeef))
         (vector-for-each
          (lambda (addr)
            (buddy-free! buddy addr))
          (hashtable-keys addrs))
         (assert (= (buddy-free-amount buddy) (* 1024 1024))))
      (cond ((car sizes)
             (let ((addr (buddy-allocate! buddy (car sizes))))
               (when addr
                 (when (hashtable-contains? addrs addr)
                   (error 'single-size-test "Address returned twice" (car sizes) buddy addr))
                 (hashtable-set! addrs addr #t))))
            (else
             ;; Free a random address
             (let ((addr (vector-ref (hashtable-keys addrs) 0)))
               (hashtable-delete! addrs addr)
               (buddy-free! buddy addr))))))
  (test)
  (hashtable-clear! addrs)
  (test)
  (hashtable-clear! addrs)
  (test)
  (hashtable-clear! addrs)
  (test))

(varying-size-test 12 '(131072 1024 4096 65536 8192 16384))
(varying-size-test 12 '(1024 131072 4096 8192 65536 16384))
(varying-size-test 12 '(262144 65536 16384))
(varying-size-test 12 '(262144 4096))
(varying-size-test 8 '(4096 256 256 256 8192 #f 256))
(print "Passed varying-size-test")

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

;; Allocate blocks of varying sizes until no free memory remains; then
;; free all the addresses
(define (random-test lowest-order)
  (define random-u32 (make-xorshift32 2463534242))
  (define addrs (make-eqv-hashtable))
  (define buddy (make-buddy testaddr (* 1024 1024) lowest-order))
  (define (test)
    (do ((i 0 (+ i 1)))
        ((eqv? i 5000)
         (vector-for-each
          (lambda (addr)
            (buddy-free! buddy addr))
          (hashtable-keys addrs))
         (assert (= (buddy-free-amount buddy) (* 1024 1024))))
      (cond ((and (eqv? 0 (fxand (random-u32) 15))
                  (not (eqv? 0 (hashtable-size addrs))))
             ;; Free a random address
             (let ((addr (vector-ref (hashtable-keys addrs) 0)))
               (hashtable-delete! addrs addr)
               (buddy-free! buddy addr)))
            (else
             (let* ((size (fxmod (random-u32) (* 1024 1024)))
                    (addr (buddy-allocate! buddy size)))
               (when addr
                 (when (hashtable-contains? addrs addr)
                   (error 'single-size-test "Address returned twice" buddy addr))
                 (do ((i 0 (fx+ i 4))
                      (addr addr (fx+ addr 4)))
                     ((fx>=? i size))
                   (put-mem-u32 addr #xdeadbeef))
                 (hashtable-set! addrs addr #t)))))))
  (print (list 'random-test lowest-order))
  (test)
  (hashtable-clear! addrs)
  (test)
  (hashtable-clear! addrs)
  (test)
  (hashtable-clear! addrs)
  (test))

(random-test 5)
(random-test 12)
(print "Random test passed")
