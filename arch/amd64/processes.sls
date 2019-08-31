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

;;; Process control

(library (loko arch amd64 processes)
  (export
    ;; process stuff
    $process-yield $process-start)
  (import
    (rnrs (6))
    (only (loko) machine-type)
    (loko arch amd64 memory)
    (loko system unsafe)
    (loko system $primitives)
    (loko system $host)
    (only (loko system $x86) $enable-interrupts $disable-interrupts)
    (only (loko libs context) CPU-VECTOR:SCHEDULER-RUNNING?
          CPU-VECTOR:SCHEDULER-SP))

;; This should be split: the mapping and the starting. After mapping
;; and setting up the stack frame and pointer, just put it in the
;; process queue. The fixed mappings might not be such a good idea.

(define ($process-start area)
  ;; TODO: most of this should be replaced with a real virtual
  ;; memory address allocator.
  (define K 1024)
  (define M (* 1024 K))
  (define G (* 1024 M))
  (define T (* 1024 G))
  (define PAGE-SIZE (* 4 K))
  (define CODE-TOP (* 4 G))
  (define MARK-AND-SWEEP-TOP (* 1 T))
  (define STACK-SIZE (* 128 M))
  ;; XXX: This is a short term fix to allow self-compilation
  (define HEAP-SIZE (if (eq? (vector-ref (machine-type) 1) 'linux)
                        (* 1024 M)
                        (* 256 M)))
  (define VIRTUAL-ADDRESS-TOP (expt 2 (- 48 1)))
  (define (stack-area n)
    (assert (and (integer? n) (not (negative? n))))
    (let ((start (+ MARK-AND-SWEEP-TOP (* n (+ HEAP-SIZE STACK-SIZE)))))
      (when (> (+ start HEAP-SIZE STACK-SIZE) VIRTUAL-ADDRESS-TOP)
        (error 'stack-area "There is no stack area with this number" n))
      start))
  (define (heap-area n)
    (assert (and (integer? n) (not (negative? n))))
    (let ((start (+ MARK-AND-SWEEP-TOP (* n (+ HEAP-SIZE STACK-SIZE)) STACK-SIZE)))
      (when (> (+ start HEAP-SIZE STACK-SIZE) VIRTUAL-ADDRESS-TOP)
        (error 'heap-area "There is no heap area with this number" n))
      start))
  (define (put-mem-u64 a v)
    (put-mem-u32 a (bitwise-and v #xffffffff))
    (put-mem-u32 (fx+ a 4) (bitwise-arithmetic-shift-right v 32)))
  (define (print . x) (for-each display x) (newline))
  (define PROCESS-SAVE-SIZE 4096)
  (let ((start-current-heap (heap-area area))
        (size-current-heap (div HEAP-SIZE 2))
        (start-other-heap (fx+ (heap-area area) (div HEAP-SIZE 2)))
        (size-other-heap (div HEAP-SIZE 2))
        (stack-bottom (fx+ (stack-area area) 4096))
        (stack-top (fx+ (stack-area area) (fx- STACK-SIZE PROCESS-SAVE-SIZE)))
        (stack-size (fx- (fx- STACK-SIZE PROCESS-SAVE-SIZE) 4096)))
    ;; (print "stack-bottom: " (number->string stack-bottom 16))
    ($mmap stack-bottom (+ stack-size PROCESS-SAVE-SIZE) 'stack)
    ($mmap (heap-area area) HEAP-SIZE 'heap)
    (let ((rflags (bitwise-ior (expt 2 18)  ;Alignment check
                               (expt 2 9))) ;Interrupt flag
          (r14 start-current-heap)
          (r13 size-current-heap)
          (start ($linker-address 'scheme-start))
          (rip ($linker-address 'process-init)))
      (let* ((sp (+ (stack-area area) (- STACK-SIZE PROCESS-SAVE-SIZE)))
             ;; The routine called to start up the Scheme
             (sp (fx- sp 8)) (_ (put-mem-u64 sp start))
             ;; Must have RFLAGS to enable #AC and interrupts
             ;; (relevant if running directly on the hardware).
             (sp (fx- sp 8)) (_ (put-mem-u64 sp rflags))
             ;; Memory management stuff for process vector
             (sp (fx- sp 8)) (_ (put-mem-u64 sp stack-size))
             (sp (fx- sp 8)) (_ (put-mem-u64 sp stack-top))
             (sp (fx- sp 8)) (_ (put-mem-u64 sp size-other-heap))
             (sp (fx- sp 8)) (_ (put-mem-u64 sp start-other-heap))
             (sp (fx- sp 8)) (_ (put-mem-u64 sp size-current-heap))
             (sp (fx- sp 8)) (_ (put-mem-u64 sp start-current-heap))
             ;; Memory management registers
             (sp (fx- sp 8)) (_ (put-mem-u64 sp r13))
             (sp (fx- sp 8)) (_ (put-mem-u64 sp r14))
             ;; Process entry point
             (sp (fx- sp 8)) (_ (put-mem-u64 sp rip)))
        ;; XXX: could send boot-data the same way Unix does it
        sp))))

(define ($process-yield msg)
  (let ((sched-sp ($processor-data-ref CPU-VECTOR:SCHEDULER-SP)))
    ;; sched-sp is 0 if the scheduler is running
    ;; (display "Yielding back to SCHED-SP=")
    ;; (display (number->string sched-sp 16))
    ;; (newline)
    (when (eqv? sched-sp 0)
      (error '$process-yield "The scheduler tried to yield"))
    ;; IRQs should be disabled when the scheduler is running
    ($disable-interrupts)
    ($processor-data-set! CPU-VECTOR:SCHEDULER-RUNNING? #t) ;currently yielding
    (let ((msg ($switch-stack sched-sp msg)))
      ($processor-data-set! CPU-VECTOR:SCHEDULER-RUNNING? #f) ;no longer yielding
      ($enable-interrupts)    ;FIXME: should not be used under Linux
      ;; (display "Secret code from scheduler: ")
      ;; (write msg)
      ;; (newline)
      msg))))
