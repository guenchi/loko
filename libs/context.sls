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

;;; Execution context

(library (loko libs context)
  (export
    CPU-VECTOR-SIZE
    CPU-VECTOR:LENGTH
    CPU-VECTOR:PROCESS-VECTOR
    CPU-VECTOR:SCHEDULER-SP
    CPU-VECTOR:ALTSIGSTK-BASE
    CPU-VECTOR:ALTSIGSTK-SIZE
    CPU-VECTOR:CPU-VECTOR-ADDRESS
    CPU-VECTOR:LAST-INTERRUPT-VECTOR
    CPU-VECTOR:BOOT-LOADER-TYPE
    CPU-VECTOR:BOOT-LOADER-DATA
    CPU-VECTOR:SCHEDULER-RUNNING?
    CPU-VECTOR:CPU-NUMBER
    PROCESS-VECTOR:ERROR-INVOKER
    PROCESS-VECTOR:START-CURRENT-HEAP
    PROCESS-VECTOR:SIZE-CURRENT-HEAP
    PROCESS-VECTOR:START-OTHER-HEAP
    PROCESS-VECTOR:SIZE-OTHER-HEAP
    PROCESS-VECTOR:STACK-TOP
    PROCESS-VECTOR:SAVE-AREA
    PROCESS-VECTOR:STACK-SIZE
    PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT
    PROCESS-SAVE-SIZE)
  (import
    (rnrs (6)))

(define-syntax define-const
  (syntax-rules ()
    ((_ name v)
     (define-syntax name
       (identifier-syntax v)))))

(define-const CPU-VECTOR-SIZE 128)      ;n * 64

;; Indexes into the per-cpu vector (offset from fs base).
(define-const CPU-VECTOR:LENGTH 0)
(define-const CPU-VECTOR:PROCESS-VECTOR 1)
(define-const CPU-VECTOR:SCHEDULER-SP 2)
(define-const CPU-VECTOR:ALTSIGSTK-BASE 3)
(define-const CPU-VECTOR:ALTSIGSTK-SIZE 4)
(define-const CPU-VECTOR:CPU-VECTOR-ADDRESS 5) ;reserved...?
(define-const CPU-VECTOR:LAST-INTERRUPT-VECTOR 6)
(define-const CPU-VECTOR:BOOT-LOADER-TYPE 7)
(define-const CPU-VECTOR:BOOT-LOADER-DATA 8)
(define-const CPU-VECTOR:SCHEDULER-RUNNING? 9)
(define-const CPU-VECTOR:CPU-NUMBER 10)

;; Indexes into the process vector (offset from the address stored in
;; CPU-VECTOR:PROCESS-VECTOR).
(define-const PROCESS-VECTOR:LENGTH -1)
(define-const PROCESS-VECTOR:ERROR-INVOKER 0)
(define-const PROCESS-VECTOR:START-CURRENT-HEAP 1)
(define-const PROCESS-VECTOR:SIZE-CURRENT-HEAP 2)
(define-const PROCESS-VECTOR:START-OTHER-HEAP 3)
(define-const PROCESS-VECTOR:SIZE-OTHER-HEAP 4)
(define-const PROCESS-VECTOR:STACK-TOP 5)
(define-const PROCESS-VECTOR:STACK-SIZE 6)
(define-const PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT 7)
;; The save area begins right after the stack.
(define-const PROCESS-VECTOR:SAVE-AREA PROCESS-VECTOR:STACK-TOP)

;; TODO: define the top-level offset... 16?

;; The amount of space reserved for saving the state of the process
;; when it is preempted. Stored above the top of the stack. On Linux
;; this should be larger than per-cpu-stack-size. On the PC this
;; should fit the FXSAVE/XSAVE image.
(define-const PROCESS-SAVE-SIZE 4096))
