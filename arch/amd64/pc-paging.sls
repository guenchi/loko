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

;;; Used in multiboot images to set up the initial paging table.

(library (loko arch amd64 pc-paging)
  (export
    text data)
  (import
    (loko arch amd64 registers)
    (only (loko arch amd64 memory)
          STACK-SIZE HEAP-SIZE)
    (rename (rnrs) (bitwise-arithmetic-shift ash)))

;; The virtual memory map:
;; 00000000-0009FFFF [640KB] available memory (*)
;; 000A0000-000BFFFF [128KB] VGA memory (write-through, NX)
;; 000C0000-000DFFFF [128KB] Add-in card BIOS memory (read-only, NX)
;; 000E0000-000EFFFF [64KB] Lower BIOS area (read-only, NX)
;; 000F0000-000FFFFF [64KB] Upper BIOS area (read-only, NX)
;; 00100000-FFFFFFFF [2MB-4GB] available memory, PCI devices etc (**)
;; All other addresses are non-present at startup. The Scheme code
;; must manage page tables himself to access the rest of the memory.

;; (*) The multiboot parameter mem_lower sets the upper range here.
;; (**) Same as above, but mem_upper. Check the e820 memory map.

;; Flags available in all levels of the page table hierarchy
(define page-P   (ash 1 0))             ;present
(define page-R/W (ash 1 1))             ;read/write
(define page-U/S (ash 1 2))             ;user/supervisor
(define page-PWT (ash 1 4))             ;page-level writethrough(*)
(define page-PCD (ash 1 4))             ;page-level cache disable(*)
(define page-A   (ash 1 5))             ;accessed
(define page-NX  (ash 1 63)) ;no-execute (must be enabled in EFER, check CPUID etc)

;; Flags available only in the lowest level of the page table
(define page-D       (ash 1 6))         ;dirty
(define pte-PAT      (ash 1 7))         ;page-attribute table(*)
(define pde/pdpe-PAT (ash 1 12))        ;page-attribute table(*)

;; Flags available only at the PDE level, and the PDPE level if
;; support for 1GB pages is available.
(define page-PS (ash 1 7))              ;page size

;; (*) = the PAT, PCD, PWT bits are combined into a 3 bit field that
;; points at an entry in the page-attribute table. The values returned
;; below are for 4KB pages. See "Page-Attribute Table Mechanism" in
;; the AMD64 programming manual, volume 2.
(define (pte-PA n)
  (bitwise-ior (if (fxbit-set? n 0) page-PWT 0)
               (if (fxbit-set? n 1) page-PCD 0)
               (if (fxbit-set? n 2) pte-PAT 0)))
(define PA-writeback (pte-PA 0))
(define PA-write-through (pte-PA 1))
(define PA-uncacheable-minus (pte-PA 2))
(define PA-uncacheable (pte-PA 3))
(define PA-write-combining (pte-PA 5))
(define PA-write-protect (pte-PA 6))

;; PAT type encodings that are loaded into the PAT register
(define PAT-UC 0)
(define PAT-WC 1)
(define PAT-WT 4)
(define PAT-WP 5)
(define PAT-WB 6)
(define PAT-UC- 7)

(define (make-first-1GB-pde)
  ;; The first 2MB is given special treatment and points to pte0,
  ;; which contains 4KB pages.
  (do ((pde (make-bytevector (- 4096 8)))
       (i 0 (+ i 8))
       (base (bitwise-ior page-P page-R/W page-U/S page-A page-D page-PS
                          #x200000)
             (+ base #x200000)))
      ((= i (- 4096 8))
       `((%u64 (+ pte0 ,(bitwise-ior page-P page-R/W page-U/S page-A)))
         (%vu8 ,pde)))
    (bytevector-u64-set! pde i base (endianness little))))

(define (make-first-2MB-pte)
  (do ((pte (make-bytevector 4096))
       (i 0 (+ i 8))
       (base 0 (+ base #x1000)))
      ((= i 4096) pte)
    (let ((e (cond ((< base #x9FFFF)    ;normal memory
                    (bitwise-ior base page-P page-R/W page-U/S page-A page-D))
                   ((< base #xBFFFF)    ;VGA memory
                    (bitwise-ior base
                                 PA-write-combining
                                 page-P page-R/W page-U/S page-A page-D page-NX))
                   ((< base #xFFFFF)    ;BIOS
                    (bitwise-ior base page-P page-U/S page-A page-NX))
                   (else
                    (bitwise-ior base page-P page-R/W page-U/S page-A page-D)))))
      (bytevector-u64-set! pte i e (endianness little)))))

(define (make-pde base)
  ;; Make a page descriptor entry table and fill it with addresses.
  ;; Each 64-bit entry represents 2MB of virtual address space, since
  ;; PS=1. See the chapter on "Long-Mode Page Translation" in the
  ;; AMD64 programming manual, volume 2.
  (do ((pde (make-bytevector 4096))
       (i 0 (+ i 8))
       (base (bitwise-ior base page-P page-R/W page-U/S page-A page-D page-PS)
             (+ base #x200000)))
      ((= i 4096) pde)
    (bytevector-u64-set! pde i base (endianness little))))

(define (make-stack-area-pte base)
  ;; Builds a list of 4KB pages. The first page is not present.
  (let ((flags (bitwise-ior page-P page-R/W page-U/S page-A page-D))
        (base* `(,base)))
    (let lp ((i (- (* 2 1024 1024) 4096))
             (ret '()))
      (if (negative? i)
          (if (eq? base 'area0-stack0)
              `(0                     ;not present
                ,@(cdr ret))
              ret)
          (lp (- i 4096)
              `((+ ,(+ flags i) ,@base*)
                ,@ret))))))

(define (make-heap-area-pte base)
  ;; Builds a list of 4KB pages. The first page is not present.
  (let ((flags (bitwise-ior page-P page-R/W page-U/S page-A page-D))
        (base* `(,base)))
    (let lp ((i (- (* 2 1024 1024) 4096))
             (ret '()))
      (if (negative? i)
          ret
          (lp (- i 4096)
              `((+ ,(+ flags i) ,@base*)
                ,@ret))))))

(define (text)
  ;; Set the meaning of the PAT bits. The zero encoding should
  ;; always be WB. Runs in 32-bit mode. Must not clobber EBX.
  `((mov ecx ,(MSR IA32_PAT))
    (mov eax ,(bitwise-ior (ash PAT-WB 0)    ;PA0
                           (ash PAT-WT 8)    ;PA1
                           (ash PAT-UC- 16)  ;PA2
                           (ash PAT-UC 24))) ;PA3
    (mov edx ,(bitwise-ior (ash PAT-WB 0)    ;PA4
                           (ash PAT-WC 8)    ;PA5
                           (ash PAT-WP 16)   ;PA6
                           (ash PAT-UC 24))) ;PA7
    (wrmsr)))                         ;load page attribute table

(define (data)
  (assert (= STACK-SIZE (* 2 1024 1024)))
  (assert (= HEAP-SIZE (* 16 1024 1024)))
  `( ;; Page map level 4 table (address bits 47-39). Each entry is
    ;; 512GB.
    (%align 4096 0)
    (%label pml4)                     ;CR3 points at this
    (%u64 (+ pdpt ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 0)
    (%u64 (+ pdpt-areas ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))

    ;; Page directory pointer table (address bits 38-30). 1GB per entry.
    (%align 4096 0)
    (%label pdpt)
    (%u64 (+ pde0 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pde1 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pde2 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pde3 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))

    ;; Page directory table (address bits 29-21).
    (%align 4096 0)
    (%label pde0) ,@(make-first-1GB-pde)
    (%label pde1) (%vu8 ,(make-pde (* 1 #x40000000)))
    (%label pde2) (%vu8 ,(make-pde (* 2 #x40000000)))
    (%label pde3) (%vu8 ,(make-pde (* 3 #x40000000)))

    ;; Page table (address bits 20-12), first 2MB only
    (%align 4096 0)
    (%label pte0) (%vu8 ,(make-first-2MB-pte))

;;; The thread areas. Initially only area #0 is mapped. Starts at 1TB.

    (%align 4096 0)
    (%label pdpt-areas)               ;1GB entries.
    (%u64 (+ pde-areas ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))

    (%align 4096 0)                   ;2MB entries.
    (%label pde-areas)
    (%u64 (+ pte-stack0 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pte-heap0 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pte-heap1 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pte-heap2 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pte-heap3 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pte-heap4 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pte-heap5 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pte-heap6 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))
    (%u64 (+ pte-heap7 ,(bitwise-ior page-P page-R/W page-U/S page-A page-D)))

    ;; 4KB entries.
    (%align 4096 0)
    (%label pte-stack0) (%u64 ,@(make-stack-area-pte 'area0-stack0))
    (%label pte-heap0) (%u64 ,@(make-heap-area-pte 'area0-heap0))
    (%label pte-heap1) (%u64 ,@(make-heap-area-pte 'area0-heap1))
    (%label pte-heap2) (%u64 ,@(make-heap-area-pte 'area0-heap2))
    (%label pte-heap3) (%u64 ,@(make-heap-area-pte 'area0-heap3))
    (%label pte-heap4) (%u64 ,@(make-heap-area-pte 'area0-heap4))
    (%label pte-heap5) (%u64 ,@(make-heap-area-pte 'area0-heap5))
    (%label pte-heap6) (%u64 ,@(make-heap-area-pte 'area0-heap6))
    (%label pte-heap7) (%u64 ,@(make-heap-area-pte 'area0-heap7))

    ;; Physical memory for stack area #0 and heap area #0. This
    ;; memory is taken from bss because the multiboot spec makes it
    ;; hard to take any other memory. XXX: one page is is unmapped
    ;; wasted on the stack, and the last page is the process save
    ;; area.
    (%comm area0-stack0 ,(* 2 1024 1024) 4096)
    (%comm area0-heap0 ,(* 2 1024 1024) 4096)
    (%comm area0-heap1 ,(* 2 1024 1024) 4096)
    (%comm area0-heap2 ,(* 2 1024 1024) 4096)
    (%comm area0-heap3 ,(* 2 1024 1024) 4096)
    (%comm area0-heap4 ,(* 2 1024 1024) 4096)
    (%comm area0-heap5 ,(* 2 1024 1024) 4096)
    (%comm area0-heap6 ,(* 2 1024 1024) 4096)
    (%comm area0-heap7 ,(* 2 1024 1024) 4096))))
