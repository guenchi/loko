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

;;; Used in multiboot images to set up initial segment descriptors.

(library (loko arch amd64 pc-segments)
  (export
    data
    code-PL0
    data-PL0
    code-PL3
    data-PL3
    code-PL0-non-conforming
    task)
  (import
    (rename (rnrs) (bitwise-arithmetic-shift ash)))

(define stack-size 512)

;; Some useful segment type field values
(define seg-data #b0011)                ;read/write, accessed
(define seg-code-conforming #b1111)     ;execute/read, conforming,
                                        ;accessed
(define seg-code #b1011)                ;execute/read, accessed

;; Encodes a 64-bit code/data descriptor.
(define (segdesc/64 dpl type)
  (let ((desc (bitwise-ior (ash 1 (+ 32 12))    ;S=code or data
                           (ash 1 (+ 32 15))))) ;P=segment present
    (if (bitwise-bit-set? type 3)
        (bitwise-ior desc               ;code
                     (ash (bitwise-and #b1100 type) (+ 32 8))
                     (ash (bitwise-and #b11 dpl) (+ 32 13))
                     (ash 1 (+ 32 21))) ;L=64-bit segment
        (bitwise-ior desc
                     ;; #xffff           ;XXX:limit
                     ;; (bitwise-arithmetic-shift-left 1 (+ 32 12)) ;XXX:"1"
                     ;; (bitwise-arithmetic-shift-left 1 (+ 32 23)) ;XXX:G
                     ;; (bitwise-arithmetic-shift-left #xf (+ 32 16)) ;XXX:segment
                     ;; (bitwise-arithmetic-shift-left #x00c09300 32)  ;XXX:LINUX

                     ;; The DPL is supposed to be ignored?
                     (ash (bitwise-and #b11 dpl) (+ 32 13))
                     ;; Intel interestingly does not ignore the W flag
                     ;; here.
                     (ash (bitwise-and #b1010 type) (+ 32 8))))))

(define (print-desc/64 d)
  ;; Mildly useful for debugging broken segment descriptors.
  (define (print . x) (for-each display x) (newline))
  (define (print-tss)
    (let ((limit (bitwise-ior (bitwise-bit-field d 0 16)
                              (ash (bitwise-bit-field d 48 52) 16)))
          (base (bitwise-ior (bitwise-bit-field d 16 40)
                             (ash (bitwise-bit-field d (+ 32 24) 192) 40))))
      (print "Segment limit: #x" (number->string limit 16))
      (print "Base address: #x" (number->string base 16))))

  (let ((dpl (bitwise-bit-field d (+ 32 13) (+ 32 15)))
        (type (bitwise-bit-field d (+ 32 8) (+ 32 8 4))))

    (cond ((not (bitwise-bit-set? d (+ 32 15)))
           (print "Segment not present"))
          ((bitwise-bit-set? d (+ 32 12))
           (print "Code/data segment descriptor (64 bits wide), DPL=" dpl)
           (cond ((bitwise-bit-set? type 3)
                  (display "Code-segment, Execute")
                  (when (bitwise-bit-set? type 0) (display ", Accessed"))
                  (when (bitwise-bit-set? type 1) (display ", Readable"))
                  (when (bitwise-bit-set? type 2) (display ", Conforming"))
                  (newline))
                 (else
                  (print "Data-segment"))))
          (else
           (print "System descriptor (128 bits wide), DPL=" dpl)
           (case type
             ((#b0010) (print "64-bit LDT"))
             ((#b1001) (print "Available 64-bit TSS") (print-tss))
             ((#b1011) (print "Busy 64-bit TSS") (print-tss))
             ((#b1100) (print "64-bit call gate"))
             ((#b1110) (print "64-bit interrupt gate"))
             ((#b1111) (print "64-bit trap gate"))
             (else
              (print "Reserved system descriptor type: " (number->string type 2))))))))

;; (print-desc/64 0)
;; (print-desc/64 #x00008b100b0c006c)
;; (print-desc/64 #x00008910808c006c)
;; (print-desc/64 (segdesc/64 0 seg-code-conforming)) ; #x8, cs
;; (print-desc/64 (segdesc/64 0 seg-data))            ; #x10, ds, ss
;; (print-desc/64 (segdesc/64 3 seg-code-conforming)) ; #x18, cs for CPL=3
;; (print-desc/64 (segdesc/64 3 seg-data)) ; #x20, ds, ss, etc for CPL=3
;; (print-desc/64 (segdesc/64 0 seg-code)) ; #x28, cs, non-conforming for interrupts


;; Segment selectors. Load these into cs or ss, and the CPU goes to
;; the GDT to look up the details. The lower two bits are the
;; privilege level. Bit 2 is only used if there is an LDT, but there
;; isn't one.

;; The support for SYSCALL/SYSRET forces a certain layout here. The
;; selectors for CPL=3 must be in this order: SS, CS. For CPL=0 the
;; order is CS, SS. Do this and the processor will be pleased.
(define code-PL0                #x8)
(define data-PL0                #x10)
(define data-PL3                (+ #x18 3))
(define code-PL3                (+ #x20 3))
(define code-PL0-non-conforming #x28)
(define task                    #x30)

(define (make-lbl cpu)
  (define (lbl suffix)
    (string->symbol
     (string-append "cpu" (number->string cpu) "-" (symbol->string suffix))))
  lbl)

;; Defines cpu0-gdtr64, cpu1-gdtr64, etc.
(define (processor-state cpu)
  (define lbl (make-lbl cpu))
  `((%comm ,(lbl 'rsp0) ,stack-size 16)
    (%comm ,(lbl 'ist1) ,stack-size 16)
    (%comm ,(lbl 'ist2) ,stack-size 16)
    (%comm ,(lbl 'ist3) ,stack-size 16)
    (%comm ,(lbl 'ist4) ,stack-size 16)
    (%comm ,(lbl 'ist5) ,stack-size 16)
    (%comm ,(lbl 'ist6) ,stack-size 16)
    (%comm ,(lbl 'ist7) ,stack-size 16)

    ;; Task-state segment for the BSP
    (%align 8 0)
    (%u32 0)
    (%label ,(lbl 'tss))
    (%u32 0)                            ;Reserved
    (%u64 (+ ,(lbl 'rsp0) ,stack-size)) ;RSP0
    (%u64 0)                            ;RSP1
    (%u64 0)                            ;RSP2
    (%u64 0)                            ;Reserved
    (%u64 (+ ,(lbl 'ist1) ,stack-size)) ;IST1 (for NMI)
    (%u64 (+ ,(lbl 'ist2) ,stack-size)) ;IST2 (for #PF)
    (%u64 (+ ,(lbl 'ist3) ,stack-size)) ;IST3
    (%u64 (+ ,(lbl 'ist4) ,stack-size)) ;IST4
    (%u64 (+ ,(lbl 'ist5) ,stack-size)) ;IST5 (for PIC IRQ)
    (%u64 (+ ,(lbl 'ist6) ,stack-size)) ;IST6 (unknown int)
    (%u64 (+ ,(lbl 'ist7) ,stack-size)) ;IST7 (FIXME: misc)
    (%u64 0)                            ;Reserved
    (%u64 (bitwise-arithmetic-shift-left (- ,(lbl 'tss-end) ,(lbl 'tss)) 32))
    ;; The optional I/O permission map goes here.
    (%label ,(lbl 'tss-end))

    ;; Global descriptor table for the BSP. Each AP must allocate
    ;; its own GDT, which can be a copy of this one, but with a
    ;; different TSS entry.
    (%align 8 0)
    (%label ,(lbl 'gdt))
    (%u64 0
          ,(segdesc/64 0 seg-code-conforming) ; #x8, cs
          ,(segdesc/64 0 seg-data)            ; #x10, ss
          ,(segdesc/64 3 seg-data)            ; #x18, ss for CPL=3
          ,(segdesc/64 3 seg-code-conforming) ; #x20, cs for CPL=3
          ,(segdesc/64 0 seg-code))           ; #x28, cs for ints
    ;; #x30, tr: 64-bit TSS descriptor
    (%u128 (bitwise-ior (ash #b1 47)    ; P=Segment Present
                        (ash #b1001 40) ;Type=64-bit TSS
                        ;; Base Address (pointer to the TSS)
                        (ash (bitwise-bit-field ,(lbl 'tss) 0 24) 16)
                        (ash (bitwise-bit-field ,(lbl 'tss) 24 64) (+ 32 24))
                        ;; Segment limit (size of the TSS)
                        (ash (bitwise-and (ash (- ,(lbl 'tss-end) ,(lbl 'tss)) -16) #xf) 48)
                        (bitwise-and (- ,(lbl 'tss-end) ,(lbl 'tss)) #xffff)))
    (%label ,(lbl 'gdt-end))))

;; Global descriptor table pointer for use with lgdt
(define (gdt-pointer cpu)
  (define lbl (make-lbl cpu))
  `((%align 8 0)
    (%label ,(lbl 'gdtr64))
    (%u16 (- ,(lbl 'gdt-end) ,(lbl 'gdt) 1)) ;pointer to the last byte
    (%u64 ,(lbl 'gdt))))

(define (data max-cpus)
  `((%align 8 0)
    ;; 32-bit GDT to use during the switch to long mode. The 64-bit cs
    ;; here has to be in the same position as cs above.
    (%label gdt32)
    (%u64 0
          ;; #x8, cs
          ,(segdesc/64 0 seg-code-conforming)
          ;; #x10, ds, es, fs, gs, ss
          ,(bitwise-ior #xffff        ;limit
                        (ash #xf (+ 32 16))
                        ;; Type: data, read/write, accessed
                        (ash #b0011 (+ 32 8))
                        ;; S=code or data
                        (ash 1 (+ 32 12))
                        ;; P=segment present
                        (ash 1 (+ 32 15))
                        ;; D/B=32-bit segment
                        (ash 1 (+ 32 22))
                        ;; G=4K-4GB
                        (ash 1 (+ 32 23)))
          ;; #x18 cs for 32-bit code
          ,(bitwise-ior #xffff        ;limit
                        (ash #xf (+ 32 16))
                        ;; Type: code, execute-only, accessed
                        (ash #b1001 (+ 32 8))
                        ;; S=code or data
                        (ash 1 (+ 32 12))
                        ;; P=segment present
                        (ash 1 (+ 32 15))
                        ;; D/B=32-bit segment
                        (ash 1 (+ 32 22))
                        ;; G=4K-4GB
                        (ash 1 (+ 32 23))))
    (%label gdt32-end)

    ;; For use with lgdt.
    (%align 8 0)
    (%label gdtr32)
    (%u16 (- gdt32-end gdt32 1))      ;pointer to the last byte
    (%u32 gdt32)

    ;; One set of processor state per CPU
    ,@(let lp ((i 0))
        (if (= i max-cpus)
            '()
            (append (processor-state i) (lp (+ i 1)))))

    ;; A table of GDT pointers for use with lgdt. It must be possible
    ;; to get the right pointer by using CPU-VECTOR:CPU-NUMBER * 16.
    (%label gdtr-table)
    ,@(let lp ((i 0))
        (if (= i max-cpus)
            '()
            (append (gdt-pointer i) (lp (+ i 1)))))
    (%align 8 0))))
