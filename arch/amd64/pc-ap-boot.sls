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

;;; Boot code for Application Processors

(library (loko arch amd64 pc-ap-boot)
  (export
    boot-application-processors)
  (import
    (rnrs (6))
    (machine-code assembler x86)
    (loko arch amd64 registers)
    (loko system unsafe)
    (only (loko system $primitives) $linker-address))

(define (ap-init-code ap-start)
  ;; This code is run by the application processors, i.e. all the
  ;; other processors in the system. They start in real mode and the
  ;; goal is simple: enter protected mode and jump to multiboot:start.
  ;; The code has to be located on a page-aligned address in the first
  ;; 1MB of RAM.
  `((%origin ,ap-start)
    (%mode 16)
    (mov al #xb3) (out #x80 al)         ;port80h debugging
    ;; XXX: are cs and ds the same?
    (lgdt (mem+ gdtr))
    (mov ax ,(CR0 PE))
    (lmsw ax)                           ;protected mode

    (jmpf (far #x8 final-jump))         ;jump to a 32-bit code segment
    (%mode 32)
    (%label final-jump)

    (mov eax #x10)
    (mov ds ax)
    (mov es ax)
    (mov fs ax)
    (mov gs ax)
    (mov ss ax)

    (mov eax #x2BADB002)                ;multiboot magic
    (xor ebx ebx)                       ;information structure
    (jmp ,($linker-address 'multiboot:start))

    ;; Global descriptor table pointer
    (%align 8 0)
    (%label gdtr)
    (%u16 (- gdt-end gdt 1))            ;pointer to the last byte
    (%u64 gdt)

    ;; Global descriptor table
    (%align 8 0)
    (%label gdt)
    (%u64 0
          ;; #x8, cs
          (bitwise-ior #xffff          ;limit
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
                        (ash 1 (+ 32 23)))
          ;; #x10, ds, es, fs, gs, ss
          (bitwise-ior #xffff          ;limit
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
                        (ash 1 (+ 32 23))))
    (%label gdt-end)))

(define (boot-application-processors &code-page &APIC:ICR sleep)
  ;; XXX: bug somewhere (hashtables?) causes assembler to stop too soon
  (let-values ([(code symbols) (assemble (ap-init-code &code-page))])
    ;; Copy the code to code-page
    (do ((i 0 (fx+ i 1)))
        ((fx=? i (bytevector-length code)))
      (put-mem-u8 (fx+ &code-page i) (bytevector-u8-ref code i)))
    ;; Do the INIT-SIPI-SIPI dance.
    (let ((INIT (fxior (APIC-ICR-DSH #b11) ;To all excluding self
                       (APIC-ICR-MT #b101) ;Delivery mode: INIT
                       (APIC-ICR L)))
          (SIPI (fxior (fxior (APIC-ICR-DSH #b11) ;To all excluding self
                              (APIC-ICR-MT #b110) ;Delivery mode: Start Up
                              (APIC-ICR L))        ;Level: assert
                       (APIC-ICR-VEC &code-page))))
      (put-mem-u32 &APIC:ICR INIT)
      (sleep #e0.01)
      (put-mem-u32 &APIC:ICR SIPI)
      (sleep #e0.0002)
      (put-mem-u32 &APIC:ICR SIPI)

      #;(sleep #e0.0002)

      ;; Now it might be prudent to either wait 100ms for all APs to
      ;; appear and/or wait for (APIC-LVT DS) to be cleared.
      ))))
