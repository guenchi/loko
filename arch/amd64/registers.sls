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

;;; A collection and documentation of architectural register numbers,
;;; bits, fields, etc.

(library (loko arch amd64 registers)
  (export
    CR0 CR3 CR4 RFLAGS RFLAGS-IOPL
    MSR EFER

    APIC-ICR-DSH
    APIC-ICR-RRS
    APIC-ICR-MT
    APIC-ICR-VEC
    APIC-ICR
    APIC-LVT)
  (import
    (rnrs))

(define-syntax define-inlined
  (lambda (x)
    (syntax-case x ()
      ((_ name v)
       (identifier? #'name)
       #'(define-syntax name (identifier-syntax v)))
      ((_ (name args ...) v)
       #'(define-syntax name (identifier-syntax (lambda (args ...) v)))))))

(define-syntax define-bits
  (lambda (x)
    (syntax-case x ()
      ((_ register . rest)
       (identifier? #'register)
       (let lp ((rest #'rest) (acc '()) (mnemonics '()))
         (with-syntax ((acc acc) (mnemonics mnemonics))
           (syntax-case rest ()
             ((bit mnemonic . rest)
              (lp #'rest #'((bit mnemonic) . acc) #'(mnemonic . mnemonics)))
             (()
              (syntax-case #'acc ()
                (((bit mnemonic) ...)
                 #'(define-syntax register
                     (lambda (x)
                       (syntax-case x mnemonics
                         (( _ mnemonic . x)
                          #'(bitwise-ior (bitwise-arithmetic-shift-left 1 bit)
                                         (register . x)))
                         ...
                         ((_) 0))))))))))))))

;; Reserved bits should be preserved.
(define-bits CR0
  31    PG       ;Paging
  30    CD       ;Cache Disable
  29    NW       ;Not Writethrough
  18    AM       ;Alignment Mask
  16    WP       ;Write Protect
  5     NE       ;Numeric Error
  4     ET       ;Extension Type
  3     TS       ;Task Switched
  2     EM       ;Emulation
  1     MP       ;Monitor Coprocessor
  0     PE)      ;Protection Enabled

;; Used to set the level 4 page map table
(define-bits CR3
  4 PCD         ; Page-level Cache Disable
  3 PWT)        ; Page-level Write-Through

(define-bits CR4
  18 OSXSAVE    ;XSAVE and Processor Extended States Enable Bit
  10 OSXMMEXCPT ;Operating System Unmasked Exception Support
  9  OSFXSR     ;Operating System FXSAVE/FXRSTOR Support
  8  PCE        ;Performance-Monitoring Counter Enable
  7  PGE        ;Page-Global Enable
  6  MCE        ;Machine Check Enable
  5  PAE        ;Physical-Address Extension
  4  PSE        ;Page Size Extensions
  3  DE         ;Debugging Extensions
  2  TSD        ;Time Stamp Disable
  1  PVI        ;Protected-Mode Virtual Interrupts
  0  VME)       ;Virtual-8086 Mode Extensions

(define-bits RFLAGS
  21    ID       ;ID Flag
  20    VIP      ;Virtual Interrupt Pending
  19    VIF      ;Virtual Interrupt Flag
  18    AC       ;Alignment Check
  17    VM       ;Virtual-8086 Mode
  16    RF       ;Resume Flag
  14    NT       ;Nested Task
  ;;13-12 IOPL   ;I/O Privilege Level
  11    OF       ;Overflow Flag
  10    DF       ;Direction Flag
  9     IF       ;Interrupt Flag
  8     TF       ;Trap Flag
  7     SF       ;Sign Flag
  6     ZF       ;Zero Flag
  4     AF       ;Auxiliary Flag
  2     PF       ;Parity Flag
  0     CF)      ;Carry Flag

(define (RFLAGS-IOPL x) (fxarithmetic-shift-left (fxand x #b11) 12))

;; Model-Specific Registers (MSRs). They are used with the rdmsr or
;; wrmsr instructions. Load the number into ecx and the data into
;; edx:eax. Some of them are not model-specific, they are
;; architectural.

;; The names are from Intel's IA32 SDM Vol 3b Appendix B
(define-syntax MSR
  (lambda (x)
    (syntax-case x (IA32_APIC_BASE
                    IA32_MPERF IA32_APERF
                    IA32_PAT
                    IA32_EFER IA32_STAR IA32_LSTAR IA32_FMASK IA32_FS_BASE
                    IA32_GS_BASE IA32_KERNEL_GS_BASE)
      ((_ IA32_APIC_BASE) #x1B)
      ((_ IA32_MPERF) #xE7)           ;Maximum performance
      ((_ IA32_APERF) #xE8)           ;Actual performance
      ((_ IA32_PAT) #x277)
      ((_ IA32_EFER) #xC0000080)      ;Extended Feature Enables
      ((_ IA32_STAR) #xC0000081)      ;System Call Target Address
      ((_ IA32_LSTAR) #xC0000082) ;IA-32e Mode System Call Target Address
      ((_ IA32_FMASK) #xC0000084) ;System Call Flag Mask
      ((_ IA32_FS_BASE) #xC0000100)   ;Map of BASE Address of FS
      ((_ IA32_GS_BASE) #xC0000101)   ;Map of BASE Address of GS
      ((_ IA32_KERNEL_GS_BASE) #xC0000102) ;Swap Target of BASE Address of GS
      )))

(define-bits EFER
  14  FFXSR         ;Fast FXSAVE/FXRSTOR
  13  LMSLE         ;Long Mode Segment Limit Enable
  12  SVME          ;Secure Virtual Machine Enable
  11  NXE           ;No-Execute Enable
  10  LMA           ;Long Mode Active
  8   LME           ;Long Mode Enable
  0   SCE)          ;System Call Extensions

;;; APIC

(define-bits APIC_BASE                ;in (MSR IA32_APIC_BASE)
  ;;51-12 ABA      APIC Base Address
  11    AE       ;APIC Enable
  10    x2APIC   ;x2APIC mode
  8     BSC)     ;Boot Strap CPU Core

;; Advanced Programmable Interrupt Controller registers

#;
(define-syntax APIC
  (lambda (x)
    (syntax-case x (ID
                    version
                    TPR EOI DFR
                    SVR
                    ICR-low ICR-high
                    LVT-timer LVT-thermal LVT-perfcnt
                    LVT-lint0 LVT-lint1 LVT-error
                    timer-initial
                    timer-current
                    timer-divide
                    IER)
      ((_ ID) #x20)
      ((_ version) #x30)
      ((_ TPR) #x80)      ;Task Priority Register
      ((_ EOI) #xB0)      ;End Of Interrupt
      ((_ DFR) #xE0)      ;Destination Format
      ((_ SVR) #xF0)      ;Spurious Interrupt Vector + enable
      ((_ ICR-low) #x300)
      ((_ ICR-high) #x310)
      ((_ LVT-timer) #x320)
      ((_ LVT-thermal) #x330)
      ((_ LVT-perfcnt) #x340)
      ((_ LVT-lint0) #x350)
      ((_ LVT-lint1) #x360)
      ((_ LVT-error) #x370)
      ((_ timer-initial) #x380)
      ((_ timer-current) #x390)
      ((_ timer-divide) #x3E0)
      ;; APIC Extended Space...
      ((_ IER) #x480)          ;Interrupt Enable Registers
      )))

(define-bits APIC_SVR                 ;in (APIC SVR)
  9 FCC                               ;Focus CPU Core Checking
  8 ASE                               ;APIC enabled
  ;; 7-0 VEC
  )

(define-inlined (APIC-ICR-DSH x) (fxarithmetic-shift-left (fxand x #b11) 18))
(define-inlined (APIC-ICR-RRS x) (fxarithmetic-shift-left (fxand x #b11) 16))
(define-inlined (APIC-ICR-MT x) (fxarithmetic-shift-left (fxand x #b111) 8))
(define-inlined (APIC-ICR-VEC x) (fxand (fxarithmetic-shift-right x 12) #b11111111))

(define-bits APIC-ICR
  15 TGM      ;Trigger Mode
  14 L        ;Level
  12 DS       ;Delivery Status
  11 DM       ;Destination Mode
  )

(define-bits APIC-LVT
  17   TMM      ;Timer Mode
  16   M        ;Mask
  15   TGM      ;Trigger Mode
  14   RIR      ;Remote IRR
  12   DS       ;Delivery Status
  ))
