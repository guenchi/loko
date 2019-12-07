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

;;; Convert traps to conditions

(library (loko arch amd64 traps)
  (export)
  (import
    (rnrs)
    (loko match)
    (only (loko system unsafe) get-mem-u8)
    (only (loko runtime control) register-error-invoker)
    (only (loko runtime conditions) make-program-counter-condition)
    (loko arch amd64 disassembler)
    (loko system $primitives))

(define (recover-explicit-condition live-data code-window)
  ;; Look at a few of the AMD64 instructions at the restart point of
  ;; an explicit trap. The code generator uses with-restart-handler
  ;; to get the program here via UD2. Nothing else uses UD2. No
  ;; clash with other generated code is therefore possible.
  (define (OVERFLOW? x) (memq x '(jo jno)))
  (define (BOUND? x) (memq x '(jb jnb jbe jnbe)))
  (define (EQ? x) (memq x '(jz jnz)))
  (define (assvio msg)
    (condition (make-assertion-violation)
               (make-message-condition msg)))
  (define (impvio msg)
    (condition (make-implementation-restriction-violation)
               (make-message-condition msg)))
  (define (irr c . irritants)
    (condition c (make-irritants-condition irritants)))
  ;; TODO: recover irritants
  (match code-window
    ;; Various overflows
    ([('neg . _) ((? OVERFLOW?) . _) . _]
     (irr (impvio "The result of (fx- (least-fixnum)) is not a fixnum")
          (least-fixnum)))
    ([('add . _) ((? OVERFLOW?) . _) . _]
     (impvio "Overflow in fx+"))
    ([('sub . _) ((? OVERFLOW?) . _) . _]
     (impvio "Overflow in fx-"))
    ([('imul . _) ((? OVERFLOW?) . _) . _]
     (impvio "Overflow in fx*"))
    ([('shl . _) ((? OVERFLOW?) . _) . _]
     (impvio "Overflow in fxarithmetic-shift-left or (fx* 2 ...)"))
    ;; Type checks
    ([_ ... ('test _ #b111) ((? EQ?) . _)]
     (assvio "Type error: expected a fixnum"))
    ([_ ... ('cmp _ #b11111) ((? EQ?) . _)]
     (assvio "Type error: expected a character"))
    ([_ ... ('cmp _ #x4f) ((? EQ?) . _)]
     (assvio "Type error: expected a flonum"))
    ;; Index checks
    ([('cmp reg ('mem64+ _ -6)) ((? BOUND?) . _) . _]
     ;; TODO: here reg should be inspected, because it might not
     ;; even be a fixnum.
     (assvio "The given vector index is out of range (or not a fixnum)"))
    ([('cmp reg ('mem64+ _ -4)) ((? BOUND?) . _) . _]
     (assvio "The given string index is out of range"))
    ;; Unicode scalar value.
    ;; #x3ff8 = immediate (- #xDFFF #xD800)
    ([('cmp reg #x3ff8) ((? BOUND?) . _) . _]
     (assvio "The given integer is in the Unicode High Surrogate Area"))
    ;; #x87FFF8 = immediate #x10FFFF
    ([('cmp reg #x87FFF8) ((? BOUND?) . _) . _]
     (assvio "The given integer is not a Unicode scalar value"))
    (_ #f)))

(define (recover-memory-condition live-data inst rip category closure)
  ;; The given instruction has caused #AC/#GP/#SS. Try to find out
  ;; what it was doing. TODO: use live-data to
  ;; to find the irritants
  ;; TODO: locate the source of the call
  (define (ret msg)
    (condition (make-assertion-violation)
               (make-message-condition msg)))
  (define (disp->type disp)
    (case (fx- 8 (bitwise-and disp #b111))
      ((#b001) "boxed object")
      ((#b010) "pair")
      ((#b011) "procedure")
      ((#b100) "string")
      ((#b110) "vector")
      ((#b101) "bytevector")
      ;; XXX: this is one of those impossible things
      (else "bad pointer")))
  (define (ref disp)
    (let ((type (disp->type disp)))
      (and type
           (condition
            (make-assertion-violation)
            (make-message-condition
             (string-append "Type error: expected a " type))
            (make-program-counter-condition rip)))))
  (define (set disp)
    (let ((type (disp->type disp)))
      (and type
           (condition
            (make-assertion-violation)
            (make-message-condition
             (if (eq? category 'page-fault)
                 (string-append "Attempted to mutate an immutable " type)
                 (string-append "Type error: expected to mutate a " type)))
            (make-program-counter-condition rip)))))
  (match inst
    (((or 'mov 'cmp) dst ('mem64+ r disp))
     (ref disp))
    (('cmp ('mem64+ r disp) op2)
     (ref disp))
    (('mov ('mem64+ r disp) src)
     (set disp))
    (('mov ('mem32+ r disp _) src)
     (set disp))
    (((or 'call 'jmp) ('mem64+ 'r15 disp))
     (and (equal? (disp->type disp) "procedure")
          (condition
           (make-assertion-violation)
           (make-who-condition 'apply)
           (make-message-condition "Tried to call a non-procedural object")
           (make-irritants-condition (list closure))
           (make-program-counter-condition rip))))
    (_ #f)))

;; See invoke-error in (loko arch amd64 lib).
(define (raise-trap category closure rip irritants)
  (define disasm-rip rip)
  (define (copy-inst addr)
    (and (fx<=? (* 2 1024 1024) addr (expt 2 32))
         (do ((ret (make-bytevector 15))
              (i 0 (fx+ i 1)))
             ((fx=? i (bytevector-length ret))
              ret)
           (bytevector-u8-set! ret i (get-mem-u8 (fx+ addr i))))))
  (define (get-instruction)
    ;; Disassembles the instruction at disasm-rip and increments
    ;; disasm-rip to point past that instruction.
    (let ((bytes (and (procedure? disassemble1)
                      (copy-inst disasm-rip))))
      (and bytes
           (disassemble1 bytes (lambda (tag . b*)
                                 (set! disasm-rip (fx+ disasm-rip (length b*))))))))
  (define (get-instructions-up-to-branch)
    ;; Start reading instructions from disasm-rip up to the branch
    ;; that jumped to rip.
    (let lp ((window '()) (i 6))
      (case i
        ((0) (reverse window))        ;stop early
        (else
         (let ((inst (get-instruction)))
           (match inst
             ((Jcc ('+ 'rip disp))
              (reverse (cons inst window)))
             (else
              (lp (cons inst window) (fx- i 1)))))))))
  (define (closure->who closure)
    ;; Returns &who or #f.
    (and (procedure? closure)
         (let ((info ($procedure-info closure)))
           (define (info? x) (and ($box? x) (eq? ($box-type x) 'info)))
           (define (info-name x) ($box-ref x 2))
           (and (info? info)
                (symbol? (info-name info))
                (make-who-condition
                 (string->symbol (symbol->string (info-name info))))))))
  (define (return c)
    (let ((who (closure->who closure)))
      (if who
          (let ((c* (simple-conditions c)))
            (apply condition (car c*) who (cdr c*)))
          c)))
  (define (generic type message . irritants)
    (return
     (condition type
                (make-message-condition message)
                (make-irritants-condition irritants)
                (make-program-counter-condition rip))))
  (define (translate-DE inst)
    (match inst
      (('idiv . _) "Division by zero")
      (_ #f)))
  (define (translate-explicit-trap)
    ;; An explicitly inserted trap instruction has been found
    ;; (currently UD2, maybe INT1 or INT3 in the future).
    (match (get-instruction)
      (('jmp ('+ 'rip disp))
       (set! disasm-rip (fx+ disasm-rip disp))
       (let ((window (get-instructions-up-to-branch)))
         (return (or (recover-explicit-condition #f window)
                     (condition (make-error)
                                (make-message-condition "An unrecognized error was trapped")
                                (make-irritants-condition window)
                                (make-program-counter-condition rip))))))
      (inst
       ;; If there is no jump back there's something wrong. TODO: the
       ;; jump back isn't strictly needed, because the location of
       ;; the branch can be found by disassembling the whole
       ;; function.
       (generic (make-error) "It has all gone terribly wrong" inst))))
  (define (find-condition)
    (case category
      ;; Software-triggered.
      ((formals)
       (return (condition
                (make-assertion-violation)
                (make-message-condition
                 "This procedure has been called with the wrong number of arguments")
                (make-irritants-condition irritants))))
      ((undefined)
       ;; XXX: disassemble could result in an &invalid-opcode
       ;; condition if the instruction *really* is invalid
       (let ((inst (get-instruction)))
         (cond ((equal? inst '(ud2))
                (translate-explicit-trap))
               (else
                (generic (make-error)
                         "The program has used an unsupported CPU instruction" inst)))))
      ;; Hardware triggered
      ((alignment noncanonical page-fault)
       (let ((inst (get-instruction)))
         (return (or (and inst (recover-memory-condition #f inst rip category closure))
                     (condition (make-assertion-violation)
                                (make-message-condition
                                 "An unrecognized condition was detected by the hardware")
                                (make-irritants-condition (list inst))
                                (make-program-counter-condition rip))))))
      ((accvio)
       ;; TODO
       (generic (make-error) "The hardware has detected an error" category))
      ((divide)
       ;; This is #DE.
       (let* ((inst (get-instruction))
              (msg (and inst (translate-DE inst))))
         (if msg
             (generic (make-assertion-violation) msg)
             (generic (make-assertion-violation)
                      "The program performed an undefined mathematical operation"
                      inst))))
      (else
       (generic (make-error)
                "There was an error detected at the low level" category))))
  ;; TODO: where should the flush really be?
  ;; (stack-trace)                       ;TODO: bottle up the stack trace!
  (raise (find-condition)))

(register-error-invoker raise-trap))
