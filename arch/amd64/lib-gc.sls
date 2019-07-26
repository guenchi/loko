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

;;; Stop-and-copy garbage collector

(library (loko arch amd64 lib-gc)
  (export
    lib-gc:text lib-gc:data)
  (import
    (loko arch amd64 objects)
    (loko libs context)
    (rnrs (6)))

(define (pvec i) `(,(+ 8 (* 8 i) (- (tag 'vector)))))
(define %heap-rem 'r13)
(define %alloc 'r14)
(define %closure 'r15)

;; Register usage in this file:

;; r12 - scan pointer
;; r13 - heap remaining
;; r14 - allocation pointer
;; r15 - current closure

(define (lib-gc-stop-and-copy)
  (define gc-stack (vector 'gc-stack))
  `((%align 8)
    (%label stop-and-copy relocate)

    ;; Must preserve rsp and relocate r15, rbx and rbp. Must update
    ;; r13 and r14. The stack is split in one area that only contains
    ;; live references and another area where the NOPs after the
    ;; return addresses determine what parts of the stack frame are
    ;; live. Everything else which may be of interest must have been
    ;; saved on the stack. The stuff which is preserved here must
    ;; match what analyzer.scm has.
    (push ,%closure)
    (push rbx)
    (push rbp)

    ;; Remove anything on the stack that isn't live. The caller sets
    ;; rdi to point right after the first return address where the
    ;; stack tracing mechanism will work.
    (mov r8 (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (lea rbx (mem+ rdi -8))
    (mov rbp (mem64+ r8 ,@(pvec PROCESS-VECTOR:STACK-TOP)))
    (call cleanup-stack)

    ;; (mov rdi ,(char->integer #\G))
    ;; (call (mem64+ *debug-put-u8))

    ;; Swap the heaps.
    ;; (mov r8 (mem64+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (mov rax (mem64+ r8 ,@(pvec PROCESS-VECTOR:START-CURRENT-HEAP)))
    (mov rbx (mem64+ r8 ,@(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)))
    (mov rcx (mem64+ r8 ,@(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    (mov rdx (mem64+ r8 ,@(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    (mov (mem64+ r8 ,@(pvec PROCESS-VECTOR:START-CURRENT-HEAP)) rcx)
    (mov (mem64+ r8 ,@(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)) rdx)
    (mov (mem64+ r8 ,@(pvec PROCESS-VECTOR:START-OTHER-HEAP)) rax)
    (mov (mem64+ r8 ,@(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)) rbx)

    ;; Increment the garbage collection counter.
    (add (mem64+ r8 ,@(pvec PROCESS-VECTOR:GARBAGE-COLLECTION-COUNT))
         ,(immediate 1))

    ;; The scan pointer moves from the start of the new heap and
    ;; tries to catch up with the free pointer.
    (mov r12 (mem64+ r8 ,@(pvec PROCESS-VECTOR:START-CURRENT-HEAP)))

    ;; Set the remaining heap space to be the top of the heap
    ;; minus the requested memory amount. Later on this is
    ;; adjusted to be exactly the amount of space remaining in
    ;; the new heap.
    (mov rdx (mem64+ r8 ,@(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    (add rdx (mem64+ r8 ,@(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    (sub rdx ,%alloc)               ;rdx = space remaining in old heap
    (add ,%heap-rem r12)
    (add ,%heap-rem (mem64+ r8 ,@(pvec PROCESS-VECTOR:SIZE-CURRENT-HEAP)))
    (sub ,%heap-rem rdx)

    ;; Allocate stuff at the start of the new heap.
    (mov ,%alloc r12)

    ;; Relocate all locals on the stack.
    (mov r9 rsp)
    (mov r10 (mem64+ r8 ,@(pvec PROCESS-VECTOR:STACK-TOP)))
    (%align 16)
    (%label ,gc-stack)
    (mov rax (mem+ r9))
    (call relocate)                     ;relocate a local
    (mov (mem+ r9) rax)
    (add r9 8)
    ;;(call gc-loop)
    (cmp r9 r10)
    (jb ,gc-stack)

    ;; Relocate the processor vector.
    (mov rax (mem+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)))
    (call relocate)
    (mov (mem+ fs ,(* 8 CPU-VECTOR:PROCESS-VECTOR)) rax)
    (mov r8 rax)

    (call gc-loop)

    ;; Clear the old heap. TODO: if all constructors were very
    ;; careful it wouldn't be necessary to clear the old heap.
    ;; This would mean that the space between objects (the
    ;; alignment padding) would need to be filled in.
    (mov rdi (mem64+ r8 ,@(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    (mov rcx (mem64+ r8 ,@(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    (sar rcx 3)                         ;quads
    (xor eax eax)
    (rep.stos (mem64+ rdi) rax)

    ;; Tell the program how much is left and check that
    ;; there's enough space in the new heap for the object
    ;; that was being allocated. TODO: if there is enough
    ;; memory free in this heap (maybe the request is crazy
    ;; big), but the heap should not grow, then just restore
    ;; %heap-rem and raise a condition.
    (sub ,%heap-rem ,%alloc)
    (js heap-overflow)

    (pop rbp)
    (pop rbx)
    (pop ,%closure)
    (ret)))

(define (lib-gc-relocate)
  (define relocate-copy (vector 'relocate-copy))
  (define relocate-ret (vector 'relocate-ret))
  `((%align 8)
    (%label relocate gc-loop)
    ;; rax is an object which will be relocated and returned
    ;; in rax. If it was found on the stack it might also be a
    ;; return address. No return address can be located inside
    ;; the stop-and-copy areas, so these addresses are never
    ;; dereferenced here.
    (mov ebx eax)
    (and ebx #b111)
    (dec ebx)                           ;rbx = tag minus one
    (cmp ebx 5)
    (ja ,relocate-ret)                  ;return if tag is outside the
                                        ;range #b001-#b110
    (mov rsi rax)
    (sub rsi (mem64+ r8 ,@(pvec PROCESS-VECTOR:START-OTHER-HEAP)))
    (cmp rsi (mem64+ r8 ,@(pvec PROCESS-VECTOR:SIZE-OTHER-HEAP)))
    (ja ,relocate-ret)                  ;return if object outside heap
    (mov rsi rax)                       ;source = old object
    (and rsi ,(fxnot #b111))            ;unmask tag
    (cmp (mem64+ rsi) ,(tag 'moved-mark)) ;object already moved?
    (jne ,relocate-copy)
    (mov rax (mem+ rsi 8))              ;return new location
    (ret)
    (%label ,relocate-copy)                 ;move the object
    (call (mem64+ object-sizers (* rbx 8))) ;set rcx, given rax
    (inc rcx)
    (and rcx -2)                        ;always aligned to 16 bytes
    (mov rdi ,%alloc)                   ;copy into the free memory
    (rep.movs (mem64+ rdi) (mem64+ rsi))
    (mov rcx rax)
    (and rcx ,(fxnot #b111))
    (mov (mem64+ rcx) ,(tag 'moved-mark)) ;mark as moved
    (lea rax (mem+ ,%alloc rbx 1))
    (mov (mem64+ rcx 8) rax)            ;store forwarding pointer
    (mov ,%alloc rdi)
    (%label ,relocate-ret)
    (ret)))

(define (lib-gc-gc-loop)
  (define seek (vector 'seek))
  (define exit (vector 'exit))
  (define fake (vector 'fake))
  (define panic (vector 'panic))
  (assert (eqv? (bitwise-bit-field (mask 'seek-mark) 0 60) #xffff))
  (assert (eqv? (mask 'box-header) #xffff))
  `((%align 16)
    (%label gc-loop box-sizer)
    ;; This routine treats the new heap as a queue. scan points to the
    ;; start of the queue. Its job is to relocate each cell in every
    ;; object which has already been relocated.
    (cmp r12 ,%alloc)
    (je ,exit)
    (mov rax (mem+ r12))
    (cmp ax ,(bitwise-and (tag 'seek-mark) #xffffffff))
    (je ,seek)                    ;seek mark?
    (call relocate)
    (mov (mem+ r12) rax)
    (add r12 8)
    (jmp gc-loop)
    (%label ,seek)
    ;; The object under the scan pointer looks like a seek mark. It
    ;; could be a return address from a stack copied to the heap, so
    ;; seek marks are also tagged with high bits so that they are
    ;; non-canonical.
    (shl rax 1)
    (jnc ,fake)
    (shr rax ,(+ (shift 'seek-mark) 1))
    (add r12 rax)                 ;seek over non-references
    (cmp r12 ,%alloc)
    (ja ,panic)                   ;scan > free?
    (jne gc-loop)
    (%label ,exit)
    (ret)
    (%label ,fake)
    ;; It can't be a real seek mark, so ignore it.
    (add r12 8)
    (jmp gc-loop)
    (%label ,panic)
    ;; The scan pointer is above the free pointer. Caused by an
    ;; invalid seek mark.
    (mov rdi ,(immediate 'bad-seek))
    (call (mem64+ *panic))))

;; These functions set rcx to the number of 8-byte words occupied by
;; the object in rax.

(define (lib-gc-object-sizer)
  (define boxhdr (vector 'boxhdr))
  (define bignum (vector 'bignum))
  (define record (vector 'record))
  (define fixup (vector 'fixup))
  (define k (vector 'k))
  `((%align 8)
    (%label box-sizer pair-sizer)
    ;; Boxes are either built-in types (the first field is an
    ;; immediate symbol), in which case the second field is a length
    ;; field (except for bignums which have a fixed size), or record
    ;; types. Record types have an rtd (boxed value) as their first
    ;; field. The rtd contains a field that gives the number of fields
    ;; in the record itself. Most recently a new box header type was
    ;; created that contains an explicit length field, counted in
    ;; multiple of eight.
    (mov rcx (mem+ rax ,(fx- (tag 'box))))
    (cmp cx ,(tag 'box-header))
    (je ,boxhdr)                        ;box header

    (mov rcx (mem+ rax ,(fx- (tag 'box))))
    (and ecx ,(mask 'box))
    (cmp ecx ,(tag 'box))
    (je ,record)                       ;records have the length in rtd

    ;; TODO: Remove all need for this case
    (mov rcx (mem+ rax 8 ,(fx- (tag 'box))))
    (shr rcx ,(shift 'fixnum))
    (add rcx 2)                         ;type + length
    (ret)

    (%label ,record)
    (mov rcx (mem+ rax ,(fx- (tag 'box)))) ;get rtd
    (cmp ecx ,(tag 'moved-mark))
    (je ,fixup)

    (%label ,k)
    (mov rcx (mem+ rcx 8 8 ,(fx- (tag 'box))))
    (shr rcx ,(shift 'fixnum))
    (inc ecx)                           ;32-bit mask, and type field
    (ret)

    ;; Box header
    (%label ,boxhdr)
    (shr rcx ,(shift 'box-header:length))
    (inc rcx)                           ;+ box header
    (ret)

    (%label ,fixup)                     ;FIXME
    (mov rdi ,(immediate 'todo))
    (call (mem64+ *panic))
    (jmp ,fixup)))

(define (lib-gc-pair-sizer)
  '((%align 8)
    (%label pair-sizer procedure-sizer)
    (mov ecx 2)
    (ret)))

(define (lib-gc-procedure-sizer)
  `((%align 8)
    (%label procedure-sizer string-sizer)
    ;; info is always outside the GC'd heaps
    (mov rcx (mem+ rax 8 ,(fx- (tag 'procedure)))) ;get "info"
    (mov rcx (mem+ rcx 16 ,(fx- (tag 'box))))      ;free vars
    (add rcx 16)                        ;label and info field
    (shr rcx 3)
    (ret)))

(define (lib-gc-string-sizer)
  `((%align 8)
    (%label string-sizer bytevector-sizer)
    ;; XXX: in search of a shorter instruction sequence...
    (mov rcx (mem+ rax ,(fx- (tag 'string))))
    (shr rcx ,(shift 'fixnum))          ;rcx = number of u32s
    (inc rcx)
    (and rcx -2)                        ;rcx = even number of u32s
    (shr rcx 1)                         ;number of u64s
    (inc rcx)                           ;length field
    (ret)))

(define (lib-gc-bytevector-sizer)
  `((%align 8)
    (%label bytevector-sizer vector-sizer)
    ;; XXX: in search of a shorter instruction sequence...
    (mov rcx (mem+ rax ,(fx- (tag 'bytevector))))
    (shr rcx ,(shift 'fixnum))          ;rcx = number of u8s
    (add rcx 7)
    (and rcx -8)                        ;rcx aligned to u64s
    (shr rcx 3)                         ;number of u64s
    (add rcx 2)                         ;length field + seek mark
    (ret)))

(define (lib-gc-vector-sizer)
  `((%align 8)
    (%label vector-sizer invoke-trap)
    (mov rcx (mem+ rax ,(fx- (tag 'vector))))
    (add rcx 8)                         ;length field
    (shr rcx ,(shift 'fixnum))
    (ret)))

(define (lib-gc:text)
  `(,@(lib-gc-stop-and-copy)
    ,@(lib-gc-relocate)
    ,@(lib-gc-gc-loop)
    ,@(lib-gc-object-sizer)
    ,@(lib-gc-pair-sizer)
    ,@(lib-gc-procedure-sizer)
    ,@(lib-gc-string-sizer)
    ,@(lib-gc-bytevector-sizer)
    ,@(lib-gc-vector-sizer)))

(define (lib-gc:data)
  '((%align 8 0)
    ;; These are in the order of the type tags.
    (%label object-sizers)
    (%u64 box-sizer
          pair-sizer
          procedure-sizer
          string-sizer
          bytevector-sizer
          vector-sizer))))
