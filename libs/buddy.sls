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

;;; Power of two buddy allocator

;; A buddy allocator is a form of memory allocator. There are many
;; like it, but this one is straight from a book. It allows for
;; allocations that are powers of two. It has a minimum and a maximum
;; allocation size, e.g. 4K and 512K bytes. Any requested allocation
;; is rounded up to the nearest size.

;; The algorithms are described in _The Art of Computer Programming_
;; volume 1 (pp. 442-444) by Donald E. Knuth himself.

(library (loko libs buddy)
  (export
    make-buddy buddy-allocate! buddy-free!
    buddy-start-address
    (rename (buddy-free buddy-free-amount)
            (buddy-size buddy-capacity))
    buddy-dump)
  (import
    (rnrs (6))
    (loko system unsafe))

;;; Doubly-linked lists defined inside the free memory blocks

;; Forward link
(define (linkf-ref addr) (get-mem-s61 addr))
(define (linkf-set! addr v) (put-mem-s61 addr v))

;; Backward link
(define (linkb-ref addr) (get-mem-s61 (fx+ addr 8)))
(define (linkb-set! addr v) (put-mem-s61 (fx+ addr 8) v))

;;; Data structures in normal Scheme objects

;; One tag per smallest block, where 1 means available, and a k value
;; for the order of the block.
(define (tag-ref buddy i)
  (fxand (bytevector-u8-ref (buddy-tags buddy) i) 1))

(define (k-ref buddy i)
  (fxarithmetic-shift-right (bytevector-u8-ref (buddy-tags buddy) i) 1))

(define (tag/k-set! buddy i tag k)
  (assert (memv tag '(0 1)))
  (bytevector-u8-set! (buddy-tags buddy) i
                      (fxior tag (fxarithmetic-shift-left k 1))))

(define (address->index buddy addr)
  (fxarithmetic-shift-right (fx- addr (buddy-start-address buddy))
                            (buddy-lowest-order buddy)))

;; LOC(AVAIL[k]) in the book. In the original algorithm an empty list
;; was indicated by a pointer to itself. But Scheme objects, like the
;; avail vector, can't be linked to in raw memory. So #f is used
;; instead to indicate empty lists.
(define (avail-ptr buddy k)
  (vector-ref (buddy-avail buddy) k))

(define (avail-ptr-set! buddy k addr/nil)
  (vector-set! (buddy-avail buddy) k addr/nil))

;; Link to the rear of the AVAIL[k] list
(define (availf-ref buddy k)
  (linkf-ref (avail-ptr buddy k)))

(define (availf-set! buddy k addr)
  (linkf-set! (avail-ptr buddy k) addr))

;; Link to the front of the AVAIL[k] list
(define (availb-ref buddy k)
  (linkb-ref (avail-ptr buddy k)))

(define (availb-set! buddy k addr)
  (linkb-set! (avail-ptr buddy k) addr))

(define-record-type buddy
  (sealed #t)
  (fields
   ;; The start address of this buddy allocator
   start-address
   ;; Size of the lowest order allocation (i.e. the k in blocksize=2^k)
   lowest-order
   ;; The size of highest order allocation
   highest-order
   ;; Heads of doubly-linked lists
   avail
   ;; One KVAL + TAG byte per block (TAG = 1 means available)
   tags
   ;; Total capacity
   size
   ;; Amount free
   (mutable free))
  (protocol
   (lambda (new)
     (lambda (start-address size lowest-order)
       (assert (fx<=? 4 lowest-order 40))
       ;; size is rounded down to the closest block multiple
       (let* ((smallest-block-size (fxarithmetic-shift-left 1 lowest-order))
              (size (fxand size (fxnot (fx- smallest-block-size 1)))))
         (unless (fxpositive? size)
           (assertion-violation 'make-buddy
                                "Expected space for at least one block"
                                start-address size lowest-order))
         (let* ((highest-order (let ((h (fxlength (fx- size 1))))
                                 (if (fx<? size (fxarithmetic-shift-left 1 h))
                                     (- h 1)
                                     h)))
                (avail (make-vector (fx+ highest-order 1) #f))
                (tags (make-bytevector (fxarithmetic-shift-right size lowest-order) 1))
                (buddy (new start-address lowest-order highest-order avail tags size size)))
           (let lp ((order highest-order) (rem-size size) (address start-address) (avail 0))
             (cond
               ((eqv? rem-size 0)
                (assert (fx=? avail size)))
               ((fx<? rem-size (fxarithmetic-shift-left 1 order))
                (lp (fx- order 1) rem-size address avail))
               (else
                (avail-ptr-set! buddy order address)
                (availf-set! buddy order address)
                (availb-set! buddy order address)
                (tag/k-set! buddy (address->index buddy address) 1 order)
                (let ((order-size (fxarithmetic-shift-left 1 order)))
                  (lp (fx- order 1)
                      (fx- rem-size order-size)
                      (fx+ address order-size)
                      (fx+ avail order-size))))))
           buddy))))))

(define (buddy-block-size buddy)
  (fxarithmetic-shift-left 1 (buddy-lowest-order buddy)))

;;; Algorithms to reserve and liberate memory

(define (next-power-of-2 n)
  (fxarithmetic-shift-left 1 (fxlength (fx- n 1))))

;; Allocate size bytes from the buddy allocator. If the allocation
;; fails then #f is returned.
(define (buddy-allocate! buddy size)
  (assert (fixnum? size))
  (let ((n (buddy-lowest-order buddy))
        (m (buddy-highest-order buddy)))
    (and
      (fx<=? 1 size (fxarithmetic-shift-left 1 m))
      (let ((k (fxmax n (fxfirst-bit-set (next-power-of-2 size)))))
        (let lp ((j k))
          (cond
            ((fx>? j m)
             ;; Allocation failed
             #f)
            ((avail-ptr buddy j) =>
             (lambda (avail-j)
               ;; (print "Using a block of size " (fxarithmetic-shift 1 m))
               ;; (print "List head at " (number->string avail-j 16))
               (let* ((L (availb-ref buddy j))
                      (P (linkb-ref L)))
                 ;; (print (list 'L (number->string L 16) 'P (number->string P 16)))
                 ;; Unlink the block
                 (availb-set! buddy j P)
                 (linkf-set! P (avail-ptr buddy j))
                 (tag/k-set! buddy (address->index buddy L) 0 k)
                 ;; (print (list 'linkf (number->string (linkf-ref P) 16)
                 ;;              'linkb (number->string (linkb-ref P) 16)))
                 (when (fx=? L avail-j)
                   #;(print "Moving head of order " j)
                   (avail-ptr-set! buddy j P))
                 (when (and (fx=? (linkf-ref P) L) (fx=? (linkb-ref P) L))
                   ;; Unlinked the last block of this order
                   ;; (print "Unlinked last available block of order " j)
                   (avail-ptr-set! buddy j #f))
                 (let lp ((j j))
                   (unless (fx=? j k)
                     (let ((j (fx- j 1)))
                       ;; (print "Splitting the leftover block to the size "
                       ;;        (fxarithmetic-shift-left 1 j))
                       (let ((P (fx+ L (fxarithmetic-shift-left 1 j))))
                         (tag/k-set! buddy (address->index buddy P) 1 j)
                         (assert (not (avail-ptr buddy j)))
                         (avail-ptr-set! buddy j P)
                         (availf-set! buddy j P)
                         (availb-set! buddy j P)
                         (lp j)))))
                 (buddy-free-set! buddy (fx- (buddy-free buddy)
                                             (fxarithmetic-shift 1 k)))
                 ;; (print "** Allocated " (number->string (- L (buddy-start-address buddy)) 16))
                 L)))
            (else
             ;; There are no blocks empty of this size. Try a
             ;; larger size.
             (lp (fx+ j 1)))))))))

;; Release the given address.
(define (buddy-free! buddy addr)
  (assert (fixnum? addr))
  (let ((start (buddy-start-address buddy))
        (size  (buddy-size buddy))
        (block-size (buddy-block-size buddy)))
    ;; (print "*** Freeing " (number->string (- addr start) 16))
    (let ((n (buddy-lowest-order buddy))
          (m (buddy-highest-order buddy))
          (M (fx+ start size)))
      ;; FIXME: validate the lower bits
      (unless (fx<=? start addr M)
        (assertion-violation 'buddy-free! "The address does not belong to this buddy allocator"
                             buddy addr))
      (unless (eqv? 0 (tag-ref buddy (address->index buddy addr)))
        (assertion-violation 'buddy-free! "The address is already free" buddy addr))
      (let ((k (k-ref buddy (address->index buddy addr))))
        (buddy-free-set! buddy (fx+ (buddy-free buddy) (fxarithmetic-shift 1 k)))
        (let lp ((L addr)
                 (k k))
          (unless (fx<=? n k m)
            (error 'buddy-free! "The k value is invalid" buddy addr k))
          (let* ((P (fx+ start (fxxor (fxarithmetic-shift-left 1 k) (fx- L start))))
                 (P-idx (address->index buddy P)))
            ;;(print "Buddy to " (list (address->index buddy L) k) " is at " (list P-idx k))
            (cond ((or (fx=? k m)
                       (let ((P-tag (if (fx>=? P (fx- M (fxarithmetic-shift-left 1 k)))
                                        0
                                        (tag-ref buddy P-idx))))
                         ;;(print "TAG(P)=" P-tag)
                         (or (eqv? 0 P-tag)
                             (not (fx=? (k-ref buddy P-idx) k)))))
                   ;; The buddy is busy, let's put this on the avail list
                   (tag/k-set! buddy (address->index buddy L) 1 k)
                   (cond ((avail-ptr buddy k) =>
                          (lambda (P)
                            ;; Link L onto AVAIL[k]
                            (linkf-set! L (linkf-ref P))
                            (linkb-set! L P)
                            (linkb-set! (linkf-ref P) L)
                            (linkf-set! P L)
                            (avail-ptr-set! buddy k L)))
                         (else
                          ;; Create AVAIL[k] and add L to it
                          (linkf-set! L L)
                          (linkb-set! L L)
                          (avail-ptr-set! buddy k L))))
                  (else
                   ;; Combine the block with its buddy. Unlink P from AVAIL[k].
                   ;; (print "Combining buddies of order " k " by removing "
                   ;;        (number->string (- P start) 16))
                   #;
                   (let lp ((P^ (avail-ptr buddy k)) (seen (list L)))
                     (unless (fx=? L P^)
                       (if (memv P^ seen)
                           (unless (memv P seen)
                             (error 'buddy-free! "Tried to free address not on the free list"
                                    P k))
                           (lp (linkf-ref P^) (cons P^ seen)))))
                   (cond ((fx=? (linkf-ref P) P)
                          (avail-ptr-set! buddy k #f))
                         (else
                          (linkb-set! (linkf-ref P) (linkb-ref P))
                          (linkf-set! (linkb-ref P) (linkf-ref P))
                          (when (fx=? P (avail-ptr buddy k))
                            (avail-ptr-set! buddy k (linkb-ref P)))))
                   (lp (fxmin L P) (fx+ k 1))))))))))

;;; Bonus material

(define (print . x) (for-each display x) (newline))

(define (dump-list buddy L)
  (define (pinfo addr)
    (let ((i (address->index buddy addr)))
      (display (list (tag-ref buddy i) (k-ref buddy i)))))
  (define (fmtaddr addr)
    (number->string (fx- addr (buddy-start-address buddy)) 16))
  (display "  -> ")
  (display (fmtaddr L))
  (pinfo L)
  (let lp ((P (linkf-ref L)) (seen (list L)) (lenf 1))
    (cond ((not (fx=? L P))
           (display " -> ")
           (display (fmtaddr P))
           (pinfo P)
           (if (memv P seen)
               (display " *POW!*")
               (lp (linkf-ref P) (cons P seen) (fx+ lenf 1))))
          (else
           (display " => ")
           (display (fmtaddr P))
           (pinfo P)
           (newline)
           (display "  <- ")
           (display (fmtaddr L))
           (pinfo L)
           (let lp ((P (linkb-ref L)) (seen (list L)) (lenb 1))
             (cond ((not (fx=? L P))
                    (display " <- ")
                    (display (fmtaddr P))
                    (pinfo P)
                    (if (memv P seen)
                        (display " *BLAM!*")
                        (lp (linkb-ref P) (cons P seen) (fx+ lenb 1))))
                   (else
                    (display " <= ")
                    (display (fmtaddr P))
                    (pinfo P)
                    (unless (fx=? lenf lenb)
                      (display "*POF!*"))))))))
  (newline))

;; Dump a buddy tree to standard output.
(define (buddy-dump buddy)
  (let ((n (buddy-lowest-order buddy))
        (m (buddy-highest-order buddy)))
    (print "Buddy allocator of orders " n "-" m
           " with " (buddy-free buddy) " free space")
    (do ((k m (fx- k 1)))
        ((fx<? k n))
      (print k " " (fxarithmetic-shift-left 1 k))
      (cond
        ((avail-ptr buddy k) =>
         (lambda (L)
           (dump-list buddy L)))
        (else
         (print "  -> ()")))))))
