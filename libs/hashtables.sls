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

;;; Hashtables

;; Initially the hashtable is an alist, then when it's larger it
;; becomes a proper hashtable.

;; Eq hashtables that use buckets have a garbage collection count. If
;; the current garbage collection counter has changed since last time
;; the hashtable was updated it is necessary to rehash the table.

;; Also: make sure to not be resistant to those complexity attacks
;; http://www.kb.cert.org/vuls/id/903934
;; http://www.team5150.com/~andrew/blog/2007/03/breaking_superfasthash.html ?
;; http://burtleburtle.net/bob/hash/index.html#lookup ?
;; http://stevehanov.ca/blog/index.php?id=119

;; Maybe every hashtable should have its own random hash function. The
;; hashes returned by user-provided hash functions should be fixed
;; automatically to be resistant to those attacks, if that's possible.

(library (loko libs hashtables)
  (export
    make-eq-hashtable make-eqv-hashtable
    (rename (make-general-hashtable make-hashtable))
    hashtable? hashtable-size hashtable-ref hashtable-set!
    hashtable-delete! hashtable-contains? hashtable-update!
    hashtable-copy hashtable-clear! hashtable-keys
    hashtable-entries
    ;; Inspection
    (rename (hashtable-cmp hashtable-equivalence-function))
    hashtable-hash-function hashtable-mutable?
    ;; Hash functions
    string-hash #;string-ci-hash)
  (import
    (except (rnrs)
            make-eq-hashtable make-eqv-hashtable make-hashtable
            hashtable? hashtable-size hashtable-ref hashtable-set!
            hashtable-delete! hashtable-contains? hashtable-update!
            hashtable-copy hashtable-clear! hashtable-keys
            hashtable-entries
            hashtable-equivalence-function
            hashtable-hash-function hashtable-mutable?
            string-hash string-ci-hash)
    (rnrs mutable-pairs)
    (only (loko system $host)
          $object->fixnum
          garbage-collection-count
          $void?)
    (only (loko system $symbols) $immsym?))

(define-syntax print
  (syntax-rules ()
    #;
    ((_ . args) (begin (when (procedure? display) (for-each display (list . args)) (newline))))
    ((_ . args) (begin 'dummy))))

(define ALIST-THRESHOLD 32)           ;TODO: take measurements

(define BUCKET-THRESHOLD 8)          ;TODO: take measurements

(define FIRST-VECTOR-SIZE 47)         ;TODO: take measurements

;; TODO: pick a different positive fixnum every time. and
;; (define RANDOM 1012919948349234237)

(define (double-size x)
  ;; TODO: add or subtract some small random constant? use a series
  ;; of primes?
  (fx+ (fx* x 2) -1))

(define (larger-size x)
  ;; TODO: find the next prime? used when a bucket has grown too
  ;; large. This should in some way change the hash function...
  (fx+ x 13))

(define-record-type hashtable
  (opaque #t)
  (sealed #t)
  ;; (nongenerative)
  (fields (mutable vals)
          (mutable size)
          (mutable gc-count)            ;#f or fixnum
          (immutable cmp)
          (immutable hash)
          (mutable mutable?)            ;#t or #f only
          (immutable gc-sensitive?)))

(define (hashtable-hash-function ht)
  (and (not (hashtable-gc-sensitive? ht))
       (hashtable-hash ht)))

(define make-eq-hashtable
  (case-lambda
    (() (make-hashtable 0 0 #f eq? %eq-hash #t #t))
    ((k) (make-eq-hashtable))))

(define make-eqv-hashtable
  (case-lambda
    (() (make-hashtable 0 0 #f eqv? %eqv-hash #t #t))
    ((k) (make-eqv-hashtable))))

(define make-general-hashtable
  (case-lambda
    ((hash-function equiv)
     (make-hashtable 0 0 #f equiv hash-function #t #f))
    ((hash-function equiv k)
     (make-general-hashtable hash-function equiv))))

(define (%eq-hash x)
  ;; FNV-1 hash. Assumes a 64-bit machine.
  (define FNV_prime 16777619)
  (define offset_basis 2166136261)
  (do ((b (fxand ($object->fixnum x) (greatest-fixnum))
          (fxarithmetic-shift-right b 8))
       (d offset_basis
          (fxand (fxxor (fx* d FNV_prime)
                        (fxand b #xff))
                 #xffffffff)))
      ((eqv? b 0) d)))

(define (%eqv-hash x)
  (cond ((fixnum? x) (%eq-hash x))
        ((number? x) (equal-hash x))
        (else (%eq-hash x))))

;; Returns the cons cell for the value that matches the key. Uses 0
;; instead of '(), because in Loko vectors are initialized to
;; contain 0.
(define (alist-lookup key vals cmp mutable?)
  ;;(print ";;; ALIST: " vals)
  (let lp ((prev #f) (it vals) (len 0))
    (cond ((eqv? it 0) len)
          ((cmp (caar it) key)
           (let* ((cell (car it))
                  (value (cdr cell)))
             (if (or (not prev) (not mutable?))
                 cell
                 (let ((p (car prev))
                       (c cell))
                   (let ((pa (car p)) (pd (cdr p))
                         (ca (car c)) (cd (cdr c)))
                     ;; The value bubbles to the front
                     (set-car! p ca)
                     (set-cdr! p cd)
                     (set-car! c pa)
                     (set-cdr! c pd)
                     p)))))
          (else
           (lp it (cdr it) (fx+ len 1))))))

(define (immobile? x)
  ;; TODO: detect objects that do not live in the current heap
  (or (fixnum? x)
      (char? x)
      (boolean? x)
      (eof-object? x)
      (null? x)
      ($immsym? x)
      ($void? x)))

;; This is invoked with the old vector and a new vector. It computes
;; new hashes for all the keys and inserts them in the new vector.
(define (rehash! ht iteration ov nv)
  (print ";;; rehashing")
  (let ((start-gc (garbage-collection-count))
        (hash (hashtable-hash ht)))
    (hashtable-vals-set! ht nv)
    (let loop-vector ((i 0) (all-immobile #t))
      (if (fx=? i (vector-length ov))
          (when (hashtable-gc-sensitive? ht)
            (cond ((not all-immobile)
                   (let ((current-gc (garbage-collection-count)))
                     (cond ((fx=? start-gc current-gc)
                            (hashtable-gc-count-set! ht current-gc))
                           ((fx>? iteration 42)
                            (error 'rehash!
                                   "Internal error: rehash triggers GC every time" ht))
                           (else
                            (print ";;; rehashing did not survive GC!!")
                            (vector-fill! nv 0)
                            (rehash! ht (fx+ iteration 1) ov nv)))))
                  (else
                   (hashtable-gc-count-set! ht #f))))
          (let loop-bucket ((obucket (vector-ref ov i))
                            (all-immobile all-immobile))
            (if (eqv? obucket 0)
                (loop-vector (fx+ i 1) all-immobile)
                (let* ((key (caar obucket))
                       (value (cdar obucket))
                       (idx (fxmod (hash key) (vector-length nv))))
                  (vector-set! nv idx (cons (cons key value)
                                            (vector-ref nv idx)))
                  (loop-bucket (cdr obucket)
                               (and all-immobile (immobile? key))))))))))

(define cleared '(cleared))

(define (hashtable-ref ht key default)
  (let lp ()
    (print "hashtable-ref " ht " " key " " default)
    (let ((vals (hashtable-vals ht))
          (cmp (hashtable-cmp ht)))
      (cond ((vector? vals)
             (let* ((gc (hashtable-gc-count ht))
                    (hash (hashtable-hash ht))
                    (len (vector-length vals))
                    (idx (fxmod (hash key) len))
                    (bucket (vector-ref vals idx)))
               ;;(print "#;IDX " key " => " idx)
               (cond ((and gc (not (fx=? gc (garbage-collection-count))))
                      ;; The hashes have been changed by the GC.
                      (print ";;; must rehash on ref due to GC")
                      (rehash! ht 0 vals (make-vector len 0))
                      (lp))
                     (else
                      (let ((cell/len (alist-lookup key bucket cmp
                                                    (hashtable-mutable? ht))))
                        (cond ((pair? cell/len) (cdr cell/len))
                              (else default)))))))
            (else
             ;; association list
             (let ((cell/len (alist-lookup key vals cmp (hashtable-mutable? ht))))
               (cond ((pair? cell/len)
                      (if (eq? (cdr cell/len) cleared)
                          default
                          (cdr cell/len)))
                     (else default))))))))

(define (hashtable-set! ht key value)
  (hashtable-update! ht key (lambda _ value) value))

(define (hashtable-delete! ht key)
  ;; XXX: This is dumb
  (hashtable-set! ht key cleared))

(define (hashtable-contains? ht key)
  (and (hashtable-ref ht key #f) #t))

(define (hashtable-update! ht key proc default)
  (define (alist-update! alist)
    ;; Tries to find and update the key if possible. If inserts a
    ;; new pair in the bucket list it returns (approx.) the length
    ;; of the bucket list.
    (let ((cell/len (alist-lookup key alist (hashtable-cmp ht)
                                  (hashtable-mutable? ht))))
      (cond ((pair? cell/len)
             (print ";; already in bucket")
             (set-cdr! cell/len (proc (cdr cell/len)))
             (values 0 alist))
            (else
             (print ";; new element in bucket")
             (hashtable-size-set! ht (fx+ (hashtable-size ht) 1))
             (values cell/len (cons (cons key (proc default))
                                    alist))))))
  (assert (hashtable-mutable? ht))
  (print "hashtable-update! " key " " proc " " default)
  (let lp ()
    (let ((vals (hashtable-vals ht))
          (cmp (hashtable-cmp ht)))
      (cond ((vector? vals)
             (let* ((start-gc (garbage-collection-count))
                    (gc (hashtable-gc-count ht))
                    (hash (hashtable-hash ht))
                    (len (vector-length vals))
                    (idx (fxmod (hash key) len))
                    (bucket (vector-ref vals idx)))
               ;;(print "#;IDX " key " => " idx)
               (cond ((and gc (not (eq? gc (garbage-collection-count))))
                      ;; The hashes have been changed by the GC.
                      (print ";;; Must rehash on set! due to GC")
                      (rehash! ht 0 vals (make-vector len 0))
                      (lp))
                     (else
                      (let-values ([(blen bucket) (alist-update! bucket)])
                        (vector-set! vals idx bucket)
                        (when (and (not gc) (hashtable-gc-sensitive? ht)
                                   (not (immobile? key)))
                          (hashtable-gc-count-set! ht start-gc))
                        (when (and (fx>? blen BUCKET-THRESHOLD) (< len 100000))
                          (rehash! ht 0 vals (make-vector (double-size len) 0)))
                        (print "hashtable-update done."))))))
            (else
             (let-values (((len vals*) (alist-update! vals)))
               (print ";; alist-update! => " len " " vals*)
               (hashtable-vals-set! ht vals*)
               (when (fx>? len ALIST-THRESHOLD)
                 ;; Time to move away from alists.
                 (print ";; moving to vectors")
                 (hashtable-clear! ht)
                 (hashtable-vals-set! ht (make-vector FIRST-VECTOR-SIZE 0))
                 ;; TODO: this can use the knowledge that the key
                 ;; is not already in the table
                 (do ((vals vals* (cdr vals)))
                     ((eqv? vals 0))
                   (let ((x (car vals)))
                     (hashtable-set! ht (car x) (cdr x))))
                 (print ";; done moving to vectors"))))))))

#;
(define (print-ht x)
  (let ((vals (hashtable-vals x)))
    (print ";; GC count at last rehash: " (hashtable-gc-count x))
    (print ";; Current GC count: " (garbage-collection-count))
    (cond ((vector? vals)
           (do ((i 0 (fx+ i 1)))
               ((fx=? i (vector-length vals)))
             (print "#;BUCKET " i " : " (vector-ref vals i))))
          (else
           (print "#;ALIST: " vals)))))

;; TODO: compute a perfect hash?
(define hashtable-copy
  (case-lambda
    ((ht)
     (unless (hashtable? ht)
       (assertion-violation 'hashtable-copy
                            "Expected a hashtable"
                            ht))
     (hashtable-copy ht #f))
    ((ht mutable?)
     (unless (hashtable? ht)
       (assertion-violation 'hashtable-copy
                            "Expected a hashtable"
                            ht mutable?))
     (let ((new-ht (make-hashtable 0 0 #f
                                   (hashtable-cmp ht)
                                   (hashtable-hash ht)
                                   #t   ;temporarily mutable
                                   (hashtable-gc-sensitive? ht)))
           (vals (hashtable-vals ht)))
       (cond ((vector? vals)
              (let ((b* vals))
                (let loop-vector ((j 0) (i 0))
                  (unless (fx=? i (vector-length b*))
                    (let loop-bucket ((j j) (b (vector-ref b* i)))
                      (if (pair? b)
                          (let* ((k (caar b))
                                 (v (cdar b)))
                            (hashtable-set! new-ht k v)
                            (loop-bucket (fx+ j 1) (cdr b)))
                          (loop-vector j (fx+ i 1))))))))
             (else
              (do ((i 0 (fx+ i 1))
                   (vals vals (cdr vals)))
                  ((not (pair? vals)))
                (hashtable-set! new-ht (caar vals) (cdar vals)))))
       ;; Temporarily commented out due to some weirdness
       ;; with (machine-code assembler x86).
       #;(hashtable-mutable?-set! ht (if mutable? #t #f))
       new-ht))))

(define hashtable-clear!
  (case-lambda
    ((ht)
     (assert (hashtable-mutable? ht))
     (hashtable-vals-set! ht 0)
     (hashtable-size-set! ht 0)
     (hashtable-gc-count-set! ht #f))
    ((ht k)
     (hashtable-clear! ht))))

(define (hashtable-keys ht)
  (let ((size (hashtable-size ht))
        (vals (hashtable-vals ht)))
    (let ((k* (make-vector size)))
      (cond ((vector? vals)
             (let ((b* vals))
               (let loop-vector ((j 0) (i 0))
                 (unless (fx=? i (vector-length b*))
                   (let loop-bucket ((j j) (b (vector-ref b* i)))
                     (if (pair? b)
                         (let ((k (caar b)))
                           (vector-set! k* j k)
                           (loop-bucket (fx+ j 1) (cdr b)))
                         (loop-vector j (fx+ i 1))))))))
            (else
             (do ((i 0 (fx+ i 1))
                  (vals vals (cdr vals)))
                 ((not (pair? vals)))
               (vector-set! k* i (caar vals)))))
      k*)))

(define (hashtable-entries ht)
  (let ((size (hashtable-size ht))
        (vals (hashtable-vals ht)))
    (let ((k* (make-vector size))
          (v* (make-vector size)))
      (cond ((vector? vals)
             (let ((b* vals))
               (let loop-vector ((j 0) (i 0))
                 (unless (fx=? i (vector-length b*))
                   (let loop-bucket ((j j) (b (vector-ref b* i)))
                     (if (pair? b)
                         (let* ((k (caar b))
                                (v (cdar b)))
                           (vector-set! k* j k)
                           (vector-set! v* j v)
                           (loop-bucket (fx+ j 1) (cdr b)))
                         (loop-vector j (fx+ i 1))))))))
            (else
             (do ((i 0 (fx+ i 1))
                  (vals vals (cdr vals)))
                 ((not (pair? vals)))
               (vector-set! k* i (caar vals))
               (vector-set! v* i (cdar vals)))))
      (values k* v*))))

;;; Hash functions

;; equal-hash would be in (loko libs equal).
;; symbol-hash would be in (loko libs symbols).

(define (string-hash s)
  ;; FNV-1 hash. Assumes a 64-bit machine.
  (define FNV_prime 16777619)
  (define offset_basis 2166136261)
  (do ((i (fx- (string-length s) 1) (fx- i 1))
       (d offset_basis
          (do ((b (char->integer (string-ref s i))
                  (fxarithmetic-shift-right b 8))
               (d d
                  (fxand (fxxor (fx* d FNV_prime)
                                (fxand b #xff))
                         #xffffffff)))
              ((eqv? b 0) d))))
      ((eqv? i -1) d)))

;; TODO: string-ci-hash

)
