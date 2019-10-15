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

;;; Records

;; TODO: apparently the syntactical records layer can be used to find
;; indices etc at expand-time.

#|
record-type-descriptors:
 -1 'rtd
  0 length (6)
  1 flags-and-length
    bits 0-31: length (used by GC)
       bit 32: is opaque
       bit 33: is sealed
  2 name (symbol)
  3 parent (rtd or false)
  4 uid (symbol or false)
  5 names (vector of symbols)
  6 mutable (bitfield)
  7 record-writer (procedure)

record-constructor-descriptors:
 -1 'rcd
  0 length (4)
  1 rtd
  2 parent-rcd
  3 protocol
  4 non-default protocol (boolean)
|#

;; XXX: symbols.scm has not yet been loaded when this top-level runs

(library (loko runtime records)
  (export
    make-record-type-descriptor record-type-descriptor?
    make-record-constructor-descriptor
    record-constructor record-predicate
    record-accessor record-mutator
    record? record-rtd record-type-name record-type-parent
    record-type-uid record-type-generative? record-type-sealed?
    record-type-opaque? record-type-field-names record-field-mutable?
    record-writer)
  (import
    (except (rnrs)
            make-record-type-descriptor record-type-descriptor?
            make-record-constructor-descriptor
            record-constructor record-predicate
            record-accessor record-mutator
            record? record-rtd record-type-name record-type-parent
            record-type-uid record-type-generative? record-type-sealed?
            record-type-opaque? record-type-field-names record-field-mutable?)
    (loko system $primitives))

;;; Procedural layer

(define *uids* '())

(define (lookup-uid uid)
  ;; TODO: should switch to hashtables (and weak references?) when
  ;; that library becomes available.
  (cond ((assq uid *uids*) => cdr)
        (else #f)))

(define (remember-uid uid rtd fields)
  (set! *uids* (cons (cons uid (cons rtd fields)) *uids*)))

(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
  (define who 'make-record-type-descriptor)
  (define (make-rtd flags&len name parent uid names mutable)
    (let ((ret ($make-box 'rtd 8)))
      ($box-set! ret 0 7)
      ($box-set! ret 1 flags&len)
      ($box-set! ret 2 name)
      ($box-set! ret 3 parent)
      ($box-set! ret 4 uid)
      ($box-set! ret 5 names)
      ($box-set! ret 6 mutable)
      ($box-set! ret 7 default-record-writer)
      ret))
  (define (build parent-length parent-mutable opaque?)
    (let ((len (fx+ (vector-length fields) parent-length)))
      (assert (fx<? len #x100000))    ;XXX: somewhat arbitrary
      (do ((fi 0 (fx+ fi 1))
           (idx parent-length (fx+ idx 1))
           (names (make-vector (vector-length fields)))
           (mut parent-mutable
                (case (car (vector-ref fields fi))
                  ;; Uses a bitmask to indicate mutability.
                  ((immutable) mut)
                  ((mutable) (bitwise-ior mut (expt 2 idx)))
                  (else
                   (assertion-violation who "Incorrect field specification"
                                        fields)))))
          ((fx=? idx len)
           (make-rtd (fxior (if sealed? (fxarithmetic-shift-left #b10 32) 0)
                            (if opaque? (fxarithmetic-shift-left #b01 32) 0)
                            len)
                     name parent uid names mut))
        (let ((f (vector-ref fields fi)))
          (unless (and (pair? f)
                       (pair? (cdr f))
                       (null? (cddr f))
                       (or (not (procedure? symbol?))
                           (symbol? (cadr f))))
            (assertion-violation who "Incorrect field specification"
                                 name fields f))
          (vector-set! names fi (cadr f))))))
  ;; Check that all the little things are what they're supposed to
  ;; be. The contents of fields is checked later.
  (when (and (procedure? symbol?) (not (symbol? name)))
    (assertion-violation 'make-record-type-descriptor
                         "Expected a symbol as name"
                         name parent uid sealed? opaque? fields))
  (when parent
    (unless (record-type-descriptor? parent)
      (assertion-violation 'make-record-type-descriptor
                           "Expected an rtd or #f as parent"
                           name parent uid sealed? opaque? fields))
    (when (record-type-sealed? parent)
      (assertion-violation 'make-record-type-descriptor
                           "The parent must not be sealed"
                           name parent uid sealed? opaque? fields))
    ;; TODO: check this
    #;
    (when (and (not (record-type-uid parent)) uid)
      (assertion-violation "Must be generative because the parent is generative"
                           name parent uid)))
  (unless (or (not uid) (or (not (procedure? symbol?)) (symbol? uid)))
    (assertion-violation 'make-record-type-descriptor
                         "Expected the uid argument to be #f or a symbol"
                         name parent uid sealed? opaque? fields))
  (unless (boolean? sealed?)
    (assertion-violation 'make-record-type-descriptor
                         "Expected the sealed? argument to be a boolean"
                         name parent uid sealed? opaque? fields))
  (unless (boolean? opaque?)
    (assertion-violation 'make-record-type-descriptor
                         "Expected the opaque? argument to be a boolean"
                         name parent uid sealed? opaque? fields))
  (unless (vector? fields)
    (assertion-violation 'make-record-type-descriptor
                         "Expected the fields argument to be a vector"
                         name parent uid sealed? opaque? fields))

  ;; See if the rtd already exists or create a new one.
  (cond ((and uid (lookup-uid uid)) =>
         (lambda (rtd/fields)
           ;; The requested rtd has already been created. Verify
           ;; that everything matches.
           (let ((rtd (car rtd/fields))
                 (old-fields (cdr rtd/fields)))
             (assert (eqv? parent (record-type-parent rtd)))
             (assert (eq? sealed? (record-type-sealed? rtd)))
             (assert (eq? opaque? (record-type-opaque? rtd)))
             (assert (equal? fields old-fields))
             rtd)))
        (else
         (let ((ret (if parent
                        (build ($record-length parent)
                               ($box-ref parent 6)
                               (or opaque? (record-type-opaque? parent)))
                        (build 0 0 opaque?))))
           (when uid
             ;; Nongenerative record type.
             (remember-uid uid ret fields))
           ret))))

(define (record-type-descriptor? obj)
  (and ($box? obj) (eq? ($box-type obj) 'rtd)))

(define (make-record-constructor-descriptor rtd parent-rcd protocol)
  (define who 'make-record-constructor-descriptor)
  (define (make-rcd rtd parent-rcd protocol nondefault-protocol)
    (let ((ret ($make-box 'rcd 5)))
      ($box-set! ret 0 4)
      ($box-set! ret 1 rtd)
      ($box-set! ret 2 parent-rcd)
      ($box-set! ret 3 protocol)
      ($box-set! ret 4 nondefault-protocol)
      ret))
  (define (rcd-rtd rcd) ($box-ref rcd 1))
  (define (rcd-nondefault-protocol? rcd) ($box-ref rcd 4))
  (define (split-at args i)
    ;; srfi-1
    (let lp ((ret '()) (l args) (i i))
      (cond ((fxzero? i)
             (values (reverse ret) l))
            ((pair? l)
             (lp (cons (car l) ret) (cdr l) (fx- i 1)))
            (else
             (assertion-violation 'record-constructor
                                  "Too few arguments to record constructor"
                                  rtd args)))))
  (define (default-protocol rtd parent-rtd)
    (if parent-rtd
        (let ((parent-size ($record-length parent-rtd)))
          (lambda (p)
            ;; TODO: less consing
            (lambda all-values
              (let-values (((parent-values this-values)
                            (split-at all-values parent-size)))
                (apply (apply p parent-values) this-values)))))
        (lambda (p) p)))

  (assert (record-type-descriptor? rtd))
  (when parent-rcd
    (assert (rcd? parent-rcd))
    ;; TODO: verify this stuff
    (assert (eq? (rcd-rtd parent-rcd) (record-type-parent rtd))))

  (when protocol
    (assert (procedure? protocol)))

  (let ((parent-rtd (record-type-parent rtd)))
    (when (and parent-rcd (not parent-rtd))
      ;; TODO
      (assertion-violation who "Mismatch mishmash" rtd parent-rcd protocol
                           parent-rtd))
    (when (and (not parent-rtd) parent-rcd)
      ;; "If rtd is a base record type then parent-constructor-descriptor must be #f."
      (assertion-violation who "The parent rcd must be #f for base record types"
                           rtd parent-rcd protocol))
    (when (and parent-rcd (not protocol) (rcd-nondefault-protocol? parent-rcd))
      (error who "The parent has a protocol so this rcd must also have a protocol"
             rtd parent-rcd protocol))

    (make-rcd rtd
              (or parent-rcd
                  (and parent-rtd (make-record-constructor-descriptor parent-rtd #f #f)))
              (or protocol (default-protocol rtd parent-rtd))
              (and protocol #t))))


(define ($record-length x)
  (fxand #xffffffff ($box-ref x 1)))

(define (rcd? x)
  (and ($box? x) (eq? ($box-type x) 'rcd)))

(define (record-constructor rcd)
  (define who 'record-constructor)
  (define (rcd-rtd x) ($box-ref x 1))
  (define (rcd-parent-rcd x) ($box-ref x 2))
  (define (rcd-protocol x) ($box-ref x 3))

  (define (base-record-constructor rtd)
    (define who 'make-record-of-some-sort)
    (case ($record-length rtd)
      ((0)
       (lambda () ($make-box rtd 0)))
      ((1)
       (lambda (a)
         (let ((ret ($make-box rtd 1)))
           ($box-set! ret 0 a)
           ret)))
      ((2)
       (lambda (a b)
         (let ((ret ($make-box rtd 2)))
           ($box-set! ret 0 a)
           ($box-set! ret 1 b)
           ret)))
      ;; XXX: more of these?
      (else
       (lambda (a b c . x)
         (let* ((len ($record-length rtd))
                (ret ($make-box rtd len)))
           ($box-set! ret 0 a)
           ($box-set! ret 1 b)
           ($box-set! ret 2 c)
           (do ((i 3 (fx+ i 1))
                (f x (cdr f)))
               ((fx=? i len)
                (unless (null? f)
                  (error who "Too many arguments to record constructor"
                         rcd a b c x))
                ret)
             (when (null? f)
               (error who "Too few arguments to record constructor"
                      rcd a b c x))
             ($box-set! ret i (car f))))))))

  (define (make-make-seeder real-rtd for-desc)
    ;; From the reference implementation of SRFI-76. The license
    ;; below only applies to this complicated procedure. It has
    ;; been modified.
    ;;
    ;; Copyright (C) Michael Sperber (2005). All Rights Reserved.
    ;;
    ;; Permission is hereby granted, free of charge, to any person
    ;; obtaining a copy of this software and associated documentation files
    ;; (the "Software"), to deal in the Software without restriction,
    ;; including without limitation the rights to use, copy, modify, merge,
    ;; publish, distribute, sublicense, and/or sell copies of the Software,
    ;; and to permit persons to whom the Software is furnished to do so,
    ;; subject to the following conditions:
    ;;
    ;; The above copyright notice and this permission notice shall be
    ;; included in all copies or substantial portions of the Software.
    ;;
    ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    ;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    ;;, MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    ;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
    ;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
    ;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    ;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    ;; SOFTWARE.
    (let recur ((for-desc for-desc))
      (let ((for-rtd (rcd-rtd for-desc)))
        (cond ((rcd-parent-rcd for-desc) =>
               (lambda (parent-desc)
                 (let ((parent-protocol (rcd-protocol parent-desc))
                       (parent-make-seeder (recur parent-desc))
                       ;; TODO: can just check the length of the names
                       (for-rtd-count (- ($record-length for-rtd)
                                         ($record-length (rcd-rtd parent-desc)))))
                   ;; TODO: less consing
                   (lambda extension-values
                     (lambda parent-protocol-args
                       (lambda for-rtd-values
                         (unless (fx=? (length for-rtd-values) for-rtd-count)
                           (assertion-violation who
                                                "Wrong number of arguments to record constructor"
                                                for-rtd for-rtd-values))
                         (apply (parent-protocol
                                 (apply parent-make-seeder
                                        (append for-rtd-values extension-values)))
                                parent-protocol-args)))))))
              (else
               (let ((for-rtd-count ($record-length for-rtd)))
                 (lambda extension-values
                   ;; Take a shortcut if there are no extension values.
                   (if (null? extension-values)
                       (base-record-constructor real-rtd)
                       (lambda for-rtd-values
                         (unless (fx=? (length for-rtd-values) for-rtd-count)
                           (assertion-violation who
                                                "Wrong number of arguments to record constructor"
                                                for-rtd for-rtd-values))
                         (apply (base-record-constructor real-rtd)
                                (append for-rtd-values extension-values)))))))))))

  (assert (rcd? rcd))
  (let ((rtd (rcd-rtd rcd))
        (protocol (rcd-protocol rcd)))
    (protocol ((make-make-seeder rtd rcd)))))

(define (record-predicate rtd)
  (if (record-type-sealed? rtd)       ;TODO: verify that this is ok
      (lambda (obj)
        (and ($box? obj)
             (eq? ($box-type obj) rtd)))
      (lambda (obj)
        (and ($box? obj)
             (let ((t ($box-type obj)))
               (or (eq? t rtd)
                   (and (record-type-descriptor? t)
                        (let lp ((t (record-type-parent t)))
                          (cond ((eq? t rtd) #t)
                                ((not t) #f)
                                (else (lp (record-type-parent t))))))))))))

(define (record-field-name rtd k)
  (let lp ((t rtd) (len ($record-length rtd)))
    (let* ((names (record-type-field-names t))
           (idx0 (fx- len (vector-length names))))
      (if (fx>=? k idx0)
          (vector-ref names (fx- k idx0))
          (lp (record-type-parent t) idx0)))))

(define (raise-accessor-error rtd k r)
  ;; XXX: The syntactic layer would know the true name of
  ;; the mutator and would be preferable.
  (let ((fieldname (symbol->string (record-field-name rtd k)))
        (typename (symbol->string (record-type-name rtd))))
    (assertion-violation (string->symbol (string-append typename "-" fieldname))
                         (string-append "Expected a record of type " typename)
                         r)))

(define (record-accessor rtd k)
  (assert (record-type-descriptor? rtd))
  (assert (and (fixnum? k) (fx>=? k 0)))
  (let* ((prtd (record-type-parent rtd))
         (k (if prtd (fx+ k ($record-length prtd)) k)))
    (assert (fx<? k ($record-length rtd)))
    (if (record-type-sealed? rtd)
        (lambda (r)
          (if (and ($box? r) (eq? ($box-type r) rtd))
              ($box-ref r k)
              (raise-accessor-error rtd k r)))
        (let ((right-type? (record-predicate rtd)))
          (lambda (r)
            (if (right-type? r)
                ($box-ref r k)
                (raise-accessor-error rtd k r)))))))

(define (raise-mutator-error rtd k r v)
  (let ((fieldname (symbol->string (record-field-name rtd k)))
        (typename (symbol->string (record-type-name rtd))))
    (assertion-violation (string->symbol (string-append typename "-" fieldname "-set!"))
                         (string-append "Expected a record of type " typename)
                         r v)))

(define (record-mutator rtd k)
  (assert (record-type-descriptor? rtd))
  (assert (and (fixnum? k) (fx>=? k 0)))
  (let* ((prtd (record-type-parent rtd))
         (k (if prtd (fx+ k ($record-length prtd)) k)))
    (assert (fx<? k ($record-length rtd)))
    (if (bitwise-bit-set? ($box-ref rtd 6) k)
        (if (record-type-sealed? rtd)
            (lambda (r v)
              (if (and ($box? r) (eq? ($box-type r) rtd))
                  ($box-set! r k v)
                  (raise-mutator-error rtd k r v)))
            (let ((right-type? (record-predicate rtd)))
              (lambda (r v)
                (if (right-type? r)
                    ($box-set! r k v)
                    (raise-mutator-error rtd k r v)))))
        (assertion-violation 'record-mutator "Expected a mutable field" rtd k))))

;;; Inspection

(define (record? x)
  (and ($box? x)
       (record-type-descriptor? ($box-type x))
       (not (record-type-opaque? ($box-type x)))))

(define (record-rtd* x)
  (if (not ($box? x))
      (error 'record-rtd "Expected a record" x)
      (let ((t ($box-type x)))
        (if (not (record-type-descriptor? t))
            (error 'record-rtd "Expected a record" x)
            t))))

(define (record-rtd x)
  (let ((t (record-rtd* x)))
    (if (record-type-opaque? t)
        (error 'record-rtd "Expected a non-opaque record" x)
        t)))

(define (record-type-name rtd)
  (assert (record-type-descriptor? rtd))
  ($box-ref rtd 2))

(define (record-type-parent rtd)
  (assert (record-type-descriptor? rtd))
  ($box-ref rtd 3))

(define (record-type-uid rtd)
  (assert (record-type-descriptor? rtd))
  ($box-ref rtd 4))

(define (record-type-generative? rtd)
  (assert (record-type-descriptor? rtd))
  ;; only nongenerative records have a uid
  (not ($box-ref rtd 4)))

(define (record-type-sealed? rtd)
  (assert (record-type-descriptor? rtd))
  (fxbit-set? ($box-ref rtd 1) 33))   ;TODO: might be bignum

(define (record-type-opaque? rtd)
  (assert (record-type-descriptor? rtd))
  (fxbit-set? ($box-ref rtd 1) 32))   ;TODO: bug

(define (record-type-field-names rtd)
  (assert (record-type-descriptor? rtd))
  (vector-map (lambda (x) x) ($box-ref rtd 5)))

(define (record-field-mutable? rtd k)
  (assert (record-type-descriptor? rtd))
  (assert (and (fixnum? k) (fx>=? k 0)))
  (let* ((prtd (record-type-parent rtd))
         (k (if prtd (fx+ k ($record-length prtd)) k)))
    (assert (fx<? k ($record-length rtd)))
    (bitwise-bit-set? ($box-ref rtd 6) k)))

;;; Record writers

(define (default-record-writer v p wr)
  (let ((t (record-rtd* v)))
    (display "#[" p)
    (display (or (record-type-name t) (record-type-uid t)) p)
    (let lp ((t t))
      (when (and t (not (record-type-opaque? t)))
        (lp (record-type-parent t))
        (do ((i 0 (fx+ i 1))
             (fields (record-type-field-names t)))
            ((fx=? i (vector-length fields)))
          (let ((field (vector-ref fields i))
                (value ((record-accessor t i) v)))
            (display #\space p)
            (display field p)
            (display ": " p)
            (wr value p)))))
    (display "]" p)))

(define record-writer
  (case-lambda
    ((rtd)
     (assert (record-type-descriptor? rtd))
     ($box-ref rtd 7))
    ((rtd procedure)
     (assert (record-type-descriptor? rtd))
     (assert (procedure? procedure))
     ($box-set! rtd 7 procedure)))))
