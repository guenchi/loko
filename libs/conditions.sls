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

;;; Conditions

;; The list of rtds and rcds that must be exported can be found in
;; makefile.ss.

(library (loko libs conditions)
  (export
    condition simple-conditions condition?
    condition-predicate condition-accessor
    ;; Conditions
    &condition &condition-rtd &condition-rcd
    &message &message-rtd &message-rcd
    make-message-condition message-condition?
    condition-message
    &warning &warning-rtd &warning-rcd
    make-warning warning?
    &serious &serious-rtd &serious-rcd
    make-serious-condition serious-condition?
    &error &error-rtd &error-rcd
    make-error error?
    &violation &violation-rtd &violation-rcd
    make-violation violation?
    &assertion &assertion-rtd &assertion-rcd
    make-assertion-violation assertion-violation?
    &irritants &irritants-rtd &irritants-rcd
    make-irritants-condition irritants-condition?
    condition-irritants
    &who &who-rtd &who-rcd
    make-who-condition who-condition? condition-who
    &non-continuable &non-continuable-rtd &non-continuable-rcd
    make-non-continuable-violation
    non-continuable-violation?
    &implementation-restriction &implementation-restriction-rtd
    &implementation-restriction-rcd
    make-implementation-restriction-violation
    implementation-restriction-violation?
    &lexical &lexical-rtd &lexical-rcd
    make-lexical-violation lexical-violation?
    &syntax &syntax-rtd &syntax-rcd
    make-syntax-violation syntax-violation?
    syntax-violation-form syntax-violation-subform
    &undefined &undefined-rtd &undefined-rcd
    make-undefined-violation undefined-violation?
    ;; The I/O stuff
    &i/o &i/o-rtd &i/o-rcd
    make-i/o-error i/o-error?
    &i/o-read &i/o-read-rtd &i/o-read-rcd
    make-i/o-read-error i/o-read-error?
    &i/o-write &i/o-write-rtd &i/o-write-rcd
    make-i/o-write-error i/o-write-error?
    &i/o-invalid-position &i/o-invalid-position-rtd &i/o-invalid-position-rcd
    make-i/o-invalid-position-error
    i/o-invalid-position-error? i/o-error-position
    &i/o-filename &i/o-filename-rtd &i/o-filename-rcd
    make-i/o-filename-error i/o-filename-error?
    i/o-error-filename
    &i/o-file-protection &i/o-file-protection-rtd &i/o-file-protection-rcd
    make-i/o-file-protection-error
    i/o-file-protection-error?
    &i/o-file-is-read-only &i/o-file-is-read-only-rtd &i/o-file-is-read-only-rcd
    make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?
    &i/o-file-already-exists &i/o-file-already-exists-rtd &i/o-file-already-exists-rcd
    make-i/o-file-already-exists-error
    i/o-file-already-exists-error?
    &i/o-file-does-not-exist &i/o-file-does-not-exist-rtd &i/o-file-does-not-exist-rcd
    make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?
    &i/o-port &i/o-port-rtd &i/o-port-rcd
    make-i/o-port-error i/o-port-error? i/o-error-port
    &i/o-decoding &i/o-decoding-rtd &i/o-decoding-rcd
    make-i/o-decoding-error i/o-decoding-error?
    &i/o-encoding &i/o-encoding-rtd &i/o-encoding-rcd
    make-i/o-encoding-error i/o-encoding-error?
    i/o-encoding-error-char
    ;; Flonums
    &no-infinities &no-infinities-rtd &no-infinities-rcd
    make-no-infinities-violation
    no-infinities-violation?
    &no-nans &no-nans-rtd &no-nans-rcd
    make-no-nans-violation
    no-nans-violation?
    ;; Internal
    make-program-counter-condition)
  (import
    (rnrs base)
    (rnrs lists)
    (only (rnrs conditions) define-condition-type)
    (rnrs records procedural)
    (rnrs records syntactic)
    (rnrs syntax-case))

(define-record-type &condition)

(define &condition-rtd (record-type-descriptor &condition))

(define &condition-rcd (record-constructor-descriptor &condition))

(define-record-type compound-condition
  (fields components))

(define (condition . x)
  (let ((c* (apply append (map simple-conditions x))))
    (cond ((and (pair? c*) (null? (cdr c*)))
           (car c*))
          (else
           (make-compound-condition c*)))))

(define (simple-conditions c)
  (define who 'simple-conditions)
  (cond ((compound-condition? c)
         (apply list (compound-condition-components c)))
        ((&condition? c)
         (list c))
        (else
         (assertion-violation who "Expected a condition" c))))

(define (condition? x)
  (or (&condition? x)
      (compound-condition? x)))

(define (condition-predicate rtd)
  (let ((rtd? (record-predicate rtd)))
    (lambda (x)
      (cond ((&condition? x)
             (rtd? x))
            ((compound-condition? x)
             (exists rtd? (compound-condition-components x)))
            (else #f)))))

(define (condition-accessor rtd proc)
  (define who 'some-condition-accessor) ;TODO: make it nicer
  (let ((rtd? (record-predicate rtd)))
    (lambda (x)
      (define (error)
        (assertion-violation who "Not a condition of the expected type" rtd x))
      (cond ((rtd? x)
             (proc x))
            ((compound-condition? x)
             (let lp ((c* (compound-condition-components x)))
               (cond ((null? c*)
                      (error))
                     ((rtd? (car c*))
                      (proc (car c*)))
                     (else
                      (lp (cdr c*))))))
            (else
             (error))))))

;;; Standard condition types

;; More or less straight from the R6RS. Also exports the rtd and rcd
;; for psyntactical reasons.

(define-syntax define-condition-type*
  (lambda (x)
    (define (symcat x y)
      (datum->syntax x (string->symbol
                        (string-append
                         (symbol->string (syntax->datum x)) y))))
    (syntax-case x ()
      ((_ type parent make type? (slot* accessor*) ...)
       (with-syntax ((rtd (symcat #'type "-rtd"))
                     (rcd (symcat #'type "-rcd")))
         #'(begin
             (define-condition-type type parent
               make type?
               (slot* accessor*) ...)
             (define rtd (record-type-descriptor type))
             (define rcd (record-constructor-descriptor type))))))))

;; Conditions

(define-condition-type* &message &condition
  make-message-condition message-condition?
  (message condition-message))

(define-condition-type* &warning &condition
  make-warning warning?)

(define-condition-type* &serious &condition
  make-serious-condition serious-condition?)

(define-condition-type* &error &serious
  make-error error?)

(define-condition-type* &violation &serious
  make-violation violation?)

(define-condition-type* &assertion &violation
  make-assertion-violation assertion-violation?)

(define-condition-type* &irritants &condition
  make-irritants-condition irritants-condition?
  (irritants condition-irritants))

(define-condition-type* &who &condition
  make-who-condition who-condition?
  (who condition-who))

(define-condition-type* &non-continuable &violation
  make-non-continuable-violation
  non-continuable-violation?)

(define-condition-type* &implementation-restriction &violation
  make-implementation-restriction-violation
  implementation-restriction-violation?)

(define-condition-type* &lexical &violation
  make-lexical-violation lexical-violation?)

(define-condition-type* &syntax &violation
  make-syntax-violation syntax-violation?
  (form syntax-violation-form)
  (subform syntax-violation-subform))

(define-condition-type* &undefined &violation
  make-undefined-violation undefined-violation?)

;; I/O

(define-condition-type* &i/o &error
  make-i/o-error i/o-error?)

(define-condition-type* &i/o-read &i/o
  make-i/o-read-error i/o-read-error?)

(define-condition-type* &i/o-write &i/o
  make-i/o-write-error i/o-write-error?)

(define-condition-type* &i/o-invalid-position &i/o
  make-i/o-invalid-position-error
  i/o-invalid-position-error?
  (position i/o-error-position))

(define-condition-type* &i/o-filename &i/o
  make-i/o-filename-error i/o-filename-error?
  (filename i/o-error-filename))

(define-condition-type* &i/o-file-protection
    &i/o-filename
  make-i/o-file-protection-error
  i/o-file-protection-error?)

(define-condition-type* &i/o-file-is-read-only
    &i/o-file-protection
  make-i/o-file-is-read-only-error
  i/o-file-is-read-only-error?)

(define-condition-type* &i/o-file-already-exists
    &i/o-filename
  make-i/o-file-already-exists-error
  i/o-file-already-exists-error?)

(define-condition-type* &i/o-file-does-not-exist
    &i/o-filename
  make-i/o-file-does-not-exist-error
  i/o-file-does-not-exist-error?)

(define-condition-type* &i/o-port &i/o
  make-i/o-port-error i/o-port-error?
  (port i/o-error-port))

(define-condition-type* &i/o-decoding &i/o-port
   make-i/o-decoding-error i/o-decoding-error?)

(define-condition-type* &i/o-encoding &i/o-port
  make-i/o-encoding-error i/o-encoding-error?
  (char i/o-encoding-error-char))

;; Flonums

(define-condition-type* &no-infinities
    &implementation-restriction
  make-no-infinities-violation
  no-infinities-violation?)

(define-condition-type* &no-nans
    &implementation-restriction
  make-no-nans-violation no-nans-violation?)

;; Internal

(define-condition-type &program-counter &condition
  make-program-counter-condition program-counter-condition?
  (program-counter condition-program-counter)))
