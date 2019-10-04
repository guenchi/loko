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

;;; Standard library control operators

(library (loko runtime control)
  (export
    procedure?
    apply values call-with-values
    call/cc (rename (call/cc call-with-current-continuation)) call/1cc
    dynamic-wind
    with-exception-handler raise raise-continuable
    assertion-violation error
    make-promise force

    ;; Internal
    implementation-restriction
    register-error-invoker
    print-condition
    assertion-error
    current-winders)
  (import
    (except (rnrs)
            procedure?
            apply values call-with-values
            call/cc call-with-current-continuation
            dynamic-wind
            with-exception-handler raise raise-continuable
            assertion-violation error)
    (only (loko) parameterize make-parameter)
    (prefix (only (rnrs) procedure? apply) sys:)
    (loko system $primitives)
    (loko system $host)
    (only (loko runtime context)
          CPU-VECTOR:PROCESS-VECTOR
          PROCESS-VECTOR:ERROR-INVOKER)
    (only (loko runtime reader)
          annotation-source->condition))

(define (procedure? x) (sys:procedure? x))

(define apply
  (case-lambda
    ((f rest) (sys:apply f rest))
    ((f a rest) (sys:apply f a rest))
    ((f a b rest) (sys:apply f a b rest))
    ((f a b c rest) (sys:apply f a b c rest))
    ((f a b c d rest) (sys:apply f a b c d rest))
    ((f a b c d e rest) (sys:apply f a b c d e rest))
    ((f a b c d e . rest)
     (sys:apply f a b c d e
                (let lp ((rest rest))
                  (cond ((pair? (cdr rest))
                         ;; Move this element into the rest list. It
                         ;; was cons'd up by consargs.
                         (cons (car rest) (lp (cdr rest))))
                        (else
                         ;; The last element that was actually
                         ;; provided sent to apply and not constructed
                         ;; by consargs.
                         (car rest))))))))

;; Terrible temporary hack. Note that this should work:
;; (define (values . things)
;;    (call/cc (lambda (cont) (apply cont things))))
(define magic '#(mv))
(define values
  (case-lambda
    (() (if #f #f))
    ((x) x)
    (xs (cons magic xs))))
(define (call-with-values producer consumer)
  (let ((x (producer)))
    (if (and (pair? x) (eq? magic (car x)))
        (apply consumer (cdr x))
        (consumer x))))

;;; Continuations, dynamic-wind, exceptions.

(define *winders* '())

(define current-winders
  (case-lambda
    (() *winders*)
    ((winders) (set! *winders* winders))))

;; TODO: look at june-92-meeting.ps.gz

(define (call/cc proc)
  (let ((k ($copy-stack)))
    ;; The interesting thing about $copy-stack is that it will
    ;; return multiple times. The first time it returns a copy of
    ;; the stack. The following times it returns the values passed
    ;; to the continuation.
    (cond ((and ($box? k) (eq? ($box-type k) 'stack))
           ;; Based on code from SLIB's dynwind.scm.
           ;;
           ;; Copyright (c) 1992, 1993 Aubrey Jaffer
           ;;
           ;; Permission to copy this software, to modify it, to redistribute it,
           ;; to distribute modified versions, and to use it for any purpose is
           ;; granted, subject to the following restrictions and understandings.
           ;;
           ;; 1.  Any copy made of this software must include this copyright notice
           ;; in full.
           ;;
           ;; 2.  I have made no warranty or representation that the operation of
           ;; this software will be error-free, and I am under no obligation to
           ;; provide any services, by way of maintenance, update, or otherwise.
           ;;
           ;; 3.  In conjunction with products arising from the use of this
           ;; material, there shall be no use of my name in any advertising,
           ;; promotional, or sales literature without prior written consent in
           ;; each case.
           (letrec ((old-winders *winders*)
                    (do-winds
                     (lambda (to delta)
                       (cond ((eq? *winders* to))
                             ((fxnegative? delta)
                              (do-winds (cdr to) (fx+ delta 1))
                              ((caar to))
                              (set! *winders* to))
                             (else
                              (let ((from (cdar *winders*)))
                                (set! *winders* (cdr *winders*))
                                (from)
                                (do-winds to (fx- delta 1)))))))
                    (continuation
                     (case-lambda
                       ((v)
                        (do-winds old-winders
                                  (fx- (length *winders*)
                                       (length old-winders)))
                        ($restore-stack k v))
                       (v*
                        (do-winds old-winders
                                  (fx- (length *winders*)
                                       (length old-winders)))
                        ($restore-stack k (cons magic v*))))))
             (proc continuation)))
          (else
           k))))

(define call/1cc call/cc)

(define (dynamic-wind before thunk after)
  (before)
  (set! *winders* (cons (cons before after)
                        *winders*))
  (let-values ((v (thunk)))
    (set! *winders* (cdr *winders*))
    (after)
    (apply values v)))

;;; Exceptions

(define (default-exception-handler x)
  (define EX_SOFTWARE 70)            ;Linux: internal software error
  (define p (transcoded-port (standard-error-port) (native-transcoder)))
  (display "An exception has been raised, but no exception handler is installed.\n" p)
  (print-condition x p)
  (when (serious-condition? x)
    (exit EX_SOFTWARE)))

(define *exception-handlers*
  (make-parameter (list default-exception-handler
                        (lambda _ (exit 70)))))

(define (with-exception-handler handler thunk)
  (assert (procedure? handler))
  (assert (procedure? thunk))
  (parameterize ([*exception-handlers* (cons handler (*exception-handlers*))])
    (thunk)))

(define (raise obj)
  (let ((handlers (*exception-handlers*)))
    (parameterize ([*exception-handlers* (cdr handlers)])
      ((car handlers) obj)
      (raise (condition
              (make-non-continuable-violation))))))

(define (raise-continuable obj)
  (let ((handlers (*exception-handlers*)))
    (parameterize ([*exception-handlers* (cdr handlers)])
      ((car handlers) obj))))

(define (error who msg . irritants)
  (raise (condition
          (make-error)
          (if who (make-who-condition who) (condition))
          (make-message-condition msg)
          (make-irritants-condition irritants))))

(define (assertion-error expr pos)
  (raise (condition
          (make-assertion-violation)
          (make-who-condition 'assert)
          (make-message-condition "Assertion failed")
          (make-irritants-condition (list expr))
          (annotation-source->condition pos))))

(define (assertion-violation who msg . irritants)
  (define EX_SOFTWARE 70)            ;Linux: internal software error
  (cond
    ((not (procedure? condition))
     (string-for-each (lambda (c) ($debug-display c))
                      "Early assertion violation: ")
     (string-for-each (lambda (c) ($debug-display c))
                      msg)
     ($debug-display #\newline)
     ($debug-display who)
     ($debug-display #\newline)
     ($debug-display irritants)
     ($debug-display #\newline)
     (exit EX_SOFTWARE))             ;XXX: exit might not work
    (who
     (raise (condition
             (make-assertion-violation)
             (make-who-condition who)
             (make-message-condition msg)
             (make-irritants-condition irritants))))
    (else
     (raise (condition
             (make-assertion-violation)
             (make-message-condition msg)
             (make-irritants-condition irritants))))))

(define (implementation-restriction who msg . irritants)
  (raise (condition
          (make-implementation-restriction-violation)
          (make-who-condition who)
          (make-message-condition msg)
          (make-irritants-condition irritants))))

(define (print-condition exn p)
  (cond ((condition? exn)
         ;; TODO: does this have to consider sealed
         ;; or opaque condition types?
         (let ((c* (simple-conditions exn)))
           (display "The condition has " p)
           (display (length c*) p)
           (display " components:\n" p)
           (do ((i 1 (fx+ i 1))
                (c* c* (cdr c*)))
               ((null? c*))
             (let* ((c (car c*))
                    (rtd (record-rtd c)))
               (display " " p) (display i p) (display ". " p)
               (let loop ((rtd rtd))
                 (display (record-type-name rtd) p)
                 (cond ((record-type-parent rtd) =>
                        (lambda (rtd)
                          (unless (eq? rtd (record-type-descriptor &condition))
                            (display #\space p)
                            (loop rtd))))))
               (let loop ((rtd rtd))
                 (do ((f* (record-type-field-names rtd))
                      (i 0 (fx+ i 1)))
                     ((fx=? i (vector-length f*))
                      (cond ((record-type-parent rtd) => loop)))
                   (display "\n     " p)
                   (display (vector-ref f* i) p)
                   (display ": " p)
                   (let ((x ((record-accessor rtd i) c)))
                     (cond ((and (eq? rtd (record-type-descriptor &irritants))
                                 (pair? x) (list? x))
                            (display #\( p)
                            (write (car x) p)
                            (for-each (lambda (x)
                                        (display "\n                 " p)
                                        (write x p))
                                      (cdr x))
                            (display #\) p))
                           (else
                            (write x p)))))))
             (newline p)))
         (display "End of condition components.\n" p))
        (else
         (display "A non-condition object was raised:\n" p)
         (write exn p)
         (newline p))))

;;; Handler for hardware traps and explicit traps in generated code

;; The error invoker is supposed to be called when there's a trap.
;; Any errors trapped before this is useable results in double
;; traps. Any errors trapped before this runs result in a panic.
(define (register-error-invoker x)
  (vector-set! ($processor-data-ref CPU-VECTOR:PROCESS-VECTOR)
               PROCESS-VECTOR:ERROR-INVOKER x))

;;; Promises

;; From r6rs-lib

(define force
  (lambda (object)
    (object)))

(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
          (result #f))
      (lambda ()
        (if result-ready?
            result
            (let ((x (proc)))
              (if result-ready?
                  result
                  (begin (set! result-ready? #t)
                         (set! result x)
                         result)))))))))
