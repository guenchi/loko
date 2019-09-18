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

;;; Simple(!) type inference

;; This introduces infer records. If the infer information part of the
;; record is is not wanted, then just use infer-expr.

;; TODO: "Flow Sensitive Type Recovery In Linear-Log Time"

(library (loko compiler infer)
  (export
    pass-infer)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map)
    (rnrs mutable-pairs))

;; TODO: do this properly! Here and in pass-cp0.
(define (target-fixnum? x)
  (fixnum? x))

(define (target-least-fixnum)
  (least-fixnum))

(define (target-greatest-fixnum)
  (greatest-fixnum))

(define (infer op facts)
  ;; This expression gets its own instance of the information. If
  ;; the information is copied, these same conses are reused. If
  ;; there is a mutation, then all copies of the information are
  ;; modified appropriately.
  (make-infer op (map (lambda (x)
                        (cons (car x) (cdr x)))
                      facts)))

(define (copy dst src)
  (if (infer? src)
      (make-infer dst (infer-facts src))
      dst))

(define (constant-positive-fixnum? x)
  (and (infer? x)
       (let ((e (infer-expr x)))
         (and (const? e)
              (target-fixnum? (const-value e))))))

(define (constant-fixnum-vector? x)
  (and (infer? x)
       (let ((e (infer-expr x)))
         (and (const? e)
              (let ((v (const-value e)))
                (and (vector? v)
                     (let lp ((i 0))
                       (or (fx=? i (vector-length v))
                           (and (target-fixnum? (vector-ref v i))
                                (lp (fx+ i 1)))))))))))

(define (pass-infer x)
  (define who 'pass-infer)
  (define (last x) (if (null? (cdr x)) (car x) (last (cdr x))))
  (define (pass x env)
    (cond ((bind? x)
           (let ((lhs*^ (bind-lhs* x))
                 (rhs*^ (map (lambda (e)
                               (pass e env))
                             (bind-rhs* x))))
             (let ((env^ (append (map cons lhs*^ rhs*^) env)))
               (let ((body^ (pass (bind-body x) env^)))
                 (copy (make-bind lhs*^ rhs*^ body^)
                       body^)))))
          ((fix? x)
           (let ((body^ (pass (fix-body x) env)))
             (copy (make-fix (fix-lhs* x)
                             (map (lambda (e) (pass e env)) (fix-rhs* x))
                             body^)
                   body^)))
          ((proc? x)
           (make-proc
            (proc-label x)
            (proc-end-label x)
            (map (lambda (x)
                   (let* ((info (proccase-info x))
                          (env^ (if #t ;; (caseinfo-proper? info)
                                    env
                                    (let ((opt (last (caseinfo-formals info))))
                                      ;; The rest args is a list.
                                      `((,opt . ,(infer opt `((type . list))))
                                        ,@env)))))
                     (make-proccase (proccase-info x)
                                    (pass (proccase-body x) env^))))
                 (proc-cases x))
            (proc-free x)
            (proc-name x)
            (proc-source x)))

          ((seq? x)
           (let ((e1^ (pass (seq-e1 x) env)))
             (copy (make-seq (pass (seq-e0 x) env)
                             e1^)
                   e1^)))
          ((test? x)
           (let ((expr^ (pass (test-expr x) env)))
             (let ((then^ (pass (test-then x) env))
                   (else^ (pass (test-else x) env)))
               (make-test expr^ then^ else^))))
          ((funcall? x)
           (let f ((op^ (pass (funcall-operator x) env))
                   (operand*^ (map (lambda (e) (pass e env)) (funcall-operand* x))))
             (let ((x^ (make-funcall op^
                                     operand*^
                                     (funcall-label x)
                                     (funcall-source x))))
               (case (and (primref? op^) (primref-name op^))
                 ((integer->char)
                  (infer x^ `((type . char))))
                 ((char->integer)
                  (infer x^ `((type . fixnum)
                              ;; (lower . 0)
                              ;; (upper . ,(- (expt 2 24) 1))
                              )))
                 ((string-ref)
                  (infer x^ `((type . char))))
                 ((bytevector-length string-length vector-length
                                     length)
                  (infer x^ `((type . fixnum)
                              ;; (lower . 0)
                              ;; (upper . ,(target-greatest-fixnum))
                              )))
                 ((bytevector-u8-ref get-mem-u8 get-i/o-u8)
                  (infer x^ `((type . fixnum)
                              ;; (lower . 0) (upper . 255)
                              )))
                 ((bytevector-u16-ref bytevector-u16-native-ref get-mem-u16 get-i/o-u16)
                  (infer x^ `((type . fixnum)
                              ;; (lower . 0) (upper . 65535)
                              )))
                 ((bytevector-u32-ref bytevector-u32-native-ref get-mem-u32 get-i/o-u32)
                  (assert (target-fixnum? (- (expt 2 32) 1)))
                  (infer x^ `((type . fixnum)
                              ;; (lower . 0) (upper . ,(- (expt 2 32) 1))
                              )))
                 ((get-mem-s61 rdtsc)
                  (assert (= (target-greatest-fixnum)
                             (- (expt 2 60) 1)))
                  (infer x^ `((type . fixnum)
                              ;; (lower . ,(target-least-fixnum))
                              ;; (upper . ,(target-greatest-fixnum))
                              )))
                 ((bytevector-s8-ref)
                  (infer x^ `((type . fixnum)
                              ;; (lower . ,(- (expt 2 7))) (upper . ,(- (expt 2 7) 1))
                              )))
                 ((bytevector-s16-ref bytevector-s16-native-ref)
                  (infer x^ `((type . fixnum)
                              ;; (lower . ,(- (expt 2 15))) (upper . ,(- (expt 2 15) 1))
                              )))
                 ((bytevector-s32-ref bytevector-s32-native-ref)
                  ;; FIXME: why assert?
                  (assert (target-fixnum? (- (expt 2 31) 1)))
                  (infer x^ `((type . fixnum)
                              ;; (lower . ,(- (expt 2 31))) (upper . ,(- (expt 2 31) 1))
                              )))
                 ((bytevector-ieee-single-native-ref
                   bytevector-ieee-single-ref)
                  (infer x^ '((type . flonum))))
                 ((fxmax fxmin fx+ fx* fx- fxdiv fxmod fxdiv0 fxmod0
                         fxnot fxand fxior fxxor fxif
                         fxcopy-bit fxbit-field fxcopy-bit-field
                         fxarithmetic-shift fxarithmetic-shift-left
                         fxarithmetic-shift-right
                         fxrotate-bit-field fxreverse-bit-field)
                  (infer x^ `((type . fixnum)
                              ;; (lower . ,(target-least-fixnum))
                              ;; (upper . ,(target-greatest-fixnum))
                              )))
                 ((real->flonum flmax flmin fl+ fl* fl- fl/ flabs fldiv flmod
                                fldiv0 flmod0 flnumerator fldenominator
                                flfloor flceiling fltruncate flround
                                flexp fllog flsin flcos fltan
                                flasin flacos flatan flsqrt flexpt
                                fixnum->flonum)
                  (infer x^ '((type . flonum))))
                 ((vector-ref)
                  ;; TODO: fails if immutability of vector constants
                  ;; is not enforced, which it should be.
                  (if (and (pair? operand*^)
                           (constant-fixnum-vector? (car operand*^)))
                      (infer x^ `((type . fixnum)))
                      x^))
                 ((bitwise-and)
                  (if (exists constant-positive-fixnum? operand*^)
                      (infer x^ '((type . fixnum)))
                      x^))
                 ;; Loko stuff.
                 ((syscall bytevector-address
                           $fxquotient $fxremainder $fxlength
                           $fxfirst-bit-set $immsym->fixnum
                           $void->fixnum)
                  (infer x^ '((type . fixnum))))
                 ;; Alternative calling conventions, higher order stuff.
                 ((apply fold-left fold-right)
                  (if (pair? operand*^)
                      ;; This is inaccurate.
                      (copy x^ (f (car operand*^) (cdr operand*^)))
                      x^))
                 (else x^)))))
          ((const? x)
           (let ((v (const-value x)))
             (cond
               ((target-fixnum? v)
                (infer x `((type . fixnum)
                           ;; (lower . ,v)
                           ;; (upper . ,v)
                           )))
               ((flonum? v)
                (infer x '((type . flonum))))
               ;; ((char? v)
               ;;  (infer x '((type . char))))
               ((vector? v)
                (infer x '((type . vector))))
               (else
                x))))
          ((ref? x)
           ;; XXX: Mutation has been changed into pairs already,
           ;; except for tagbody.
           (cond ((assq (ref-name x) env)
                  => (lambda (rhs)
                       (copy x (cdr rhs))))
                 (else
                  x)))
          ((primref? x) x)
          ((goto? x) x)
          ((tagbody? x)
           (let ((body^ (pass (tagbody-body x) env)))
             (copy (make-tagbody (tagbody-label x)
                                 body^
                                 (tagbody-source x))
                   body^)))
          ((mutate? x)
           ;; The mutation in a tagbody.
           ;; (write (map (lambda (l/r)
           ;;               (cons (record->sexpr (car l/r))
           ;;                     (record->sexpr (cdr l/r))))
           ;;             env))
           ;; (newline)
           ;; (write (record->sexpr x)) (newline)
           (let ((rhs (pass (mutate-expr x) env))
                 (v (cdr (assq (mutate-name x) env))))
             (when (infer? v)
               (when (or (not (infer? rhs))
                         (not (eq? (cdr (assq 'type (infer-facts v)))
                                   (cdr (assq 'type (infer-facts rhs))))))
                 ;; XXX: The new value of the variable does not
                 ;; agree with the original, so forget about the
                 ;; original fact.
                 (set-cdr! (assq 'type (infer-facts v))
                           'object)))
             (make-mutate (mutate-name x)
                          rhs)))
          (else
           (error who "Unknown type" x))))
  (pass x '())))
