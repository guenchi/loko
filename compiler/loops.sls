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

;;; Recover loops

;; TODO: contification.

(library (loko compiler loops)
  (export
    pass-loops)
  (import
    (loko compiler recordize)
    (loko compiler cp0)
    (only (psyntax compat) gensym)
    (rename (loko utils) (map-in-order map))
    (except (rnrs) map))

(define (pass-loops x)
  (define who 'pass-loops)
  (define (match-formals op args)
    (define (match? info args)
      ;; TODO: here it is possible to match on formals with rest
      ;; arguments as well, but the resultant binding must create a
      ;; list. Unless I'm wrong.
      (let ((formals (caseinfo-formals info)))
        ;; XXX: optimize-loop depends on this being proper
        (and (caseinfo-proper? info)
             (= (length formals)
                (length args)))))
    (let lp ((cases (proc-cases op)))
      (cond ((null? cases)
             #f)
            ((match? (proccase-info (car cases)) args)
             (car cases))
            (else
             (lp (cdr cases))))))
  (define (proccase-from-ref op operands)
    (and (ref? op)
         (let* ((var (ref-name op))
                (operand (variable-operand var)))
           (and (not (variable-mutated? var))
                (operand? operand)
                (proc? (operand-value operand))
                ;; XXX: must not use proccase from operand-value in
                ;; loop optimization, because its code has not been
                ;; through the rest of the compiler passes.
                (match-formals (operand-value operand)
                               operands)))))

  (define (misused? x tail? name formal*)
    ;; This returns true if the name can be found in a place that
    ;; means the code is not a proper loop.
    (cond ((bind? x)
           (or (exists (lambda (x) (misused? x #f name formal*)) (bind-rhs* x))
               (misused? (bind-body x) tail? name formal*)))
          ((fix? x)
           (or (exists (lambda (x) (misused? x #f name formal*)) (fix-rhs* x))
               (misused? (fix-body x) tail? name formal*)))
          ((proc? x)
           (exists (lambda (x) (misused? (proccase-body x) #t name formal*))
                   (proc-cases x)))
          ((seq? x)
           (or (misused? (seq-e0 x) #f name formal*)
               (misused? (seq-e1 x) tail? name formal*)))
          ((mutate? x)
           (misused? (mutate-expr x) #f name formal*))
          ((test? x)
           (or (misused? (test-expr x) #f name formal*)
               (misused? (test-then x) tail? name formal*)
               (misused? (test-else x) tail? name formal*)))
          ((funcall? x)
           ;; The name may only be *directly* in the operator
           ;; position and this must be a tail call.
           (let ((op (funcall-operator x))
                 (operand* (funcall-operand* x)))
             (or (and (ref? op) (eq? (ref-name op) name)
                      (or (not tail?)
                          (not (= (length operand*) (length formal*)))))
                 (and (not (ref? op)) (misused? op #f name formal*))
                 (exists (lambda (x) (misused? x #f name formal*)) operand*))))
          ((const? x) #f)
          ((primref? x) #f)
          ((ref? x) (eq? (ref-name x) name))
          ((goto? x) #f)
          ((tagbody? x)
           (misused? (tagbody-body x) tail? name formal*))
          (else
           (error who "Unknown type when checking for misuses" x))))

  (define (captured? x in-lambda? name)
    ;; This returns true if the variable might be captured (it
    ;; appears free in some lambda).
    ;; (display "captured? ")
    ;; (write (list in-lambda? name))
    ;; (newline)
    ;; (write (record->sexpr x)) (newline)
    ;; TODO: maybe use proc-free.
    (cond ((bind? x)
           (or (exists (lambda (x) (captured? x in-lambda? name)) (bind-rhs* x))
               (captured? (bind-body x) in-lambda? name)))
          ((fix? x)
           (or (exists (lambda (x) (captured? x in-lambda? name)) (fix-rhs* x))
               (captured? (fix-body x) in-lambda? name)))
          ((proc? x)
           (exists (lambda (x) (captured? (proccase-body x) #t name))
                   (proc-cases x)))
          ((seq? x)
           (or (captured? (seq-e0 x) in-lambda? name)
               (captured? (seq-e1 x) in-lambda? name)))
          ((mutate? x)
           (captured? (mutate-expr x) in-lambda? name))
          ((test? x)
           (or (captured? (test-expr x) in-lambda? name)
               (captured? (test-then x) in-lambda? name)
               (captured? (test-else x) in-lambda? name)))
          ((funcall? x)
           ;; The name may only be *directly* in the operator
           ;; position and this must be a tail call.
           (or (captured? (funcall-operator x) in-lambda? name)
               (exists (lambda (x) (captured? x in-lambda? name))
                       (funcall-operand* x))))
          ((const? x) #f)
          ((primref? x) #f)
          ((ref? x)
           (and in-lambda? (eq? (variable-name (ref-name x))
                                (variable-name name))))
          ((goto? x) #f)
          ((tagbody? x)
           (captured? (tagbody-body x) in-lambda? name))
          (else
           (error who "Unknown type when checking for free variables" x))))

  (define (optimizeable-loop? lhs* rhs* body)
    ;; The fix must contain a single procedure and the body must be
    ;; a call to one of the proccases in that procedure. The name of
    ;; the procedure must furthermore not be misused. None of the
    ;; formals of the proccase can be captured in the proccase.
    (and (pair? lhs*)
         (null? (cdr lhs*))
         (funcall? body)
         (let ((op (funcall-operator body))
               (operands (funcall-operand* body)))
           (cond
             ((and (ref? op)
                   (proc? (car rhs*))
                   (match-formals (car rhs*) operands)) =>
                   (lambda (c)
                     (let ((name (ref-name op))
                           (formal* (caseinfo-formals (proccase-info c))))
                       (and (eq? name (car lhs*))
                            (not (variable-mutated? (car lhs*)))
                            (not (exists variable-mutated? formal*))
                            (not (misused? body #t name formal*))
                            (not (misused? (car rhs*) #f name formal*))
                            (not (exists (lambda (var)
                                           (or (captured? (proccase-body c) #f var)
                                               (exists (lambda (operand)
                                                         (captured? operand #f var))
                                                       operands)))
                                         (cons name formal*)))
                            c))))
             (else #f)))))

  (define (optimize-loop name proc proccase operand*)
    ;; This generates a replacement for the fix record. The
    ;; replacement is a bind in which there's an tagbody.
    ;; The first body of the fix becomes the bind and any later
    ;; calls to “name” are replaced with bind, set! and goto.
    (let ((lhs* (caseinfo-formals (proccase-info proccase)))
          (asm-label (vector 'loop (variable-name name))))
      (define (rewrite x)
        (define (fresh-variable x)
          (make-variable (gensym (variable-name x)) #f #t #f #f #f #f #f #f))
        ;; (display x)
        ;; (newline)
        (cond
          ((bind? x)
           (make-bind (bind-lhs* x)
                      (map rewrite (bind-rhs* x))
                      (rewrite (bind-body x))))
          ((fix? x)
           (make-fix (fix-lhs* x)
                     (map rewrite (fix-rhs* x))
                     (rewrite (fix-body x))))
          ((proc? x)
           (make-proc (proc-label x)
                      (proc-end-label x)
                      (map (lambda (x)
                             (make-proccase (proccase-info x)
                                            (rewrite (proccase-body x))))
                           (proc-cases x))
                      (proc-free x)
                      (proc-name x)
                      (proc-source x)))
          ((seq? x)
           (make-seq (rewrite (seq-e0 x))
                     (rewrite (seq-e1 x))))
          ((mutate? x)
           (make-mutate (mutate-name x)
                        (rewrite (mutate-expr x))))
          ((test? x)
           (make-test (rewrite (test-expr x))
                      (rewrite (test-then x))
                      (rewrite (test-else x))))
          ((funcall? x)
           (let ((op (funcall-operator x))
                 (operand* (map rewrite (funcall-operand* x))))
             (cond ((and (ref? op) (eq? (ref-name op) name))
                    ;; Residualize a goto.
                    (let ((temp* (map fresh-variable lhs*)))
                      (make-bind temp* operand*
                                 (let lp ((temp* temp*) (lhs* lhs*))
                                   (cond ((null? temp*)
                                          (make-goto asm-label (funcall-source x)))
                                         (else
                                          ;; These may be mutations,
                                          ;; but they only modify
                                          ;; local variables, so
                                          ;; they are not a problem.
                                          (make-seq (make-mutate (car lhs*)
                                                                 (make-ref (car temp*)))
                                                    (lp (cdr temp*)
                                                        (cdr lhs*)))))))))
                   (else
                    (make-funcall (rewrite op)
                                  operand*
                                  (funcall-label x)
                                  (funcall-source x))))))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          ((goto? x) x)
          ((tagbody? x)
           (make-tagbody (tagbody-label x)
                         (rewrite (tagbody-body x))
                         (tagbody-source x)))
          (else
           (error who "Unknown type when rewriting" x))))
      (make-bind lhs* operand*
                 (make-tagbody asm-label
                               (pass (rewrite (proccase-body proccase)))
                               (proc-source proc)))))

  (define (pass x)
    (cond ((bind? x)
           (make-bind (bind-lhs* x)
                      (map pass (bind-rhs* x))
                      (pass (bind-body x))))
          ((fix? x)
           (let ((lhs* (fix-lhs* x))
                 (rhs* (map pass (fix-rhs* x)))
                 (body (fix-body x)))
             ;; Check if the body merely calls a proccase in the
             ;; fix. Also do misuse? on the body.
             (cond ((optimizeable-loop? lhs* rhs* body) =>
                    (lambda (c)
                      ;; (display "GENTLEMEN, WE HAVE THE TECHNOLOGY\n")
                      (let ((x (optimize-loop (car lhs*) (car rhs*)
                                              c (map pass (funcall-operand* body)))))
                        ;; (display "here it is:\n")
                        ;; (write (record->sexpr x))
                        ;; (newline)
                        x)))
                   (else
                    ;; TODO: issue an optimization warning?
                    (make-fix lhs* rhs* (pass body))))))
          ;; ((rec*? x)
          ;;  (make-rec* (rec*-lhs* x)
          ;;             (map (lambda (x) (pass x #f)) (rec*-rhs* x))
          ;;             (pass (rec*-body x) tail?)))
          ;; ((rec? x)
          ;;  (make-rec (rec-lhs* x)
          ;;            (map (lambda (x) (pass x #f)) (rec-rhs* x))
          ;;            (pass (rec-body x) tail?)))
          ((proc? x)
           (make-proc (proc-label x)
                      (proc-end-label x)
                      (map (lambda (x)
                             (make-proccase (proccase-info x)
                                            (pass (proccase-body x))))
                           (proc-cases x))
                      (proc-free x)
                      (proc-name x)
                      (proc-source x)))
          ((seq? x)
           (make-seq (pass (seq-e0 x))
                     (pass (seq-e1 x))))
          ((mutate? x)
           (make-mutate (mutate-name x)
                        (pass (mutate-expr x))))
          ((test? x)
           (make-test (pass (test-expr x))
                      (pass (test-then x))
                      (pass (test-else x))))
          ((funcall? x)
           ;; This is the magic that handles self-recursion.
           (let ((op (funcall-operator x))
                 (operands (funcall-operand* x)))
             ;; Check if the operator is a variable reference which
             ;; is known to be bound to a procedure.
             (cond
               ((proccase-from-ref op operands) =>
                (lambda (c)
                  ;; The label is known, so at least that's an
                  ;; improvement. XXX: this can also use the label
                  ;; for the procedure's general entry point if none
                  ;; of the proccases should match. it might also
                  ;; handle apply.
                  (make-funcall (pass op)
                                (map pass operands)
                                (caseinfo-label (proccase-info c))
                                (funcall-source x))))
               (else
                (make-funcall (pass op)
                              (map pass operands)
                              (funcall-label x)
                              (funcall-source x))))))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          ((goto? x) x)
          ((tagbody? x)
           (make-tagbody (tagbody-label x)
                         (pass (tagbody-body x))
                         (tagbody-source x)))
          ;; ((closure? x) x)
          ;; ((labels? x)
          ;;  (make-labels (map pass (labels-proc* x))
          ;;               (pass (labels-body x))))
          (else
           (error who "Unknown type" x))))
  ;; (write (record->sexpr x))
  ;; (newline)
  (pass x)))
