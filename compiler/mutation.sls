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

;;; Convert mutable variables to pairs.

;; This also inserts $global-set! for exported variables.

;; Previous passes have already determined which variables are
;; mutable. After this pass there are no more mutate records. TODO:
;; perhaps it would be useful to only wrap variables in cells if they
;; appear free?

(library (loko compiler mutation)
  (export
    pass-mutation)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map))

;; This is fragile... the code needs to be better at keeping apart
;; top level and lexical variables.
(define (wrap-mutable lhs* body)
  (let ((mutated (filter (lambda (x)
                           (and (variable-mutated? x)
                                (not (variable-export-name x))))
                         lhs*)))
    (if (null? mutated)
        body
        (make-bind mutated
                   (map (lambda (var)
                          (make-funcall (make-primref '$cons)
                                        (list (make-ref var)
                                              (make-const 'mutable #f))
                                        #f #f))
                        mutated)
                   body))))

(define (wrap-export lhs* body)
  (cond ((null? lhs*) body)
        ((variable-export-name (car lhs*)) =>
         (lambda (name)
           (make-seq (make-funcall (make-primref '$global-set!)
                                   (list (make-const name #f) (make-ref (car lhs*)))
                                   #f #f)
                     (wrap-export (cdr lhs*) body))))
        (else (wrap-export (cdr lhs*) body))))

(define (pass-mutation x)
  (define who 'pass-mutation)
  (define (pass x)
    ;; (display x)
    ;; (newline)
    (cond ((ref? x)
           (let* ((var (ref-name x))
                  (m (variable-mutated? var)))
             (cond ((not m)
                    x)
                   ((variable-export-name var) =>
                    (lambda (name)
                      ;; global reference
                      (make-funcall (make-primref '$global-ref)
                                    (list (make-const name #f))
                                    #f #f)))
                   (else
                    ;; cell reference
                    (make-funcall (make-primref 'car)
                                  (list x)
                                  #f #f)))))
          ((mutate? x)
           (let* ((var (mutate-name x))
                  (m (variable-mutated? var)))
             (cond ((not m)
                    (error who "Internal error: mutable not mutable?" x))
                   ((symbol? m)
                                        ;XXX: breaks when compiling all tests without cp0
                    (error who "TODO: what does this really mean?" x))
                   ((variable-export-name var) =>
                    (lambda (name)
                      ;; global mutation
                      (make-funcall (make-primref '$global-set!)
                                    (list (make-const name #f)
                                          (pass (mutate-expr x)))
                                    #f #f)))
                   (else
                    ;; cell mutation
                    (make-funcall (make-primref 'set-car!)
                                  (list (make-ref var) (pass (mutate-expr x)))
                                  #f #f)))))
          ((bind? x)
           ;; The bindings are partitioned into mutable and
           ;; immutable. The mutable bindings have to be wrapped in
           ;; pairs. Maybe the outer bindings should use different
           ;; variables?
           (make-bind (bind-lhs* x)
                      (map pass (bind-rhs* x))
                      (wrap-export (bind-lhs* x)
                                   (wrap-mutable (bind-lhs* x)
                                                 (pass (bind-body x))))))
          ((proc? x)
           ;; Same as with bind.
           (make-proc (proc-label x)
                      (proc-end-label x)
                      (map (lambda (x)
                             (make-proccase (proccase-info x)
                                            (wrap-mutable (caseinfo-formals
                                                           (proccase-info x))
                                                          (pass (proccase-body x)))))
                           (proc-cases x))
                      (proc-free x)
                      (proc-name x)
                      (proc-source x)))
          ((fix? x)
           ;; Only binds lambdas, but they can be exported
           (make-fix (fix-lhs* x)
                     (map pass (fix-rhs* x))
                     (wrap-export (fix-lhs* x)
                                  (pass (fix-body x)))))
          ;; The following are unaffected by this pass
          ((seq? x)
           (make-seq (pass (seq-e0 x))
                     (pass (seq-e1 x))))
          ((test? x)
           (make-test (pass (test-expr x))
                      (pass (test-then x))
                      (pass (test-else x))))
          ((funcall? x)
           (make-funcall (pass (funcall-operator x))
                         (map pass (funcall-operand* x))
                         (funcall-label x)
                         (funcall-source x)))
          ((const? x) x)
          ((primref? x) x)
          (else
           (error who "Unknown type" x))))
  (pass x)))
