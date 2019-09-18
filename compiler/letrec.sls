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

;;; Implements the transformation described by Ghuloum and Dybvig in
;;; "Fixing Letrec (reloaded)" (2009).

;; rec and rec* are replaced with bind, mutate and fix.

(library (loko compiler letrec)
  (export
    pass-letrec)
  (import
    (loko compiler recordize)
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map)
    (rnrs mutable-pairs))

(define (print . x) (for-each display x) (newline))

(define-record-type vertex
  (sealed #t)
  (opaque #t)
  (nongenerative)
  (fields lhs rhs idx (mutable adjacency) (mutable number) (mutable lowlink))
  (protocol
   (lambda (p)
     (case-lambda
       ((lhs rhs idx)
        (p lhs rhs idx '() -1 -1))
       #;((lhs)
          (p lhs #f 0 '() -1 -1))))))

(define (vertex-edge! v w)
  ;; (print "#;edge " (vertex-lhs v) " <=> " (vertex-lhs w))
  (vertex-adjacency-set! v (cons w (vertex-adjacency v))))

;; Robert Tarjan's algorithm for finding strongly connected
;; components in a graph.
(define (tarjan! vertices)
  (let ((ret '())
        (i 0)
        (stack '()))                  ;empty stack of points
    (define (strong-connect! v)
      (define (span! pred? head)
        (let lp ((tail head) (prev #f))
          (cond ((null? tail)
                 (values head tail))
                ((pred? (car tail))
                 (lp (cdr tail) tail))
                ((not prev)
                 (values '() head))
                (else
                 (set-cdr! prev '())
                 (values head tail)))))
      (set! i (+ i 1))
      (vertex-lowlink-set! v i)
      (vertex-number-set! v i)
      (set! stack (cons v stack))
      (for-each (lambda (w)
                  (cond ((= (vertex-number w) -1)
                         ;; (v,w) is a tree arc
                         (strong-connect! w)
                         (vertex-lowlink-set! v (min (vertex-lowlink v)
                                                     (vertex-lowlink w))))
                        ((and (< (vertex-number w) (vertex-number v))
                              ;; (v,w) is a frond or cross-link
                              (memq w stack))
                         (vertex-lowlink-set! v (min (vertex-lowlink v)
                                                     (vertex-number w))))))
                (vertex-adjacency v))
      (when (= (vertex-lowlink v) (vertex-number v))
        ;; v is the root of a component; start the new strongly
        ;; connected component
        (let-values (((scc stack*)
                      (span! (lambda (w)
                               (>= (vertex-number w) (vertex-number v)))
                             stack)))
          ;; scc is a strongly connect component.
          (set! stack stack*)
          (set! ret (cons scc ret)))))
    (for-each (lambda (w)
                (when (= (vertex-number w) -1)
                  (strong-connect! w)))
              vertices)
    (reverse ret)))

;; from fixing letrec:
;; unreferenced: if x is unreferenced, else
;; simple: if x is unassigned and e is a simple expression, else
;; lambda: if x is unassigned and e is a lambda expression, and
;; complex: if it does not fall into any of the other categories.

(define (unreferenced? id)
  (not (variable-referenced? id)))

(define (unassigned? id)
  (not (variable-mutated? id)))

;; TODO: this can probably be done much faster
(define (free-variable? lhs x)
  (define who 'free-variable?)
  (define (free-variable? lhs x)
    ;; does lhs appear free in x?
    (cond ((bind? x)
           (or (exists (lambda (x) (free-variable? lhs x))
                       (bind-rhs* x))
               (free-variable? lhs (bind-body x))))
          ((rec*? x)
           (or (exists (lambda (x) (free-variable? lhs x))
                       (rec*-rhs* x))
               (free-variable? lhs (rec*-body x))))
          ((rec? x)
           (or (exists (lambda (x) (free-variable? lhs x))
                       (rec-rhs* x))
               (free-variable? lhs (rec-body x))))
          ((fix? x)
           (or (exists (lambda (x) (free-variable? lhs x))
                       (fix-rhs* x))
               (free-variable? lhs (fix-body x))))
          ((proc? x)
           (exists (lambda (x)
                     (free-variable? lhs (proccase-body x)))
                   (proc-cases x)))
          ((seq? x)
           (or (free-variable? lhs (seq-e0 x))
               (free-variable? lhs (seq-e1 x))))
          ((mutate? x)
           (or (eq? (mutate-name x) lhs)
               (free-variable? lhs (mutate-expr x))))
          ((test? x)
           (or (free-variable? lhs (test-expr x))
               (free-variable? lhs (test-then x))
               (free-variable? lhs (test-else x))))
          ((funcall? x)
           (or (free-variable? lhs (funcall-operator x))
               (exists (lambda (x) (free-variable? lhs x))
                       (funcall-operand* x))))
          ((const? x) #f)
          ((ref? x) (eq? lhs (ref-name x)))
          ((primref? x) #f)
          (else
           (error who "Unknown type" x))))
  (if (not (variable-referenced? lhs))
      #f
      (free-variable? lhs x)))

(define (might-cause-side-effects? x)
  ;; Answers this riddle: is it possible that x, an expression,
  ;; causes side effects?
  (define (P x)
    (cond ((bind? x)
           (or (exists P (bind-rhs* x))
               (P (bind-body x))))
          ((rec*? x)
           (or (exists P (rec*-rhs* x))
               (P (rec*-body x))))
          ((rec? x)
           (or (exists P (rec-rhs* x))
               (P (rec-body x))))
          ((fix? x)
           (or (exists P (fix-rhs* x))
               (P (fix-body x))))
          ((proc? x)
           #f)
          ((seq? x)
           (or (P (seq-e0 x))
               (P (seq-e1 x))))
          ((mutate? x)
           #t)
          ((test? x)
           (or (P (test-expr x))
               (P (test-then x))
               (P (test-else x))))
          ((funcall? x)
           ;; (print "should check for side-effects: " x)
           ;; TODO: be less conservative. cons with two arguments
           ;; never has a side-effect... unless the arguments have
           ;; side-effects
           #t)
          ((const? x) #f)
          ((ref? x) #f)
          ((primref? x) #f)
          (else #t)))
  (P x))

(define (pass-letrec x)
  (define who 'pass-letrec)
  (define (unassigned-procedure? b)
    ;; XXX: There are some easy ways to mess up this analysis, e.g.
    ;; by putting in a begin
    (and (unassigned? (vertex-lhs b))
         (proc? (vertex-rhs b))))
  (define (make-fixes vertices body)
    (if (null? vertices)
        body
        (make-fix (map vertex-lhs vertices)
                  (map vertex-rhs vertices)
                  body)))
  (define (make-mutations vertices body)
    (let lp ((V vertices))
      (if (null? V)
          body
          (let ((var (vertex-lhs (car V)))
                (init (vertex-rhs (car V))))
            ;; TODO: print source location
            (when (not (variable-mutated? var))
              ;; (print "WARNING: complex binding: " (record->sexpr var))
              (set-variable-mutated?! var (or (variable-export-name var) #t)))
            (make-seq (make-mutate var init)
                      (lp (cdr V)))))))
  (define (fix1 scc fixes body let-type)
    ;; body is the body to be used in the fix, let, etc
    (if (null? (cdr scc))
        ;; An SCC containing one binding
        (let ((b (car scc)))
          (let ((var (vertex-lhs b))
                (init (vertex-rhs b)))
            (cond ((unassigned-procedure? b)
                   ;; "If init is a lambda, and var is unassigned"
                   (values (cons b fixes) body))
                  ((not (free-variable? var init))
                   ;; "If var is not free in init". Consumes the
                   ;; fixes previously saved up.
                   (values '()
                           (make-bind (list var)
                                      (list init)
                                      (make-fixes fixes body))))
                  (else
                   ;; "Otherwise, we resort to assignment". Also
                   ;; consumes any fixes saved up.
                   (values '()
                           (make-bind (list var)
                                      (list (make-funcall (make-primref 'void) '() #f #f))
                                      (make-mutations (list b)
                                                      (make-fixes fixes body))))))))
        ;; An SCC with multiple bindings.
        (let-values (((l c) (partition unassigned-procedure? scc)))
          ;; l has lambda bindings, c has complex bindings
          (if (null? c)
              ;; <var_λ,init_λ> if init is a lambda expression and
              ;; var is unassigned.
              (values (append l fixes) body)
              (let ((c (if (eq? let-type 'letrec*)
                           (list-sort (lambda (a b)
                                        (< (vertex-idx a) (vertex-idx b)))
                                      c)
                           c)))
                ;; <var_c,init_c> otherwise.
                (values '()
                        (make-bind (map vertex-lhs c)
                                   (map (lambda (x)
                                          (make-funcall (make-primref 'void) '() #f #f))
                                        c)
                                   (make-fixes (append l fixes)
                                               (make-mutations c body)))))))))


  (define (fixing scc* body let-type)
    (let lp ((scc* scc*))
      (if (null? scc*)
          (values '() body)
          (let-values (((fixes body) (lp (cdr scc*))))
            (fix1 (car scc*) fixes body let-type)))))

  (define (rec-deps V lhs* rhs*)
    (for-each (lambda (v)
                (for-each
                 (lambda (w)
                   (when (and (not (eq? w v))
                              (free-variable? (vertex-lhs v)
                                              (vertex-rhs w)))
                     #;(print "the variable " (vertex-lhs v) " is free in "
                              (vertex-rhs w))
                     (vertex-edge! w v)))
                 V))
              V))
  (define (rec*-deps V lhs* rhs*)
    ;; Add letrec* dependencies
    (let lp ((w V))
      (cond ((null? w))
            ((pair? (cdr w))
             (let ((xj (cadr w))
                   (xi (car w)))
               ;; FIXME: only needed if xi might have a side-effect?
               (when (might-cause-side-effects? (vertex-rhs xi))
                 (vertex-edge! xj xi)))
             (lp (cdr w))))))

  (define (iota n)
    (let lp ((i 0))
      (if (fx=? n i)
          '()
          (cons i (lp (fx+ i 1))))))
  (define (fixing-letrec* lhs* rhs* body)
    (let ((V (map make-vertex lhs* rhs* (iota (length lhs*)))))
      (rec-deps V lhs* rhs*)
      (rec*-deps V lhs* rhs*)
      (let-values (((fixes body) (fixing (tarjan! V) body 'letrec*)))
        (make-fixes fixes body))))
  (define (fixing-letrec lhs* rhs* body)
    (let ((V (map make-vertex lhs* rhs* (iota (length lhs*)))))
      (rec-deps V lhs* rhs*)
      (let-values (((fixes body) (fixing (tarjan! V) body 'letrec)))
        (make-fixes fixes body))))

  (define (pass x)
    (cond ((rec*? x)
           (pass (fixing-letrec* (rec*-lhs* x)
                                 (rec*-rhs* x)
                                 (rec*-body x))))
          ((rec? x)
           (pass (fixing-letrec (rec-lhs* x)
                                (rec-rhs* x)
                                (rec-body x))))
          ((fix? x)
           (make-fix (fix-lhs* x)
                     (map pass (fix-rhs* x))
                     (pass (fix-body x))))
          ((bind? x)
           (make-bind (bind-lhs* x)
                      (map pass (bind-rhs* x))
                      (pass (bind-body x))))
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
           (make-funcall (pass (funcall-operator x))
                         (map pass (funcall-operand* x))
                         (funcall-label x)
                         (funcall-source x)))
          ((const? x) x)
          ((ref? x) x)
          ((primref? x) x)
          (else
           (error who "Unknown type" x))))
  (pass x)))
