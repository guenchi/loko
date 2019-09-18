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

;;; Eval

;; The true R6RS eval procedure is in (psyntax expander). This is
;; compatibility stuff required by it.

;; This also hosts the interpreter, which will hopefully be going
;; away.

(library (loko runtime eval)
  (export
    void pretty-print eval-core
    expand expand/optimize)
  (import
    (except (rnrs) map)
    (rnrs mutable-pairs)

    (only (psyntax internal) current-primitive-locations)
    (only (psyntax expander) core-expand interaction-environment
          new-interaction-environment)

    (loko compiler recordize)
    (loko compiler let)
    (loko compiler letrec)
    (loko compiler cp0)
    (loko compiler mutation)
    (loko compiler freevar)
    (loko compiler loops)
    (loko compiler closure)
    (loko compiler infer)
    (loko compiler optimize)
    (loko config)
    (rename (loko runtime utils) (map-in-order map))
    (only (loko arch asm) code-generator instruction-analyzer
          target-convention assemble)
    (only (loko runtime symbols) $gensym-generate-names! symbol-value
          gensym? *unbound-hack*)
    (loko system $primitives))

(define (void)
  ;; TODO: to provide the #<void #x...> traceability, this object
  ;; should include the return address of this procedure.
  ;; This can be done with $stack-pointer, see stack-trace.
  (if #f #f))

(define (compile code)
  (define verbose #f)
  (define (show pass x)
    (when verbose
      (display "#;")
      (display pass)
      (display " ")
      (write (record->sexpr x))
      (newline))
    #f)
  (let* ((x (pass-recordize '(eval) code))
         (_ (show 'recordize x))
         (x (pass-let x))
         (_ (show 'let x))
         (x (pass-letrec x))
         (_ (show 'letrec x))
         (x (pass-cp0 x))
         (_ (show 'cp0 x))
         (x (pass-let x))
         (_ (show 'let x))
         (x (pass-mutation x))
         (_ (show 'mutation x)))
    #;
    (let* ((x (pass-infer x))
           (_ (show 'infer x))
           (x (pass-closure '(eval) x))
           (_ (show 'closure x))
           (target-cpu (config-target-cpu)))
      (let*-values ([(text data)
                     (let ([primlocs (current-primitive-locations)]
                           [make-init-code? #f])
                       (code-generator target-cpu (list x) primlocs make-init-code?))]
                    [(text data) (pass-optimize text data
                                                (instruction-analyzer target-cpu)
                                                (target-convention target-cpu))])
        ;; TODO:
        ;;  1. Code wants to init the global environment, but can't
        ;;  2. Symbols that are part of the output need a stable location
        ;;  3. Allocate memory for text and data
        ;;  4. Stack unwinding table needs to be extended or the mechanism needs to be redone.
        ;;  5. Garbage collect the new text/data
        (display "text:\n")
        (for-each (lambda (x) (write x) (newline)) text)
        (display "\ndata:\n")
        (for-each (lambda (x) (write x) (newline)) data)
        (newline)
        (let-values ([(code labels)
                      (assemble target-cpu
                                `((%origin #x600000)
                                  (%section text)
                                  (%label text)
                                  (%mode 64)
                                  (%equiv formals-mismatch ,($linker-address 'formals-mismatch))
                                  (%equiv stop-and-copy ,($linker-address 'stop-and-copy))
                                  ,@text
                                  (%label data)
                                  (%section data)
                                  ,@data
                                  (%label bss)
                                  (%section bss)
                                  (%label bss-end)))])
          (display code)
          (newline)
          (let-values ([(keys vals) (hashtable-entries labels)])
            (write (vector-map cons keys vals))
            (newline)))
        #f))
    x))

(define pretty-print write)

(define (expand x)
  (let-values (((code invoke-libraries) (core-expand x (interaction-environment))))
    (record->sexpr (pass-recordize '(expand) code))))

(define (expand/optimize x)
  (let-values (((code invoke-libraries) (core-expand x (interaction-environment))))
    (let* ((x (pass-recordize '(expand/optimize) code))
           (x (pass-let x))
           (x (pass-letrec x))
           (x (pass-cp0 x))
           (x (pass-let x))
           (x (pass-mutation x)))
      (record->sexpr x))))

;; Replaces primitive references with procedures. Replaces variables
;; and references with symbols. Replaces some records with lists
;; (which are significantly faster than records for now).
(define (record->eval-tree x)
  (define who 'record->eval-tree)
  (define (pass x)
    (cond ((ref? x) (variable-name (ref-name x)))
          ((const? x) `(quote ,(const-value x)))
          ((bind? x)
           `(let ,(map (lambda (lhs rhs)
                         (list (variable-name lhs) (pass rhs)))
                       (bind-lhs* x)
                       (bind-rhs* x))
              ,(pass (bind-body x))))
          ((fix? x)
           (make-fix (map variable-name (fix-lhs* x))
                     (map pass (fix-rhs* x))
                     (pass (fix-body x))))
          ((proc? x)
           (let ((cases (proc-cases x)))
             (if (and (pair? cases) (null? (cdr cases))
                      (caseinfo-proper? (proccase-info (car cases))))
                 (let* ((case0 (car cases))
                        (info0 (proccase-info case0)))
                   ;; Special-case easy lambdas
                   `(lambda ,(map variable-name (caseinfo-formals info0))
                      (quote ,(proc-name x))
                      ,(pass (proccase-body case0))))
                 (make-proc #f
                            #f
                            (map (lambda (x)
                                   (let ((info (proccase-info x)))
                                     (make-proccase
                                      (make-caseinfo #f
                                                     (map variable-name
                                                          (caseinfo-formals info))
                                                     (caseinfo-proper? info))
                                      (pass (proccase-body x)))))
                                 (proc-cases x))
                            #f
                            (proc-name x)
                            (proc-source x)))))
          ((seq? x)
           `(begin ,(pass (seq-e0 x))
                   ,(pass (seq-e1 x))))
          ((test? x)
           `(if ,(pass (test-expr x))
                ,(pass (test-then x))
                ,(pass (test-else x))))
          ((funcall? x)
           `(funcall ,(pass (funcall-operator x))
                     ,@(map pass (funcall-operand* x))))
          ((primref? x)
           (let ((name (primref-name x)))
             (cond (((current-primitive-locations) name) =>
                    (lambda (sym)
                      (let ((v (symbol-value sym)))
                        (if (procedure? v) v (list 'quote v)))))
                   ((eq? name '$set-interaction-env!)
                    ;; TODO:what was the point of this then?
                    (lambda (lhs rhs)
                      (assert (gensym? lhs))
                      ($box-set! lhs 2 rhs)))
                   ((eq? name '$global-ref)
                    ;; TODO: would be less hackish to use symbol-value, maybe
                    (lambda (lhs)
                      (assert (gensym? lhs))
                      (let ((x ($box-ref lhs 2)))
                        (if (eq? x *unbound-hack*)
                            ;; FIXME: should use make-undefined-violation
                            (error 'eval "Unbound variable" lhs)
                            x))))
                   ((eq? name '$global-set!)
                    ;; TODO: would be less hackish to use set-symbol-value!.
                    (lambda (lhs rhs)
                      (assert (gensym? lhs))
                      ($box-set! lhs 2 rhs)))
                   (else
                    (lambda _
                      (error 'eval "This primitive is not yet implemented" name))))))
          (else
           (error who "Unknown type" x))))
  (pass x))

(define (fast-assq obj alist)
  (let lp ((x alist))
    (if (eq? (caar x) obj)
        (car x)
        (lp (cdr x)))))

(define (eval-core code)
  (define debug #f)
  (let eval ((x (let ((compiled (compile code)))
                  (when debug
                    (display "#;EVAL: ")
                    (write (record->sexpr compiled))
                    (newline))
                  (record->eval-tree compiled)))
             (env '()))
    (cond
      ((symbol? x)
       (cdr (fast-assq x env)))
      ((pair? x)
       (case (car x)
         ((quote) (cadr x))
         ((if) (if (eval (cadr x) env)
                   (eval (caddr x) env)
                   (eval (cadddr x) env)))
         ((begin)
          (eval (cadr x) env)
          (eval (caddr x) env))

         ((let)
          (let lp ((env env) (bind* (cadr x)))
            (if (null? bind*)
                (eval (caddr x) env)
                (let ((bind (car bind*)))
                  (lp (cons (cons (car bind) (eval (cadr bind) env))
                            env)
                      (cdr bind*))))))
         ((funcall)
          (apply (eval (cadr x) env)
                 (map (lambda (x) (eval x env))
                      (cddr x))))
         ((lambda)
          ;; XXX: Special-casing for easy lambdas, with only a single
          ;; body and no rest arguments. FIXME: Set the name.
          (let ((formals (cadr x))
                (name (caddr x))
                (body (cadddr x)))
            (let ((len (length formals)))
              (case len
                ((2)
                 (lambda (x y)
                   (eval body `(,(cons (car formals) x)
                                ,(cons (cadr formals) y)
                                ,@env))))
                ((3)
                 (lambda (x y z)
                   (eval body `(,(cons (car formals) x)
                                ,(cons (cadr formals) y)
                                ,(cons (caddr formals) z)
                                ,@env))))
                ((0)
                 (lambda ()
                   (eval body env)))
                ((1)
                 (lambda (x)
                   (eval body `(,(cons (car formals) x) ,@env))))
                ((4)
                 (lambda (x y z w)
                   (eval body `(,(cons (car formals) x)
                                ,(cons (cadr formals) y)
                                ,(cons (caddr formals) z)
                                ,(cons (cadddr formals) w)
                                ,@env))))
                (else
                 (lambda x*
                   (unless (fx=? (length x*) len)
                     (assertion-violation 'apply "Wrong number of arguments" name x*))
                   (eval body (append (map cons formals x*) env))))))))
         (else
          (error 'eval-core "Internal error: unimplemented form" x))))
      ((procedure? x) x)
      ((fix? x)
       (let* ((new-vars (map (lambda (lhs) (cons lhs *unbound-hack*))
                             (fix-lhs* x)))
              (new-env (append new-vars env)))
         (for-each (lambda (lhs rhs)
                     (set-cdr! lhs (eval rhs new-env)))
                   new-vars
                   (fix-rhs* x))
         (eval (fix-body x) new-env)))
      ((proc? x)
       ;; FIXME: Set the name.
       (lambda args
         (let ((arglen (length args))
               (name (proc-name x)))
           (let lp ((cases (proc-cases x)))
             (if (null? cases)
                 (assertion-violation 'apply
                                      "Wrong number of arguments" name args)
                 (let* ((info (proccase-info (car cases)))
                        (fml* (caseinfo-formals info)))
                   (cond ((caseinfo-proper? info)
                          (if (fx=? (length fml*) arglen)
                              (eval (proccase-body (car cases))
                                    (append (map (lambda (fml arg)
                                                   (cons fml arg))
                                                 fml* args)
                                            env))
                              (lp (cdr cases))))
                         (else
                          (if (fx<=? (fx- (length fml*) 1) arglen)
                              (let lp ((args args)
                                       (fml* fml*)
                                       (env env))
                                (cond ((null? (cdr fml*))
                                       (eval (proccase-body (car cases))
                                             (cons (cons (car fml*) args)
                                                   env)))
                                      (else
                                       (lp (cdr args) (cdr fml*)
                                           (cons (cons (car fml*) (car args))
                                                 env)))))
                              (lp (cdr cases)))))))))))
      (else
       (error 'eval-core "Internal error: unimplemented form" x))))))
