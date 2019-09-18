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

;;; Converts psyntax's output to records

;; TODO: nanopass

(library (loko compiler recordize)
  (export
    pass-recordize record->sexpr
    fix? make-fix fix-lhs* fix-rhs* fix-body
    bind? make-bind bind-lhs* bind-rhs* bind-body
    rec? make-rec rec-lhs* rec-rhs* rec-body
    rec*? make-rec* rec*-lhs* rec*-rhs* rec*-body
    primref? make-primref primref-name
    funcall? make-funcall funcall-operator funcall-operand* funcall-label funcall-source
    const? make-const const-value const-ref set-const-ref!
    seq? make-seq seq-e0 seq-e1
    proc? make-proc proc-label proc-end-label proc-cases proc-free proc-name proc-source
    proccase? make-proccase proccase-info proccase-body
    caseinfo? make-caseinfo caseinfo-label caseinfo-formals caseinfo-proper?
    test? make-test test-expr test-then test-else
    variable? symbol->variable make-variable variable-name
    set-variable-operand! variable-operand
    variable-referenced? variable-mutated?
    variable-residual-referenced? variable-residual-mutated?
    set-variable-mutated?! set-variable-referenced?!
    set-variable-residual-mutated?! set-variable-residual-referenced?!
    variable-singly-referenced? variable-residual-singly-referenced?
    set-variable-singly-referenced?! set-variable-residual-singly-referenced?!
    variable-export-name
    ref? make-ref ref-name
    mutate? make-mutate mutate-name mutate-expr
    labels? make-labels labels-top-level-name labels-proc* labels-body
    closure? make-closure closure-code closure-free*
    tagbody? make-tagbody tagbody-label tagbody-body tagbody-source
    goto? make-goto goto-label goto-source
    infer? make-infer infer-expr infer-facts)
  (import
    (psyntax compat)          ;for define-record, gensym and annotations
    (rename (loko runtime utils) (map-in-order map))
    (except (rnrs) map))

(define-record rec (lhs* rhs* body))  ;letrec

(define-record rec* (lhs* rhs* body)) ;letrec*

(define-record primref (name))        ;built-in primitives

(define-record funcall (operator operand* label source)) ;application

(define-record const (value ref))     ;quote

(define-record seq (e0 e1))           ;begin

(define-record proc (label end-label cases free name source)) ;case-lambda
(define-record proccase (info body))
(define-record caseinfo (label formals proper?))

(define-record test (expr then else)) ;if

(define-record variable (name operand              ;operand for cp0
                              referenced? mutated? ;for pass-letrec
                              ;; for cp0:
                              residual-referenced? residual-mutated?
                              singly-referenced? residual-singly-referenced?
                              ;; for library-letrec*:
                              export-name))

;; TODO: it's possible that ref is useless, it should all be
;; variables directly.
(define-record ref (name))            ;variables

(define-record mutate (name expr))    ;set!

;; Introduced by pass-let:
(define-record bind (lhs* rhs* body)) ;let

;; Introduced by pass-letrec:
(define-record fix (lhs* rhs* body))  ;"fixing letrec"

;; Introduced by pass-loops:
(define-record tagbody (label body source))
(define-record goto (label source))

;; Introduced by pass-closure:
(define-record closure (code free*))
(define-record labels (top-level-name proc* body))

;; Introduced by pass-infer:
(define-record infer (expr facts))

(define (formals-to-list x)
  (cond ((null? x) x)
        ((pair? x)
         (cons (car x) (formals-to-list (cdr x))))
        (else (list x))))

(define (symbol->variable x)
  (make-variable x #f #f #f #f #f #t #t #f))

(define (inner-name name)
  (and (variable? name)
       (symbol->variable
        (gensym
         (string->symbol
          (string-append
           (symbol->string (variable-name name))
           "§"))))))

;; tail? is true in a tail-call context. name is the name to give
;; procedures (comes from letrecs)
(define (pass-recordize top-level-name x)
  (define (print . x) (for-each display x) (newline))
  ;; XXX: remove tail?.
  (define (pass x tail? name vars)
    (define (make-var x)
      ;; record a new lexical variable
      (let ((ret (symbol->variable x)))
        (hashtable-set! vars x ret)
        ret))
    (define (mk-caseinfo name formals)
      (let ((f (map make-var (formals-to-list formals)))
            (proper? (list? formals)))
        (make-caseinfo (gensym
                        (if proper?
                            (string-append (symbol->string name)
                                           "=" (number->string (length f)))
                            (string-append (symbol->string name)
                                           "≥" (number->string (fx- (length f) 1))
                                           "")))
                       f proper?)))
    ;; (print "#;" `(pass ,x ,tail? ,name))
    (if (symbol? x)
        (cond ((hashtable-ref vars x #f) =>
               (lambda (var)
                 (set-variable-referenced?! var #t)
                 (make-ref var)))
              (else
               (make-funcall (make-primref '$global-ref) (list (make-const x #f))
                             #f #f)))
        (case (car x)
          ((set!)
           (cond ((hashtable-ref vars (cadr x) #f) =>
                  (lambda (var)
                    (set-variable-mutated?! var #t)
                    (make-mutate var (pass (caddr x) #f var vars))))
                 (else
                  ;; These are generated by define in the REPL. Are
                  ;; they anywhere else?
                  (make-funcall (make-primref '$set-interaction-env!)
                                (list (make-const (cadr x) #f)
                                      (pass (caddr x) #f
                                            (symbol->variable (cadr x))
                                            vars))
                                #f #f))))
          ((library-letrec*)
           ;; (library-letrec* ([local-name exported-name expression] ...) body)
           (let ((binds (cadr x))
                 (body (caddr x)))
             (let ((lhs* (map make-var (map car binds)))
                   (export* (map cadr binds))
                   (rhs* (map caddr binds)))
               ;; Everything here can be exported, either explicitly
               ;; or implicitly (by a macro).
               (for-each set-variable-export-name! lhs* export*)
               (make-rec* lhs* (map (lambda (lhs rhs)
                                      (pass rhs #f lhs vars))
                                    lhs* rhs*)
                          (pass body tail? name vars)))))
          ((letrec* letrec)
           (let ((binds (cadr x))
                 (body (caddr x)))
             (let ((lhs* (map make-var (map car binds)))
                   (rhs* (map cadr binds)))
               ((if (eq? (car x) 'letrec*) make-rec* make-rec)
                lhs* (map (lambda (lhs rhs) (pass rhs #f lhs vars))
                          lhs* rhs*)
                (pass body tail? name vars)))))
          ((primitive)
           (make-primref (cadr x)))
          ((quote)
           ;; TODO: check that this is a proper datum and make sure
           ;; it's immutable
           (make-const (cadr x) #f))
          ((begin)
           (let lp ((args (cdr x)))
             (if (null? (cdr args))
                 (pass (car args) tail? name vars)
                 (make-seq (pass (car args) #f name vars)
                           (lp (cdr args))))))
          ((if)
           (make-test (pass (cadr x) #f name vars)
                      (pass (caddr x) tail? name vars)
                      (pass (cadddr x) tail? name vars)))

          ((annotated-case-lambda)
           (let ((source (cadr x))
                 (cases (cddr x)))
             (let* ((formals* (map car cases))
                    (body* (map cadr cases))
                    (name^ (variable-name name))
                    (label (gensym name^)))
               (make-proc label (vector 'end label)
                          (map (lambda (formals body)
                                 (let ((ci (mk-caseinfo name^ formals)))
                                   (make-proccase ci (pass body 'tail-context
                                                           (inner-name name) vars))))
                               formals* body*)
                          '()
                          (make-const name^ #f)
                          (make-const (and (annotation? source)
                                           (annotation-source source)
                                           #;
                                           (let* ((s (annotation-source source))
                                                  (fil (car s))
                                                  (pos (cdr s)))
                                             (cons (string->utf8 fil) pos)))
                                      #f)))))
          ((case-lambda)
           (let ((cases (cdr x)))
             (let* ((formals* (map car cases))
                    (body* (map cadr cases))
                    (name^ (variable-name name))
                    (label (gensym name^)))
               (make-proc label (vector 'end label)
                          (map (lambda (formals body)
                                 (let ((ci (mk-caseinfo name^ formals)))
                                   (make-proccase ci (pass body 'tail-context
                                                           (inner-name name) vars))))
                               formals* body*)
                          '()
                          (make-const name^ #f)
                          (make-const #f #f)))))

          ((annotated-call)
           (make-funcall (pass (caddr x) #f name vars)
                         (map (lambda (arg)
                                (pass arg #f name vars))
                              (cdddr x))
                         #f
                         ;; XXX: save the original expression?
                         (and (annotation? (cadr x))
                              (annotation-source (cadr x)))))
          (else
           (make-funcall (pass (car x) #f name vars)
                         (map (lambda (arg)
                                (pass arg #f name vars))
                              (cdr x))
                         #f #f)))))
  ;; Generate a name for top-level anonymous procedures.
  (let ((name (symbol->variable
               (gensym
                (call-with-string-output-port
                  (lambda (p)
                    (display "top-level§" p)
                    (let lp ((x top-level-name))
                      (cond ((null? x))
                            ((null? (cdr x))
                             (display (car x) p))
                            (else
                             (display (car x) p)
                             (display #\- p)
                             (lp (cdr x)))))))))))
    (pass x #f name (make-eq-hashtable))))

;; (for-each (lambda (code)
;;             (print "Library: " (car code) "\n")
;;             (psyntax:pretty-print
;;              (record->sexpr
;;               (pass-let
;;                (pass-recordize (cdr code))))))
;;           (psyntax:expand-top-level (read-top-level "test.scm")))


(define (record->sexpr x)
  (define who 'record->sexpr)
  (define strip-gensyms
    (let ((names (make-eq-hashtable)))
      (lambda (x)
        (define (strip1 x)
          (string->symbol (symbol->string x)))
        (define (make-sym base nums)
          (string->symbol
           (string-append (symbol->string (strip1 base))
                          "_"
                          (number->string (length nums)))))
        (define (strip x)
          (cond ((pair? x)
                 (cons (strip (car x)) (strip (cdr x))))
                ((vector? x)
                 (vector-map strip x))
                ((symbol? x)
                 (let* ((base (strip1 x))
                        (nums (hashtable-ref names base '())))
                   (cond ((eq? base x) x)
                         ((assq x nums) => cdr)
                         (else
                          (let ((name (make-sym x nums)))
                            (hashtable-set! names base (cons (cons x name) nums))
                            name)))))
                (else x)))
        (strip x))))
  (define (pass x)
    (cond ((fix? x)
           `(fix ,(map list (map strip-gensyms (map variable-name (fix-lhs* x)))
                       (map pass (fix-rhs* x)))
                 ,(pass (fix-body x))))
          ((bind? x)
           `(let ,(map list (map strip-gensyms (map variable-name (bind-lhs* x)))
                       (map pass (bind-rhs* x)))
              ;; TODO: unbegin the body
              ,(pass (bind-body x))))
          ((rec*? x)
           `(letrec* ,(map list (map strip-gensyms (map variable-name (rec*-lhs* x)))
                           (map pass (rec*-rhs* x)))
              ,(pass (rec*-body x))))
          ((rec? x)
           `(letrec ,(map list (map strip-gensyms (map variable-name (rec-lhs* x)))
                          (map pass (rec-rhs* x)))
              ,(pass (rec-body x))))
          ((caseinfo? x)
           (let ((f* (map variable-name (caseinfo-formals x))))
             (strip-gensyms
              (if (caseinfo-proper? x)
                  f*
                  (let* ((fr (reverse f*))
                         (rest (car fr)))
                    `(,@(reverse (cdr fr)) ,@rest))))))
          ((proc? x)
           (if (= (length (proc-cases x)) 1)
               `(lambda ,(pass (proccase-info (car (proc-cases x))))
                  ',(map pass (proc-free x))
                  ,(pass (proccase-body (car (proc-cases x)))))
               `(case-lambda
                  ,@(map (lambda (y)
                           (list (pass (proccase-info y))
                                 `',(map pass (proc-free x))
                                 (pass (proccase-body y))))
                         (proc-cases x)))))
          ((seq? x)
           `(begin ,(pass (seq-e0 x)) ,(pass (seq-e1 x))))
          ((mutate? x)
           `(set! ,(strip-gensyms (variable-name (mutate-name x)))
                  ,(pass (mutate-expr x))))
          ((test? x)
           `(if ,(pass (test-expr x))
                ,(pass (test-then x))
                ,(pass (test-else x))))
          ((const? x)
           `(quote ,(strip-gensyms (const-value x))))
          ((infer? x)
           `(infer ',(infer-facts x)
                   ,(pass (infer-expr x))))
          ((ref? x)
           (pass (ref-name x)))
          ((variable? x)
           (strip-gensyms (variable-name x)))
          ((funcall? x)
           `(,(pass (funcall-operator x))
             ,@(map pass (funcall-operand* x))))
          ((primref? x)
           (primref-name x))
          ((closure? x)
           `(closure ,(strip-gensyms (proc-label (closure-code x)))
                     ,@(strip-gensyms (map pass (closure-free* x)))))
          ((labels? x)
           `(let-values ,(map (lambda (x)
                                (list (strip-gensyms
                                       (list (or (const-value (proc-name x))
                                                 '*unknown-name*)
                                             (proc-label x)))
                                      (pass x)))
                              (labels-proc* x))
              ,(pass (labels-body x))))
          ((tagbody? x)
           `(tagbody ,(strip-gensyms (tagbody-label x))
                     ,(pass (tagbody-body x))))
          ((goto? x)
           `(goto ,(strip-gensyms (goto-label x))))
          (else
           (error who "Unknown type" x))))
  (pass x)))
