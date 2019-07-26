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

;;; Pairs and lists

(library (loko libs pairs)
  (export
    pair? cons
    car caar caaar caaaar cdaaar cdaar cadaar cddaar cdar cadar caadar cdadar
    cddar caddar cdddar cdr cadr caadr caaadr cdaadr cdadr cadadr cddadr cddr
    caddr caaddr cdaddr cdddr cadddr cddddr
    null? list? list length append reverse list-tail list-ref
    map for-each
    find for-all exists filter partition fold-left fold-right
    remp remove remv remq
    memp member memv memq
    assp assoc assv assq
    cons*
    set-car! set-cdr!)
  (import
    (except (rnrs)
            pair? cons
            car caar caaar caaaar cdaaar cdaar cadaar cddaar cdar cadar caadar cdadar
            cddar caddar cdddar cdr cadr caadr caaadr cdaadr cdadr cadadr cddadr cddr
            caddr caaddr cdaddr cdddr cadddr cddddr
            null? list? list length append reverse list-tail list-ref
            map for-each
            find for-all exists filter partition fold-left fold-right
            remp remove remv remq
            memp member memv memq
            assp assoc assv assq
            cons*)
    (prefix (rnrs) sys:)
    (prefix (only (rnrs mutable-pairs) set-car! set-cdr!) sys:)
    (only (loko system $pairs) $cons))

(define-syntax define-integrable
  (lambda (x)
    (syntax-case x ()
      ((_ (name args ...) v)
       #'(define-syntax name (identifier-syntax (lambda (args ...) v)))))))

(define-integrable (%fast-map f x* nil)
  ;; This is map, except it only works with one list, and it need
  ;; not care about dynamic extents or improper lists, and appends a
  ;; tail.
  (let lp ((x* x*) (ret nil) (prev #f))
    (cond ((null? x*)
           ret)
          (else
           (let ((new-tail (cons (f (car x*)) nil)))
             (cond ((not prev)
                    (lp (cdr x*) new-tail new-tail))
                   (else
                    (set-cdr! prev new-tail)
                    (lp (cdr x*) ret new-tail))))))))

#;
(define-integrable (%map! f list)
  (let lp ((x* list))
    (cond ((null? x*)
           list)
          (else
           (set-car! x* (f (car x*)))
           (lp (cdr x*))))))

(define-integrable (%fast-for-all f x*)
  ;; Same as above.
  (let lp ((x* x*))
    (cond ((null? x*) #t)
          ((not (f (car x*))) #f)
          (else (lp (cdr x*))))))

(define (pair? x) (sys:pair? x))

(define (cons x y) ($cons x y))

(define (car x) (sys:car x))
(define (caar x) (sys:car (sys:car x)))
(define (caaar x) (sys:car (sys:car (sys:car x))))
(define (caaaar x) (sys:car (sys:car (sys:car (sys:car x)))))
(define (cdaaar x) (sys:cdr (sys:car (sys:car (sys:car x)))))
(define (cdaar x) (sys:cdr (sys:car (sys:car x))))
(define (cadaar x) (sys:car (sys:cdr (sys:car (sys:car x)))))
(define (cddaar x) (sys:cdr (sys:cdr (sys:car (sys:car x)))))
(define (cdar x) (sys:cdr (sys:car x)))
(define (cadar x) (sys:car (sys:cdr (sys:car x))))
(define (caadar x) (sys:car (sys:car (sys:cdr (sys:car x)))))
(define (cdadar x) (sys:cdr (sys:car (sys:cdr (sys:car x)))))
(define (cddar x) (sys:cdr (sys:cdr (sys:car x))))
(define (caddar x) (sys:car (sys:cdr (sys:cdr (sys:car x)))))
(define (cdddar x) (sys:cdr (sys:cdr (sys:cdr (sys:car x)))))
(define (cdr x) (sys:cdr x))
(define (cadr x) (sys:car (sys:cdr x)))
(define (caadr x) (sys:car (sys:car (sys:cdr x))))
(define (caaadr x) (sys:car (sys:car (sys:car (sys:cdr x)))))
(define (cdaadr x) (sys:cdr (sys:car (sys:car (sys:cdr x)))))
(define (cdadr x) (sys:cdr (sys:car (sys:cdr x))))
(define (cadadr x) (sys:car (sys:cdr (sys:car (sys:cdr x)))))
(define (cddadr x) (sys:cdr (sys:cdr (sys:car (sys:cdr x)))))
(define (cddr x) (sys:cdr (sys:cdr x)))
(define (caddr x) (sys:car (sys:cdr (sys:cdr x))))
(define (caaddr x) (sys:car (sys:car (sys:cdr (sys:cdr x)))))
(define (cdaddr x) (sys:cdr (sys:car (sys:cdr (sys:cdr x)))))
(define (cdddr x) (sys:cdr (sys:cdr (sys:cdr x))))
(define (cadddr x) (sys:car (sys:cdr (sys:cdr (sys:cdr x)))))
(define (cddddr x) (sys:cdr (sys:cdr (sys:cdr (sys:cdr x)))))

(define (null? x) (sys:null? x))

(define-syntax cdr/f
  (lambda (x)
    (syntax-case x ()
      ((_ v)
       #'(let ((t v))
           (and (sys:pair? t) (cdr t)))))))
(define-syntax cddr/f
  (lambda (x)
    (syntax-case x ()
      ((_ v)
       #'(let ((t v))
           (cdr/f (cdr/f t)))))))

(define (list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
        (let ((x (cdr x)))
          (if (pair? x)
              (let ((x (cdr x))
                    (lag (cdr lag)))
                (and (not (eq? x lag))
                     (lp x lag)))
              (null? x)))
        (null? x))))

(define (list . x) x)

(define (length x0)
  (define (err x)
    (assertion-violation 'length "Expected a proper list" x))
  (let lp ((x x0) (lag x0) (len 0))
    (if (pair? x)
        (let ((x (cdr x))
              (len (fx+ len 1)))
          (if (pair? x)
              (let ((x   (cdr x))
                    (lag (cdr lag))
                    (len (fx+ len 1)))
                (cond ((eq? x lag)
                       (err x0))
                      (else
                       (lp x lag len))))
              (if (null? x) len (err x0))))
        (if (null? x) len (err x0)))))

(define append
  (case-lambda
    ((x obj)
     (if (null? x)
         obj
         (let lp ((x x) (t (cdr/f x)) (h (cddr/f x)))
           (cond ((eq? t h)
                  (assertion-violation 'append "Expected a proper list" x obj))
                 ((null? t) (cons (car x) obj))
                 (else
                  (cons (car x) (lp (cdr x) (cdr/f t) (cddr/f h))))))))
    (xs0
     (let f ((xs xs0))
       (cond ((null? xs)
              '())
             ((null? (cdr xs))
              (car xs))
             (else
              (let ((x (car xs)) (xs (cdr xs)))
                (if (null? x)
                    (f xs)
                    (let lp ((x x) (t (cdr/f x)) (h (cddr/f x)))
                      (cond ((eq? t h)
                             (assertion-violation 'append "Expected a proper list" xs0))
                            ((null? t) (cons (car x) (f xs)))
                            (else
                             (cons (car x) (lp (cdr x) (cdr/f t) (cddr/f h))))))))))))))

(define (reverse x)
  (if (null? x)
      '()
      (let lp ((t (cdr/f x)) (h (cddr/f x)) (l x) (r '()))
        (cond ((eq? t h)
               (assertion-violation 'reverse "Expected a proper list" x))
              ((null? t) (cons (car l) r))
              (else
               (lp (cdr/f t) (cddr/f h) (cdr l) (cons (car l) r)))))))

(define (list-tail list k)
  (define who 'list-tail)
  (when (or (not (fixnum? k)) (fxnegative? k))
    (assertion-violation who "Expected a non-negative fixnum" k))
  (let lp ((l list) (i k))
    (cond ((eqv? i 0) l)
          ((pair? l) (lp (cdr l) (fx+ i -1)))
          (else
           (assertion-violation who "Expected a list of length at least k" list k)))))

(define (list-ref list k)
  (define who 'list-ref)
  ;; Cycles probably do not matter here.
  (when (or (not (fixnum? k)) (fxnegative? k))
    (assertion-violation who "Expected a non-negative fixnum" k))
  (let lp ((l list) (i k))
    (if (pair? l)
        (if (eqv? i 0) (car l) (lp (cdr l) (fx+ i -1)))
        (assertion-violation who "Expected a list of length at least k+1" list k))))

;; FIXME: error checking.
(define map
  (case-lambda
    ((f l)
     (let lp ((l l))
       (if (null? l)
           '()
           (sys:cons (f (car l)) (lp (sys:cdr l))))))
    ((f a b)
     (let lp ((a a)
              (b b))
       (if (null? a)
           '()
           (sys:cons (f (sys:car a) (sys:car b))
                     (lp (sys:cdr a) (sys:cdr b))))))
    ((f l . ls)
     (let lp ((l l)
              (ls ls))
       (if (null? l)
           '()
           (sys:cons (apply f (car l) (%fast-map sys:car ls '()))
                     (lp (cdr l) (%fast-map sys:cdr ls '()))))))))

;; TODO: fix this. more cases and checks. in particular, cycle-finding
(define for-each
  (case-lambda
    ((f l)
     (unless (null? l)
       (let lp ((l l))
         (cond ((null? (cdr l))
                (f (car l)))
               (else
                (f (car l))
                (lp (cdr l)))))))
    ((f l1 . ls)
     (define (go-wrong)
       (assertion-violation 'for-each
                            "The lists do not have the same length" l1 ls))
     (cond ((null? l1)
            (unless (%fast-for-all null? ls) (go-wrong)))
           (else
            (let lp ((l1 l1) (ls ls))
              (cond ((null? (cdr l1))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) ls)
                       (go-wrong))
                     (apply f (car l1) (%fast-map sys:car ls '())))
                    (else
                     (apply f (car l1) (%fast-map sys:car ls '()))
                     (let ((l1 (cdr l1)) (ls (%fast-map sys:cdr ls '())))
                       (let ((has-null (sys:exists sys:null? ls)))
                         (when (or (and (null? l1) (not has-null))
                                   (and (pair? l1) has-null))
                           (go-wrong)))
                       (lp l1 ls))))))))))

;;; (rnrs lists)

;; find is defined later.

(define for-all
  (case-lambda
    ((p l)
     (or (null? l)
         (let lp ((t (cdr/f l)) (h (cddr/f l)) (x l))
           (cond ((eq? t h)
                  (assertion-violation 'for-all "Expected a proper list" l))
                 ((null? (cdr x)) (p (car x)))
                 (else
                  (and (p (car x))
                       (lp (cdr/f t) (cddr/f h) (cdr x))))))))
    ((p l1 . ls)
     (define (go-wrong)
       (assertion-violation 'for-all
                            "The lists do not have the same length" l1 ls))
     (cond ((null? l1)
            (or (%fast-for-all null? ls) (go-wrong)))
           (else
            ;; FIXME: check that the lists are proper
            (let lp ((l1 l1) (ls ls))
              (cond ((null? (cdr l1))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) ls)
                       (go-wrong))
                     (apply p (car l1) (%fast-map sys:car ls '())))
                    (else
                     (and (apply p (car l1) (%fast-map sys:car ls '()))
                          (let ((l1 (cdr l1)) (ls (%fast-map sys:cdr ls '())))
                            (let ((has-null (sys:exists sys:null? ls)))
                              (when (or (and (null? l1) (not has-null))
                                        (and (pair? l1) has-null))
                                (go-wrong)))
                            (lp l1 ls)))))))))))

(define exists
  (case-lambda
    ((p l)
     (and (not (null? l))
          (let lp ((t (cdr/f l)) (h (cddr/f l)) (x l))
            (cond ((eq? t h)
                   (assertion-violation 'exists "Expected a proper list" l))
                  ((null? (cdr x)) (p (car x)))
                  (else
                   (or (p (car x))
                       (lp (cdr/f t) (cddr/f h) (cdr x))))))))
    ((p lst . lst*)
     (cond ((null? lst)
            (unless (%fast-for-all null? lst*)
              (apply assertion-violation 'exists
                     "The lists are not the same length"
                     p lst lst*))
            #f)
           (else
            (let lp ((t (cdr/f lst)) (h (cddr/f lst)) (x lst) (x* lst*))
              (cond ((eq? t h)
                     (apply assertion-violation 'exists
                            "The first list is not a proper list"
                            p lst lst*))
                    ((null? (cdr x))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) x*)
                       (if (%fast-for-all sys:list? x*)
                           (apply assertion-violation 'exists
                                  "One of the lists is longer than the first list"
                                  p lst lst*)
                           (apply assertion-violation 'exists
                                  "One of the lists is not a proper list"
                                  p lst lst*)))
                     (apply p (car x) (%fast-map sys:car x* '())))
                    (else
                     (let ((cdrs (%fast-map cdr x* '())))
                       (unless (%fast-for-all pair? cdrs)
                         (if (%fast-for-all sys:list? cdrs)
                             (apply assertion-violation 'exists
                                    "One of the lists is shorter than the first list"
                                    p lst lst*)
                             (apply assertion-violation 'exists
                                    "One of the lists is not a proper list"
                                    p lst lst*)))
                       (or (apply p (car x) (%fast-map car x* '()))
                           (lp (cdr/f t) (cddr/f h) (cdr x) cdrs)))))))))))

(define (partition proc list)
  (if (null? list)
      '()
      (let lp ((rett '()) (retf '()) (x list) (t (cdr/f list)) (h (cddr/f list)))
        (cond ((null? x)
               (values (sys:reverse rett) (sys:reverse retf)))
              ((eq? t h)
               (assertion-violation 'partition
                                    "This is not a proper list" list))
              ((proc (car x))
               (lp (cons (car x) rett) retf t (cdr/f t) (cddr/f h)))
              (else
               (lp rett (cons (car x) retf) t (cdr/f t) (cddr/f h)))))))

(define fold-left
  (case-lambda
    ((f nil lst)
     (if (null? lst)
         nil
         (let fold-left:lp ((acc nil) (t (cdr/f lst)) (h (cddr/f lst)) (x lst))
           (cond ((eq? t h)
                  (assertion-violation 'fold-left
                                       "The list argument is not a proper list"
                                       f nil lst))
                 ((null? (cdr x))
                  (f acc (car x)))
                 (else
                  (let ((acc (f acc (car x))))
                    (fold-left:lp acc (cdr/f t) (cddr/f h) (cdr x))))))))
    ((f nil lst . lst*)
     (cond ((null? lst)
            (unless (%fast-for-all null? lst*)
              (apply assertion-violation 'fold-left
                     "The lists are not the same length"
                     f nil lst lst*))
            nil)
           (else
            (let lp ((acc nil) (t (cdr/f lst)) (h (cddr/f lst)) (x lst) (x* lst*))
              (cond ((eq? t h)
                     (apply assertion-violation 'fold-left
                            "The first list is not a proper list"
                            f nil lst lst*))
                    ((null? (cdr x))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) x*)
                       (if (%fast-for-all sys:list? x*)
                           (apply assertion-violation 'fold-left
                                  "One of the lists is longer than the first list"
                                  f nil lst lst*)
                           (apply assertion-violation 'fold-left
                                  "One of the lists is not a proper list"
                                  f nil lst lst*)))
                     (apply f acc (car x) (%fast-map sys:car x* '())))
                    (else
                     ;; TODO: this could update x* in place and avoid
                     ;; making new lists. There could also be an
                     ;; apply-cars primitive that applies the cars of
                     ;; the lists, without building a new list to hold
                     ;; the cars.
                     (let ((cdrs (%fast-map sys:cdr x* '())))
                       (unless (%fast-for-all pair? cdrs)
                         (if (%fast-for-all sys:list? cdrs)
                             (apply assertion-violation 'fold-left
                                    "One of the lists is shorter than the first list"
                                    f nil lst lst*)
                             (apply assertion-violation 'fold-left
                                    "One of the lists is not a proper list"
                                    f nil lst lst*)))
                       (let ((acc (apply f acc (car x) (%fast-map sys:car x* '()))))
                         (lp acc (cdr/f t) (cddr/f h) (cdr x) cdrs)))))))))))

(define fold-right
  (case-lambda
    ((f nil lst)
     (if (null? lst)
         nil
         (let lp ((t (cdr/f lst)) (h (cddr/f lst)) (x lst))
           (cond ((eq? t h)
                  (assertion-violation 'fold-right
                                       "The list argument is not a proper list"
                                       f nil lst))
                 ((null? (cdr x))
                  (f (car x) nil))
                 (else
                  (f (car x) (lp (cdr/f t) (cddr/f h) (cdr x))))))))
    ((f nil lst . lst*)
     (cond ((null? lst)
            (unless (%fast-for-all sys:null? lst*)
              (apply assertion-violation 'fold-right
                     "The lists are not the same length"
                     f nil lst lst*))
            nil)
           (else
            (let lp ((t (cdr/f lst)) (h (cddr/f lst)) (x lst) (x* lst*))
              (cond ((eq? t h)
                     (apply assertion-violation 'fold-right
                            "The first list is not a proper list"
                            f nil lst lst*))
                    ((null? (cdr x))
                     (unless (%fast-for-all (lambda (l) (null? (cdr l))) x*)
                       ;; This is the error case, so it doesn't
                       ;; matter that it's a little slower.
                       (if (%fast-for-all sys:list? x*)
                           (apply assertion-violation 'fold-right
                                  "One of the lists is longer than the first list"
                                  f nil lst lst*)
                           (apply assertion-violation 'fold-right
                                  "One of the lists is not a proper list"
                                  f nil lst lst*)))
                     (apply f (car x) (%fast-map sys:car x* (list nil))))
                    (else
                     ;; TODO: this needs an apply-cars that also
                     ;; takes an extra last argument.
                     (let ((cdrs (%fast-map sys:cdr x* '())))
                       (unless (%fast-for-all sys:pair? cdrs)
                         (if (%fast-for-all sys:list? cdrs)
                             (apply assertion-violation 'fold-right
                                    "One of the lists is shorter than the first list"
                                    f nil lst lst*)
                             (apply assertion-violation 'fold-right
                                    "One of the lists is not a proper list"
                                    f nil lst lst*)))
                       (apply f (car x)
                              (let ((nil^ (lp (cdr/f t) (cddr/f h) (cdr x) cdrs)))
                                (%fast-map sys:car x* (list nil^)))))))))))))

(define-syntax define-remove
  (lambda (x)
    (syntax-case x ()
      ((_ name obj match?)
       #'(define (name obj list)
           (if (null? list)
               '()
               (let lp ((ret '()) (x list) (t (cdr/f list)) (h (cddr/f list)))
                 (cond ((null? x)
                        (sys:reverse ret))
                       ((eq? t h)
                        (assertion-violation 'name "Expected a proper list" list))
                       ((match? (car x))
                        (lp ret t (cdr/f t) (cddr/f h)))
                       (else
                        (lp (cons (car x) ret) t (cdr/f t) (cddr/f h)))))))))))

(define-remove filter proc (lambda (x) (not (proc x))))
(define-remove remp proc proc)
(define-remove remove obj (lambda (x) (equal? obj x)))
(define-remove remv obj (lambda (x) (eqv? obj x)))
(define-remove remq obj (lambda (x) (eq? obj x)))

(define-syntax define-member
  (lambda (x)
    (syntax-case x ()
      ((_ name obj return found?)
       #'(define (name obj list)
           (if (null? list)
               #f
               (let lp ((x list) (t (cdr/f list)) (h (cddr/f list)))
                 (cond ((eq? t h)
                        (assertion-violation 'name
                                             "This is not a proper list" list))
                       ((found? (car x)) (return x))
                       ((null? t) #f)
                       (else
                        (lp t (cdr/f t) (cddr/f h)))))))))))

(define-member find proc (lambda (x) (car x)) proc)
(define-member memp proc (lambda (x) x) proc)
(define-member member obj (lambda (x) x) (lambda (x) (equal? obj x)))
(define-member memv obj (lambda (x) x) (lambda (x) (eqv? obj x)))
(define-member memq obj (lambda (x) x) (lambda (x) (eq? obj x)))

(define-syntax define-assoc
  (lambda (x)
    (syntax-case x ()
      ((_ name obj found?)
       #'(define (name obj alist)
           (if (null? alist)
               #f
               (let lp ((x alist) (t (cdr/f alist)) (h (cddr/f alist)))
                 (cond ((or (eq? t h) (not (pair? (car x))))
                        (assertion-violation 'name
                                             "This is not a proper association list"
                                             alist))
                       ((found? (caar x)) (car x))
                       ((null? t) #f)
                       (else
                        (lp t (cdr/f t) (cddr/f h)))))))))))

(define-assoc assp proc proc)
(define-assoc assoc obj (lambda (x) (equal? obj x)))
(define-assoc assv obj (lambda (x) (eqv? obj x)))
(define-assoc assq obj (lambda (x) (eq? obj x)))

(define cons*
  (case-lambda
    ((a) a)
    ((a b) (sys:cons a b))
    ((a b c) (sys:cons a (sys:cons b c)))
    ((a b c d) (sys:cons a (sys:cons b (sys:cons c d))))
    (ret
     (let lp ((xs ret))
       (cond ((null? (cddr xs))
              (set-cdr! xs (cadr xs))
              ret)
             (else
              (lp (cdr xs))))))))

;; (rnrs mutable-pairs)
(define (set-car! x v) (sys:set-car! x v))
(define (set-cdr! x v) (sys:set-cdr! x v)))
