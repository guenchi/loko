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

;;; Primitives for strings

(library (loko runtime strings)
  (export
    string? make-string string string-length string-ref
    string=? string<? string>? string<=? string>=?
    substring string-append string->list list->string
    string-for-each string-copy
    string-fill! string-set!

    string-ci=? ;; string-ci<? string-ci>? string-ci<=? string-ci>=?
    string-upcase string-downcase ;; string-foldcase string-titlecase
    ;; string-normalize-nfd string-normalize-nfkd
    string-normalize-nfc ;; string-normalize-nfkc
    )
  (import
    (except (rnrs)
            string? make-string string string-length string-ref
            string=? string<? string>? string<=? string>=?
            substring string-append string->list list->string
            string-for-each string-copy
            string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
            string-upcase string-downcase string-foldcase string-titlecase
            string-normalize-nfd string-normalize-nfkd
            string-normalize-nfc string-normalize-nfkc)
    (prefix (rnrs) sys:)
    (prefix (rnrs mutable-strings) sys:)
    (loko system $primitives))

(define (string? x) (sys:string? x))

(define make-string
  (case-lambda
    ((len)
     (make-string len #\nul))
    ((len fill)
     (assert (and (fixnum? len) (not (fxnegative? len)) (char? fill)))
     (if (eqv? len 0)
         ""
         (let ((v ($make-string len)))
           (unless (eqv? fill #\nul)
             (string-fill! v fill))
           v)))))

(define (string . chars)
  (let ((len (length chars)))
    (do ((s (make-string len))
         (i 0 (fx+ i 1))
         (x chars (cdr x)))
        ((null? x) s)
      (when (not (char? (car x)))
        (apply assertion-violation 'string "Expects characters" chars))
      (string-set! s i (car x)))))

(define (string-length s) (sys:string-length s))

(define (string-ref s i) (sys:string-ref s i))

(define string=?
  (case-lambda
    ((x1 x2)
     (or (eq? x1 x2)
         (and (fx=? (string-length x1) (string-length x2))
              (let ((len (string-length x1)))
                (let lp ((i 0))
                  (or (fx=? i len)
                      (and (eq? (string-ref x1 i) (string-ref x2 i))
                           (lp (fx+ i 1)))))))))
    ((x1 x2 . x*)
     (and (string=? x1 x2)
          (let lp ((x* x*))
            (or (null? x*)
                (and (string=? x1 (car x*))
                     (lp (cdr x*)))))))))

(define string<?
  (case-lambda
    ((x1 x2)
     (if (eq? x1 x2)
         #f
         (let lp ((i 0))
           (cond ((fx=? i (string-length x2)) #f)
                 ((fx=? i (string-length x1)) #t)
                 (else
                  (let ((c1 (string-ref x1 i))
                        (c2 (string-ref x2 i)))
                    (cond ((char>? c1 c2) #f)
                          ((char<? c1 c2) #t)
                          (else (lp (fx+ i 1))))))))))
    ((x1 x2 . x*)
     (and (string<? x1 x2)
          (let lp ((x x2) (x* x*))
            (or (null? x*)
                (and (string<? x (car x*))
                     (lp (car x*) (cdr x*)))))))))

(define string<=?
  (case-lambda
    ((x1 x2)
     (not (string>? x1 x2)))
    ((x1 x2 . x*)
     (and (string<=? x1 x2)
          (let lp ((x x2) (x* x*))
            (or (null? x*)
                (and (string<=? x (car x*))
                     (lp (car x*) (cdr x*)))))))))

(define string>?
  (case-lambda
    ((x1 x2)
     (string<? x2 x1))
    ((x1 x2 . x*)
     (and (string>? x1 x2)
          (let lp ((x x2) (x* x*))
            (or (null? x*)
                (and (string>? x (car x*))
                     (lp (car x*) (cdr x*)))))))))

(define string>=?
  (case-lambda
    ((x1 x2)
     (not (string<? x1 x2)))
    ((x1 x2 . x*)
     (and (string>=? x1 x2)
          (let lp ((x x2) (x* x*))
            (or (null? x*)
                (and (string>=? x (car x*))
                     (lp (car x*) (cdr x*)))))))))

(define (substring string start end)
  (let ((len (fx- end start)))
    (do ((new (make-string len))
         (i 0 (fx+ i 1)))
        ((fx=? i len) new)
      (string-set! new i (string-ref string (fx+ i start))))))

(define (string-append . x)
  (let ((length (apply + (map string-length x))))
    (do ((str (make-string length))
         (x x (cdr x))
         (n 0 (fx+ n (string-length (car x)))))
        ((null? x) str)
      (do ((s (car x))
           (i 0 (fx+ i 1)))
          ((fx=? i (string-length s)))
        (string-set! str (fx+ n i) (string-ref s i))))))

(define (string->list s)
  (do ((i (fx- (string-length s) 1) (fx- i 1))
       (ret '() (cons (string-ref s i) ret)))
      ((fx=? i -1) ret)))

(define (list->string list)
  ;; XXX: checks for cycles using length
  (do ((string (make-string (length list)))
       (n 0 (fx+ n 1))
       (l list (cdr l)))
      ((null? l) string)
    (when (not (char? (car l)))
      (apply assertion-violation 'list->string
             "Expected a list of characters" list))
    (string-set! string n (car l))))

(define string-for-each
  (case-lambda
    ((proc s1)
     (do ((length (string-length s1))
          (n 0 (fx+ n 1)))
         ((fx=? n length))
       (proc (string-ref s1 n))))
    ((proc s1 . s*)
     (let ((length (string-length s1)))
       (assert (apply = length (map string-length s*)))
       (do ((n 0 (fx+ n 1)))
           ((fx=? n length))
         (apply proc (string-ref s1 n)
                (map (lambda (sk) (string-ref sk n)) s*)))))))

(define (string-copy string)
  (do ((length (string-length string))
       (copy (make-string (string-length string)))
       (n 0 (fx+ n 1)))
      ((fx=? n length) copy)
    (string-set! copy n (string-ref string n))))

;; (rnrs mutable-strings)

(define (string-set! s i c)
  (sys:string-set! s i c))

(define (string-fill! s c)
  (assert (and (string? s) (char? c)))
  (do ((len (string-length s))
       (i 0 (fx+ i 1)))
      ((fx=? i len))
    (string-set! s i c)))

;; (rnrs unicode)

;;; TODO: real implementations

(define string-ci=?
  (case-lambda
    ((x1 x2)
     (and (fx=? (string-length x1) (string-length x2))
          (let ((len (string-length x1)))
            (let lp ((i 0))
              (or (fx=? i len)
                  (and (char-ci=? (string-ref x1 i) (string-ref x2 i))
                       (lp (fx+ i 1))))))))
    ((x1 x2 . x*)
     (and (string-ci=? x1 x2)
          (let lp ((x* x*))
            (or (null? x*)
                (and (string-ci=? x1 (car x*))
                     (lp (cdr x*)))))))))

(define (string-upcase str)
  (call-with-string-output-port
    (lambda (p)
      (string-for-each (lambda (c)
                         (put-char p (char-upcase c)))
                       str))))

(define (string-downcase str)
  (call-with-string-output-port
    (lambda (p)
      (string-for-each (lambda (c)
                         (put-char p (char-downcase c)))
                       str))))

(define (string-normalize-nfc str)
  str))
