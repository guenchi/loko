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

;; REPL and related stuff.

(library (loko repl)
  (export
    banner repl print-condition load)
  (import
    (rnrs)
    (rnrs eval)
    (only (loko system $repl) stack-trace)
    (only (psyntax expander)
          interaction-environment new-interaction-environment)
    (only (psyntax compat)
          make-parameter parameterize)
    (only (akku metadata) main-package-version))

(define (banner)
  (display "Loko Scheme ")
  (display main-package-version)
  (display "
Copyright © 2019 Göran Weinholt
License AGPLv3+: GNU AGPL version 3 or later
  <https://www.gnu.org/licenses/agpl.html>
There is NO WARRANTY, to the extent permitted by law. This is
free software: you are free to change and redistribute it under
certain conditions.

"))

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

(define env (make-parameter #f))

(define (repl)
  (define (repl-internal)
    (call/cc
      (lambda (k)
        (with-exception-handler
          (lambda (exn)
            ;; TODO: what about warnings?
            ;; TODO: flush output ports?
            (let ((p (current-error-port)))
              (display ";;; \x1b;[1;31mAn unhandled exception has found the repl.\x1b;[0m\n"
                       p)
              (print-condition exn p)
              (stack-trace p))
            (k 'restart))
          (lambda ()
            (let loop ()
              (display "> ")
              (flush-output-port (current-output-port))
              (let ((datum (read)))
                (cond ((eof-object? datum)
                       (display "\nEnd of file read.\n")
                       'exit)
                      (else
                       (call-with-values
                         (lambda () (eval datum (env)))
                         (case-lambda
                           (() #f)
                           ((x)
                            (unless (eqv? x (if #f #f))
                              (write x) (newline)))
                           (x*
                            (for-each (lambda (x)
                                        (write x)
                                        (newline))
                                      x*))))
                       (loop))))))))))
  (unless (interaction-environment)
    (interaction-environment (new-interaction-environment)))
  (parameterize ((env (interaction-environment)))
    (let lp ()
      (case (repl-internal)
        ((restart) (lp))
        (else #f)))))

;; FIXME: This is really a bad implementation
(define (load filename)
  (unless (env)
    (error 'load "Load can only be called from a repl." filename))
  (call-with-input-file filename
    (lambda (p)
      (let lp ()
        (let ((datum (read p)))
          (unless (eof-object? datum)
            (eval datum (env))
            (lp))))))))
