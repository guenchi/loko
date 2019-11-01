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

(library (loko runtime repl)
  (export
    banner repl load)
  (import
    (rnrs)
    (rnrs eval)
    (only (loko runtime control) print-condition)
    (only (loko system $primitives) $void?)
    (only (loko system $host) stack-trace)
    (only (psyntax expander) interaction-environment new-interaction-environment)
    (only (psyntax compat) make-parameter parameterize)
    (only (akku metadata) main-package-version))

(define (banner)
  (display "Loko Scheme ")
  (display main-package-version)
  (display "
Copyright © 2019 Göran Weinholt
See <https://scheme.fail/> for source code and new releases.
License AGPLv3+: GNU Affero GPL version 3 or later
  <https://www.gnu.org/licenses/agpl.html>
There is NO WARRANTY, to the extent permitted by law. This is
free software: you are free to change and redistribute it under
certain conditions.\n"))

(define env (make-parameter #f))

(define (repl)
  (define (repl-internal)
    (call/cc
      (lambda (k)
        (with-exception-handler
          (lambda (exn)
            ;; TODO: flush output ports?
            (let ((p (current-error-port)))
              (stack-trace p)
              (print-condition exn p))
            (when (serious-condition? exn)
              (k 'restart)))
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
                            (unless ($void? x)
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
    (error 'load "Load can only be called from a repl" filename))
  (call-with-input-file filename
    (lambda (p)
      (let lp ()
        (let ((datum (read p)))
          (unless (eof-object? datum)
            (eval datum (env))
            (lp))))))))
