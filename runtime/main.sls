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

;;; Main function for pid 1, which is the first process started by the
;;; scheduler (pid 0). In SRFI-18 terms, this is the primordial
;;; thread.

(library (loko runtime main)
  (export
    main)
  (import
    (rnrs (6))
    (rnrs mutable-strings (6))
    (only (loko) compile-program)
    (loko config)
    (only (loko runtime init) init-set! set-file-mode)
    (only (loko runtime repl) banner repl)
    (only (psyntax expander) compile-r6rs-top-level)
    (only (loko runtime reader) read-annotated)
    (loko runtime fibers)
    (loko match))

(define (skip-shebang p)
  (let* ((pos0 (port-position p))
         (shebang (get-string-n p 3)))
    (if (and (string? shebang)
             (or (string=? shebang "#!/")
                 (string=? shebang "#! ")))
        (begin (get-line p) #f)
        (set-port-position! p pos0))))

(define (read-code fn)
  (call-with-input-file fn
    (lambda (p)
      (skip-shebang p)
      (let lp ((codes '()))
        (let ((datum (read-annotated p fn)))
          (if (eof-object? datum)
              (reverse codes)
              (lp (cons datum codes))))))))

(define (run-program fn)
  (let* ((top-level (read-code fn))
         (proc (compile-r6rs-top-level top-level)))
    (proc)))

(define (string-suffix? sfx str)
  (let ((strlen (string-length str))
        (sfxlen (string-length sfx)))
    (and (fx>=? strlen sfxlen)
         (string=? sfx (substring str (fx- strlen sfxlen) strlen)))))

(define (string-prefix? pfx str)
  (let ((strlen (string-length str))
        (pfxlen (string-length pfx)))
    (and (fx>=? strlen pfxlen)
         (string=? pfx (substring str 0 pfxlen)))))

(define (main)
  ;; Sanity check
  (guard (exn (else #f))
    (let ((msg "warning: Read-only data is not being protected from writes.\n"))
      (string-set! msg 0 #\W)
      (display msg (current-error-port))))

  ;; Parse the command line. TODO: Get a real command line parser
  ;; (maybe srfi-37)
  (let loop-args ((args (command-line))
                  (options '()))
    (match args
      [((? (lambda (x) (string-suffix? "scheme-script" x)) exec-name) . rest)
       (match rest
         [(fn . rest)
          (init-set! 'command-line (cons fn rest))
          (run-program fn)]
         [()
          (display "Fatal: the Loko scheme-script program expects a filename.\n"
                   (current-error-port))
          (exit 1)])]
      [(exec-name (or "--script" "--program") fn . rest)
       (init-set! 'command-line (cons fn rest))
       (run-program fn)
       (flush-output-port (current-output-port))
       (exit 0)]

      ;; Compiler options
      [(exec-name (? (lambda (x) (string-prefix? "-ftarget=" x)) target) . rest)
       (let ((targetsym (string->symbol
                         (substring target (string-length "-ftarget=")
                                    (string-length target)))))
         (config-target-kernel targetsym))
       (loop-args (cons exec-name rest) options)]
      [(exec-name "-feval" . rest)
       (loop-args (cons exec-name rest) '(eval use-primlocs))]
      [(exec-name "-ffreestanding" . rest)
       (loop-args (cons exec-name rest) '(freestanding))]

      [(exec-name "--compile" sps-fn "--output" out-fn)
       (display "building with options ")
       (write options)
       (newline)
       (compile-program out-fn sps-fn options)
       (set-file-mode out-fn #o755)]
      [(exec-name)
       (banner)
       (repl)
       (flush-output-port (current-output-port))
       ;; All polite Schemes say good bye
       (display "Sabbaṁ pahāya gamanīyaṁ.\n")
       (exit 0)]
      [args
       (display "Fatal: unrecognized Loko command line:\n" (current-error-port))
       (write args (current-error-port))
       (newline (current-error-port))
       (exit 1)]))))
