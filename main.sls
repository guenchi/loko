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

(library (loko main)
  (export
    main)
  (import
    (rnrs (6))
    (rnrs mutable-strings (6))
    (only (loko) compile-program)
    (only (loko init) init-set! set-file-mode)
    (only (loko repl) banner repl)
    (only (psyntax expander) compile-r6rs-top-level)
    (only (loko libs reader) read-annotated)
    (loko libs fibers)
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

(define (string-scheme-script? str)
  (define name "scheme-script")
  (let ((strlen (string-length str))
        (namelen (string-length name)))
    (and (>= strlen namelen)
         (string=? name (substring str (- strlen namelen) strlen)))))

(define (main)
  ;; Sanity check
  (guard (exn (else #f))
    (let ((msg "warning: Read-only data is not being protected from writes.\n"))
      (string-set! msg 0 #\W)
      (display msg (current-error-port))))

  ;; Parse the command line
  (match (command-line)
    [((? string-scheme-script? exec-name) . rest)
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
    [(exec-name "--compile" sps-fn "--output" out-fn)
     (compile-program out-fn sps-fn '())
     (set-file-mode out-fn #x755)]
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
     (exit 1)])))
