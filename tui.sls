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

;;; Text User Interface. Mainly for the text mode console.

;; Pretty rinky dink at the moment.

(library (loko tui)
  (export
    tui)
  (import
    (rnrs)
    (rnrs eval)
    (rnrs mutable-strings)
    (only (loko init) init-set!)
    (only (loko repl) repl)
    (loko system $host)
    (only (psyntax expander)
          interaction-environment new-interaction-environment)
    (only (psyntax compat)
          make-parameter parameterize)
    (text-mode console)
    (text-mode console events)
    (text-mode platform)
    (text-mode terminfo))

(define (println/newlines str)
  (let ((p (open-string-input-port str)))
    (let lp ()
      (let ((line (get-line p)))
        (unless (eof-object? line)
          (println line)
          (lp))))))

(define (print/write datum)
  (print
   (call-with-string-output-port
     (lambda (p) (write datum p)))))

(define (print-condition exn)
  (cond ((condition? exn)
         ;; TODO: does this have to consider sealed or opaque
         ;; condition types?
         (let ((c* (simple-conditions exn)))
           (text-color LightRed)
           (println "A unhandled condition was raised:")
           (do ((i 1 (fx+ i 1))
                (c* c* (cdr c*)))
               ((null? c*))
             (let* ((c (car c*))
                    (rtd (record-rtd c)))
               (text-color Default)
               (print " ")
               (let loop ((rtd rtd))
                 (text-attribute 'bold (eq? rtd (record-rtd c)))
                 (print (record-type-name rtd))
                 (text-attribute 'bold #f)
                 (cond ((record-type-parent rtd) =>
                        (lambda (rtd)
                          (unless (eq? rtd (record-type-descriptor &condition))
                            (print " ")
                            (loop rtd))))))
               (let loop ((rtd rtd))
                 (do ((f* (record-type-field-names rtd))
                      (i 0 (fx+ i 1)))
                     ((fx=? i (vector-length f*))
                      (cond ((record-type-parent rtd) => loop)))
                   (println)
                   (print "  ")
                   (text-color Default)
                   (text-attribute 'italic #t)
                   (print (vector-ref f* i))
                   (print ": ")
                   (text-attribute 'italic #f)
                   (let ((x ((record-accessor rtd i) c)))
                     (cond ((and (eq? rtd (record-type-descriptor &irritants))
                                 (pair? x) (list? x))
                            (print "(")
                            (let ((list-x (wherex)))
                              (text-color LightRed)
                              (print/write (car x))
                              (for-each (lambda (value)
                                          (println)
                                          (gotoxy list-x (wherey))
                                          (print/write value))
                                        (cdr x)))
                            (text-color Default)
                            (print ")"))
                           (else
                            (text-color LightRed)
                            (print/write x)))))))
             (println))))
        (else
         (println "A non-condition object was raised:")
         (print/write exn))))

(define (draw-ui)
  (reset-window)
  (text-color Default)
  (text-background Default)
  (clrscr)

  (gotoxy 0 0)
  (text-color Black)
  (text-background Gray)
  (print "Loko Scheme")
  (clreol)

  (gotoxy 0 (- (window-maxy) 1))
  (for-each (lambda (x)
              (text-color Red)
              (print (car x))
              (text-color Black)
              (print " ")
              (print (cdr x))
              (print "  "))
            '(("Alt+X" . "Exit")
              ("Ctrl+L" . "Refresh screen")))
  (clreol)

  (text-color Default)
  (text-background Default)
  (set-window 0 1 (window-maxx) (- (window-maxy) 1))
  (clrscr))

(define (make-console-output-port color)
  (define id "console")
  (define (write! str start count)
    (text-color color)
    (do ((count count (fx- count 1))
         (i 0 (fx+ i 1)))
        ((eqv? count 0))
      (let ((c (string-ref str i)))
        (cond ((eqv? c #\newline)
               (println)
               (update-screen))
              (else
               (print (string c))))))
    count)
  (define get-position #f)
  (define set-position! #f)
  (define (close)
    ;; TODO: This doesn't work if the user takes a reference to the port.
    (update-screen))
  (make-custom-textual-output-port id write! get-position set-position! close))

(define (eval-expr datum env)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (exn)
          ;; TODO: what about warnings?
          ;; TODO: flush output ports?
          (let ((p (current-error-port)))
            (text-color Default)
            (println)
            (println/newlines
             (call-with-string-output-port
               (lambda (p)
                 (stack-trace p))))
            (print-condition exn))
          (k #t))
        (lambda ()
          (call-with-values
            ;; TODO: should redirect input to console and use parameters
            (lambda ()
              (current-output-port (make-console-output-port Default))
              (current-error-port (make-console-output-port LightRed))
              (let-values ((v* (eval datum (env))))
                (flush-output-port (current-output-port))
                (flush-output-port (current-error-port))
                (apply values v*)))
            (case-lambda
              (()
               (text-color DarkGray)
               (print "(No return values)")
               (clreol))
              ((x)
               (cond ((eqv? x (if #f #f))
                      (text-color DarkGray)
                      (print "(void)")
                      (clreol))
                     (else
                      (print (call-with-string-output-port
                               (lambda (p) (write x p))))
                      (clreol))))
              (x*
               (for-each (lambda (x)
                           (print (call-with-string-output-port
                                    (lambda (p) (write x p))))
                           (clreol)
                           (println))
                         x*)))))))))

(define env (make-parameter #f))

(define (tui)
  (define line "")
  (define cur 0)
  (define prev #f)
  (draw-ui)

  ;; TODO: This process should receive messages for windows, wait for
  ;; input and dispatch it to either another process or the window
  ;; handling, and output screen updates. It should be lock up during
  ;; redisplay.

  (unless (interaction-environment)
    (interaction-environment (new-interaction-environment)))
  (parameterize ((env (interaction-environment)))
    (do ((stop #f)) (stop)
      (gotoxy 0 0)
      (text-color LightGreen)
      (print "> ")
      (let ((prompt-width (wherex)))
        (text-color Default)
        (print line)
        (clreol)
        (gotoxy (fx+ cur prompt-width) 0))

      (update-screen)

      (let ((ev (read-event)))
        ;; Debug printing
        (when #f
          (gotoxy 0 (- (window-maxy) 1))
          (cond
            ((key-press-event? ev)
             (print (list 'press (enum-set->list (keyboard-event-mods ev))
                          (keyboard-event-char ev) (keyboard-event-key ev)
                          (keyboard-event-location ev))))
            ((unknown-event? ev)
             (print (list 'unknown (unknown-event-source ev)
                          (if (integer? (unknown-event-data ev))
                              (number->string (unknown-event-data ev) 16)
                              (unknown-event-data ev)))))
            (else
             (print ev)))
          (clreol))

        (gotoxy 0 1)

        (cond
          ((and (key-press-event? ev)
                (enum-set=? (keyboard-event-mods ev) (modifier-set ctrl))
                (eqv? (keyboard-event-key ev) #\l))
           (resize-screen))

          ((resize-event? ev)
           (draw-ui))

          ((or (input-closed-event? ev)
               (and (key-press-event? ev)
                    (enum-set=? (keyboard-event-mods ev) (modifier-set alt))
                    (eqv? (keyboard-event-key ev) #\x)))
           (restore-console)
           (set! stop #t))

          ((and (key-press-event? ev)
                (enum-set=? (keyboard-event-mods ev) (modifier-set ctrl))
                (eqv? (keyboard-event-key ev) #\a))
           (set! cur 0))

          ((and (key-press-event? ev)
                (enum-set=? (keyboard-event-mods ev) (modifier-set ctrl))
                (eqv? (keyboard-event-key ev) #\e))
           (set! cur (string-length line)))

          ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'Backspace)
                (enum-set=? (enum-set-difference (keyboard-event-mods ev)
                                                 (modifier-set shift))
                            (modifier-set)))
           (unless (eqv? cur 0)
             (set! line (string-append (substring line 0 (fx- cur 1))
                                       (substring line cur (string-length line))))
             (set! cur (fx- cur 1))))

          ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'ArrowLeft)
                (not (keyboard-event-has-modifiers? ev)))
           (unless (eqv? cur 0)
             (set! cur (fx- cur 1))))

          ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'ArrowRight)
                (not (keyboard-event-has-modifiers? ev)))
           (unless (eqv? cur (string-length line))
             (set! cur (fx+ cur 1))))

          ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'ArrowUp)
                (not (keyboard-event-has-modifiers? ev)))
           (set! line prev)
           (set! cur (string-length line)))

          ((and (key-press-event? ev) (eq? (keyboard-event-key ev) 'Enter)
                (not (keyboard-event-has-modifiers? ev)))
           (unless (equal? line "")
             (set! prev line)
             (clrscr)
             (call/cc
               (lambda (k)
                 (guard (exn ((lexical-violation? exn)
                              (text-color LightRed)
                              (println "Parse error")
                              (k #t)))
                   (let* ((line-p (open-string-input-port line))
                          (datum (read line-p)))
                     (set! line (if (port-eof? line-p) "" (get-string-all line-p)))
                     (set! cur 0)
                     (eval-expr datum env)))))))

          ((and (key-press-event? ev) (keyboard-event-char ev)) =>
           (lambda (char)
             (set! line (string-append (substring line 0 cur)
                                       (string char)
                                       (substring line cur (string-length line))))
             (set! cur (fx+ cur 1)))))))))

(init-set! 'run-user-interface tui))
