#!/usr/bin/env -S loko --program
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

;;; Test suite for the PS/2 keyboard driver

(import
  (rnrs (6))
  (loko match)
  (loko system fibers)
  (loko drivers ps2 core)
  (loko drivers ps2 keyboard))

(define (xlate-ps/2 bv code-set)
  (define port (make-PS/2-port 'controller 1))
  (define event-ch (make-channel))
  (define command-ch (make-channel))
  (define done (make-cvar))
  (spawn-fiber
   (lambda ()
     (define (callback event)
       (put-message event-ch event))
     (driver-PS/2-kbd-scancodes port code-set command-ch callback)
     (error 'xlate-set-2 "Driver returned" bv)))
  (spawn-fiber
   (lambda ()
     (define p (open-bytevector-input-port bv))
     (let lp ()
       (let ((b (get-u8 p)))
         (cond ((eof-object? b)
                (put-message command-ch 'echo)
                (signal-cvar! done))
               (else
                (put-message (PS/2-port-rx-channel port) b)
                (lp)))))))
  (let lp ((events '()))
    (match (perform-operation
            (choice-operation (wrap-operation (get-operation event-ch)
                                              (lambda (x) (cons 'event x)))
                              (wrap-operation (wait-operation done)
                                              (lambda _ 'done))))
      ['done (reverse events)]
      [('event . event) (lp (cons event events))])))

(define (test-equal expect actual)
  (unless (equal? expect actual)
    (error 'test-equal "Nope, not equal" expect actual)))

;; Escape
(test-equal '(#(make (PS/2 2 #x76) 7 41 #f) #(break (PS/2 2 #x76) 7 41 #f))
            (xlate-ps/2 #vu8(#x76 #xF0 #x76) 2))

(test-equal '(#(make (PS/2 1 1) 7 41 #f) #(break (PS/2 1 1) 7 41 #f))
            (xlate-ps/2 #vu8(#x01 #x81) 1))

;; Pause
(test-equal '(#(make (PS/2 2 #xE11477) 7 72 #f) #(break (PS/2 2 #xE11477) 7 72 #f))
            (xlate-ps/2 #vu8(#xE1 #x14 #x77   #xE1 #xF0 #x14 #xF0 #x77) 2))
(test-equal '(#(make (PS/2 1 #xE11D45) 7 72 #f) #(break (PS/2 1 #xE11D45) 7 72 #f))
            (xlate-ps/2 #vu8(#xE1 #x1D #x45  #xE1 #x9D #xC5) 1))

;; Break
(test-equal '(#(make (PS/2 2 #xE07E) 7 72 #f) #(break (PS/2 2 #xE07E) 7 72 #f))
            (xlate-ps/2 #vu8(#xE0 #x7E   #xE0 #xF0 #x7E) 2))
(test-equal '(#(make (PS/2 2 #xE07E) 7 72 #f) #(break (PS/2 2 #xE07E) 7 72 #f))
            (xlate-ps/2 #vu8(#xE0 #x7E   #xE0 #xF0 #x7E) 2))

;; Print Screen
(test-equal '(#(make (PS/2 2 #xE012) #f #f #f)
              #(make (PS/2 2 #xE07C) 7 70 #f)
              #(break (PS/2 2 #xE07C) 7 70 #f)
              #(break (PS/2 2 #xE012) #f #f #f))
            (xlate-ps/2 #vu8(#xE0 #x12  #xE0 #x7C  #xE0 #xF0 #x7C  #xE0 #xF0 #x12) 2))
(test-equal '(#(make (PS/2 1 #xE02A) #f #f #f)
              #(make (PS/2 1 #xE037) 7 70 #f)
              #(break (PS/2 1 #xE037) 7 70 #f)
              #(break (PS/2 1 #xE02A) #f #f #f))
            (xlate-ps/2 #vu8(#xE0 #x2A  #xE0 #x37  #xE0 #xB7  #xE0 #xAA) 1))

;; Left gui
(test-equal '(#(make (PS/2 2 #xE01F) 7 227 #f)
              #(break (PS/2 2 #xE01F) 7 227 #f))
            (xlate-ps/2 #vu8(#xE0 #x1F #xE0 #xF0 #x1F) 2))

(display "PS/2 keyboard test passed\n")
