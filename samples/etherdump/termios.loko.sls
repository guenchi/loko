;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;;; Stub library

(library (text-mode termios)
  (export
    termios-get-window-size
    termios-raw-mode
    termios-canonical-mode)
  (import
    (rnrs (6)))

(define (termios-get-window-size fd)
  (error 'termios-get-window-size "Not implemented" fd))

(define (termios-raw-mode fd)
  #f)

(define (termios-canonical-mode fd)
  #f))
