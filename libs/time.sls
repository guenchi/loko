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

;;; Primitives needed by SRFI 19

;; SRFI 19 is not provided directly by Loko, but can be found in the
;; chez-srfi package.

(library (loko libs time)
  (export
    cumulative-process-time
    current-time
    time-second
    time-nanosecond
    time-resolution
    time-init-set!)
  (import
    (rnrs (6)))

(define-record-type time
  (sealed #t)
  (fields second nanosecond resolution))

(define *cumulative-process-time-resolution* 1000)
(define *cumulative-process-time* (lambda () (values 0 0)))
(define (cumulative-process-time)
  (let-values ([(s ns) (*cumulative-process-time*)])
    (make-time s ns *cumulative-process-time-resolution*)))

(define *current-time-resolution* 1000)
(define *current-time* (lambda () (values 0 0)))
(define (current-time)
  (let-values ([(s ns) (*current-time*)])
    (make-time s ns *current-time-resolution*)))

(define *nanosleep*
  (lambda _ (error 'nanosleep "No nanosleep procedure installed")))
(define (nanosleep seconds)
  ;; The process sleeps for the given amount. Can be less than a second.
  (*nanosleep* seconds))

(define (time-init-set! what value)
  (case what
    ((cumulative-process-time-resolution
      current-time-resolution)
     (let-values ([(s ns) (value)])
       (let ((res (+ ns (* s (expt 10 9)))))
         (if (eq? what 'current-time-resolution)
             (set! *current-time-resolution* res)
             (set! *cumulative-process-time-resolution* res)))))
    ((cumulative-process-time) (set! *cumulative-process-time* value))
    ((current-time) (set! *current-time* value))
    ((nanosleep) (set! *nanosleep* value))
    (else
     (error 'time-init-set! "Unrecognized key" what value)))))
