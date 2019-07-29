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

;;; Initialization calls and procedures for the execution environment.

;; The linux-init, pc-init and process-init libraries call init-set!.

(library (loko init)
  (export
    command-line
    get-environment-variables get-environment-variable
    exit
    file-exists? delete-file
    open-file-input-port
    open-file-output-port
    open-file-input/output-port
    $mmap
    allocate
    run-user-interface
    machine-type
    init
    init-set!)
  (import
    (except (rnrs) command-line exit
            file-exists? delete-file
            open-file-input-port
            open-file-output-port
            open-file-input/output-port))

(define *command-line* '("loko"))
(define (command-line)
  *command-line*)

;; SRFI-98
(define *environment-variables* '())
(define (get-environment-variables)
  *environment-variables*)
(define (get-environment-variable name)
  (cond ((assoc name *environment-variables*) => cdr)
        (else #f)))

;; Linux auxiliary vector, auxv
(define *auxiliary-vector* '())

(define *exit*
  (lambda status
    (apply error 'exit "No exit procedure has been installed" status)))
(define exit
  (case-lambda
    (()
     (flush-output-port (current-output-port))
     (*exit* #t))
    ((status)
     (flush-output-port (current-output-port))
     (*exit* status))))

(define *open-file-input-port*
  (lambda (filename y z w)
    (raise
      (make-i/o-error)
      (make-who-condition 'open-file-input-port)
      (make-message-condition "No open-file-input-port procedure has been installed")
      (make-irritants-condition filename))))
(define open-file-input-port
  (case-lambda
    ((x)
     (open-file-input-port x (file-options)))
    ((x y)
     (open-file-input-port x y (buffer-mode block)))
    ((x y z)
     (open-file-input-port x y z #f))
    ((x y z w)
     (*open-file-input-port* x y z w))))

(define *open-file-output-port*
  (lambda x
    (apply error 'open-file-output-port
           "No open-file-output-port has been installed" x)))
(define open-file-output-port
  (case-lambda
    ((x)
     (open-file-output-port x (file-options)))
    ((x y)
     (open-file-output-port x y (buffer-mode block)))
    ((x y z)
     (open-file-output-port x y z #f))
    ((x y z w)
     (*open-file-output-port* x y z w))))

(define *open-file-input/output-port*
  (lambda x
    (apply error 'open-file-input/output-port
           "No open-file-input/output-port has been installed" x)))
(define open-file-input/output-port
  (case-lambda
    ((x)
     (open-file-input/output-port x (file-options)))
    ((x y)
     (open-file-input/output-port x y (buffer-mode block)))
    ((x y z)
     (open-file-input/output-port x y z #f))
    ((x y z w)
     (*open-file-input/output-port* x y z w))))

(define *file-exists?*
  (lambda (filename)
    #f))
(define (file-exists? fn)
  (*file-exists?* fn))

(define *delete-file*
  (lambda (filename)
    (raise
      (make-i/o-error)
      (make-who-condition 'delete-file)
      (make-message-condition "No delete-file procedure has been installed")
      (make-irritants-condition filename))))
(define (delete-file fn)
  (*delete-file* fn))

(define *mmap* (lambda _ (error 'mmap "No mmap procedure installed")))
(define ($mmap start len type)
  (*mmap* start len type))

(define *allocate*
  (lambda _ (error 'allocate "No allocate procedure installed")))
(define (allocate type size mask)
  ;; Memory allocation.
  (*allocate* type size mask))

(define *init*
  (lambda _ (error 'start "No start procedure installed")))
(define (init)
  (*init*))

(define *run-user-interface*
  (lambda _ (error 'run-user-interface "There is no user interface installed")))
(define (run-user-interface)
  (*run-user-interface*))

(define *machine-type* '#(unknown unknown))
(define (machine-type)
  *machine-type*)

(define (init-set! what value)
  (case what
    ((command-line) (set! *command-line* value))
    ((environment-variables) (set! *environment-variables* value))
    ((auxiliary-vector) (set! *auxiliary-vector* value))
    (($mmap) (set! *mmap* value))
    ((exit) (set! *exit* value))
    ((file-exists?) (set! *file-exists?* value))
    ((delete-file) (set! *delete-file* value))
    ((open-file-input-port) (set! *open-file-input-port* value))
    ((open-file-output-port) (set! *open-file-output-port* value))
    ((allocate) (set! *allocate* value))
    ((init) (set! *init* value))
    ((run-user-interface) (set! *run-user-interface* value))
    ((machine-type) (set! *machine-type* value))
    (else
     (error 'init-set! "Unrecognized key" what value)))))
