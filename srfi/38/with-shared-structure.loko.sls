;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2019 Göran Weinholt
#!r6rs

;;; SRFI-38 (External Representation for Data With Shared Structure)

(library (srfi :38 with-shared-structure)
  (export
    write-with-shared-structure
    (rename (write-with-shared-structure write/ss)
            (read read-with-shared-structure)
            (read read/ss)))
  (import
    (rnrs (6)))

(define write-with-shared-structure
  (case-lambda
    ((obj)
     (write-with-shared-structure obj (current-output-port)))
    ((obj port)
     (write obj port))
    ((obj port _)
     (write obj port)))))
