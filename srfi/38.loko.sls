;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2019 Göran Weinholt
#!r6rs

;;; SRFI-38 (External Representation for Data With Shared Structure)

(library (srfi :38)
  (export
    write-with-shared-structure write/ss
    read-with-shared-structure read/ss)
  (import
    (srfi :38 with-shared-structure)))
