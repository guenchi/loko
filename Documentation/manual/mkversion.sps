#!/usr/bin/env scheme-script
;; -*- coding: utf-8; mode: scheme -*-
;; Copyright © 2019 Göran Weinholt
;; SPDX-License-Identifier: MIT

(import
  (rnrs (6))
  (loko match))

(call-with-input-file "../../Akku.manifest"
  (lambda (p)
    (read p)
    (match (read p)
      [('akku-package (_ version) . _)
       (call-with-output-file "version.texi"
         (lambda (out)
           (display "@set VERSION " out)
           (display version out)
           (newline out)))])))
