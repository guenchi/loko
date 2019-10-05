#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Loko Scheme - an R6RS Scheme compiler

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

;;; Program that builds the loko binary

(import
  (rnrs (6))
  (psyntax library-manager)
  (loko config)
  (only (loko compiler cp0) cp0-effort-limit)
  (loko compiler static)

  (only (loko compiler compat) gensym?)
  (prefix (loko arch amd64 pc-asm) pc-asm:)
  (prefix (loko arch amd64 linux-asm) linux-asm:)
  (prefix (loko arch amd64 pc-and-linux-asm) pc-and-linux-asm:)
  )

;; Workaround for the library visiting semantics in R6RS
gensym?
pc-asm:visit
linux-asm:visit
pc-and-linux-asm:visit

;; Amp up the optimizations
(cp0-effort-limit 1000)

;; We don't want to load libraries that aren't in the built-in list.
(library-directories '())

(let ((filename "loko.out"))
  (compile-program filename "loko.sps" '(eval main use-primlocs))
  (display "Build finished and written to ")
  (display filename)
  (newline))
