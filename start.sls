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

;;; Code to start a Scheme process

(library (loko start)
  (export)
  (import
    (only (rnrs (6)) call/cc lambda exit)
    (only (loko libs fibers) run-fibers)
    (only (loko init) init))

;; Call the init code from pc-init, linux-init or process-init.
(init)

;; Ensure that everything from here on (the rest of the libraries and
;; the top-level) runs with a fiber scheduler.
(call/cc
  (lambda (k)
    (run-fibers k)
    (exit 0))))
