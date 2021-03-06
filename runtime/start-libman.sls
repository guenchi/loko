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

;;; Initialize the library manager

(library (loko runtime start-libman)
  (export)
  (import
    (rnrs (6))
    (srfi :98 os-environment-variables)
    (only (loko config) config-library-path)
    (only (psyntax library-manager) library-directories library-extensions)
    (only (loko runtime utils) string-split))

;; Read the environment
(library-extensions '(".loko.sls" ".sls"))
(cond
  ((get-environment-variable "LOKO_LIBRARY_PATH") =>
   (lambda (path)
     (library-directories (append (string-split path #\:)
                                  (config-library-path)))))
  (else
   (library-directories (cons "." (config-library-path))))))
