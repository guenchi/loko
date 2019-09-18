;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;; Compatibility lib for bootstrapping from Chez Scheme

(library (psyntax compat)
  (export make-parameter parameterize define-record pretty-print
          gensym void eval-core symbol-value set-symbol-value!
          file-options-spec read-library-source-file
          annotation? annotation-expression annotation-stripped
          read-annotated annotation-source annotation-source->condition
          library-version-mismatch-warning
          library-stale-warning
          file-locator-resolution-error
          label-binding set-label-binding! remove-location
          format)
  (import
    (only (loko runtime reader)
          read-annotated annotation? annotation-expression
          annotation-stripped annotation-source
          annotation-source->condition)
    (rnrs)
    (for (only (chezscheme) record-writer type-descriptor) expand)
    (rename (only (chezscheme)
                  define-record
                  eval
                  gensym
                  top-level-value set-top-level-value!
                  make-parameter parameterize
                  pretty-print format
                  void)
            (define-record define-record*)
            (gensym gensym*)
            (eval eval-core)
            (top-level-value symbol-value)
            (set-top-level-value! set-symbol-value!)))

(define *LABELS* (make-eq-hashtable))

(define (label-binding x)
  (hashtable-ref *LABELS* x #f))

(define (set-label-binding! x v)
  (hashtable-set! *LABELS* x v))

(define (remove-location x)
  (hashtable-delete! *LABELS* x))

(define gensym
  (case-lambda
    (()
     (gensym* "g"))
    ((pretty-name)
     (if (symbol? pretty-name)
         (gensym* (symbol->string pretty-name))
         (gensym* pretty-name)))))

(define (read-library-source-file file-name)
  (with-input-from-file file-name
    (lambda ()
      ;; TODO: the reader will one day figure out the filename from
      ;; the port value.
      (read-annotated (current-input-port) file-name))))

(define (library-version-mismatch-warning name depname filename)
  ;;; please override this in your production implementation
  (display "Warning: inconsistent dependencies: ")
  (display name)
  (display depname)
  (display filename))

(define (library-stale-warning name filename)
  (define p (current-error-port))
  (display "WARNING: library " p)
  (display name p)
  (display " is stale; file " p)
  (display filename p)
  (display "will be recompiled from source.\n" p))

(define (file-locator-resolution-error libname failed-list pending)
  ;;; please override this in your production implementation
  (error 'file-location "cannot find library" libname failed-list pending))

(define-syntax define-record
  (syntax-rules ()
    [(_ name (field* ...) printer)
     (begin
       (define-record* name (field* ...))
       (define dummy
         (record-writer (type-descriptor name)
                        (lambda (r p wr)
                          (printer r p)))))]
    [(_ name (field* ...))
     (define-record* name (field* ...))]))

(define (file-options-spec x)
  (error 'file-options-spec "not implemented")))
