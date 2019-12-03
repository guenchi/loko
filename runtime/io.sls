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

;;; Scheme I/O ports

;; get-datum and read are defined in reader.scm

;; Note: There are tricky dependencies at startup related to ports.

;; Ports have a flag field which is a bit-field:
;;  bit 0: is an output port
;;  bit 1: is an input port
;;  bit 2: is a textual port

;; The port-buffer field is set to #f when the port is closed. Even
;; ports with buffer-mode none have a port-buffer.

;; Buffering combinations:
;; input/none: read one byte at a time
;; input/block: try to fill the buffer
;; input/line: same as block. tty line discipline provides line buffering.
;; output/none: pass each u8 or bytevector in one go. put-char puts whole char.
;; output/block: try to fill and send the whole buffer
;; output/line: try to fill the buffer until there's a linefeed

;; On transcoded input: look for all supported line endings and
;; convert them to linefeeds. On transcoded output: convert #\linefeed
;; to the port's line ending.

;; Ideas: the buffer size might expand or contract based on how often
;; the sink is flushed, and maybe depending on available memory. The
;; way output buffering is implemented here is probably very naïve.

(library (loko runtime io)
  (export
    buffer-mode? latin-1-codec utf-8-codec utf-16-codec
    native-eol-style

    make-transcoder native-transcoder
    transcoder-codec transcoder-eol-style
    transcoder-error-handling-mode
    bytevector->string string->bytevector

    eof-object eof-object?

    port? port-transcoder textual-port? binary-port?
    transcoded-port port-has-port-position?
    port-position port-has-set-port-position!?
    set-port-position! close-port
    call-with-port

    input-port? port-eof? open-bytevector-input-port
    open-string-input-port standard-input-port current-input-port
    make-custom-binary-input-port make-custom-textual-input-port
    get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
    get-bytevector-some get-bytevector-all
    get-char lookahead-char get-string-n get-string-n! get-string-all
    get-line
    output-port? flush-output-port output-port-buffer-mode
    open-bytevector-output-port
    call-with-bytevector-output-port open-string-output-port
    call-with-string-output-port standard-output-port
    standard-error-port current-output-port current-error-port
    make-custom-binary-output-port make-custom-textual-output-port
    put-u8 put-bytevector put-char put-string put-datum
    ;; make-custom-binary-input/output-port
    ;; make-custom-textual-input/output-port
    call-with-input-file call-with-output-file with-input-from-file
    with-output-to-file open-input-file open-output-file
    close-input-port close-output-port read-char peek-char
    write-char newline display write

    make-file-options             ;for psyntax/expander
    $port-buffer-mode-set!        ;for various open-file-stuff
    $init-standard-ports
    open-output-string
    get-output-string
    port-file-descriptor
    port-file-descriptor-set!)
  (import
    (except (rnrs)
            buffer-mode? latin-1-codec utf-8-codec utf-16-codec
            native-eol-style
            make-transcoder native-transcoder
            transcoder-codec transcoder-eol-style
            transcoder-error-handling-mode
            bytevector->string string->bytevector
            eof-object eof-object?
            port? port-transcoder textual-port? binary-port?
            transcoded-port port-has-port-position?
            port-position port-has-set-port-position!?
            set-port-position! close-port
            call-with-port
            input-port? port-eof? open-bytevector-input-port
            open-string-input-port standard-input-port current-input-port
            make-custom-binary-input-port make-custom-textual-input-port
            get-u8 lookahead-u8 get-bytevector-n get-bytevector-n!
            get-bytevector-some get-bytevector-all
            get-char lookahead-char get-string-n get-string-n! get-string-all
            get-line get-datum
            output-port? flush-output-port output-port-buffer-mode
            open-bytevector-output-port
            call-with-bytevector-output-port open-string-output-port
            call-with-string-output-port standard-output-port
            standard-error-port current-output-port current-error-port
            make-custom-binary-output-port make-custom-textual-output-port
            put-u8 put-bytevector put-char put-string put-datum
            make-custom-binary-input/output-port
            make-custom-textual-input/output-port
            call-with-input-file call-with-output-file with-input-from-file
            with-output-to-file open-input-file open-output-file
            close-input-port close-output-port read-char peek-char read
            write-char newline display write)
    (only (rnrs mutable-strings) string-set!)
    (prefix (rnrs io ports) sys:)
    (only (loko runtime arithmetic) $display-number)
    (only (loko runtime symbols) $gensym-generate-names!)
    (only (loko runtime records) record-writer)
    (loko system $primitives))

;; Tracing this only works when $debug-put-u8 is used directly.
(define-syntax trace
  (syntax-rules ()
    #;
    ((_ . args)
     (begin
       (for-each display (list . args))
       (newline)))
    ((_ . args) 'dummy)))

;; The `file-options' macro residualizes a call to make-file-options
(define file-options-set (make-enumeration '(no-create no-fail no-truncate)))
(define make-file-options (enum-set-constructor file-options-set))

(define (buffer-mode? obj)
  (and (memq obj '(none line block)) #t))

(define (latin-1-codec) 'latin-1-codec)

(define (utf-8-codec) 'utf-8-codec)

(define (utf-16-codec) 'utf-16-codec)

(define (native-eol-style) 'lf)

(define-record-type transcoder
  (fields codec eol-style error-handling-mode)
  (sealed #t) (opaque #t) #;(nongenerative)
  (protocol
   (lambda (p)
     (define make-transcoder
       (case-lambda
         ((c) (make-transcoder c (native-eol-style) (error-handling-mode replace)))
         ((c e) (make-transcoder c e (error-handling-mode replace)))
         ((c e h)
          ;; codec eol-style handling-mode
          (assert (memq c '(utf-8-codec latin-1-codec utf-16-codec)))
          (assert (memq e '(lf crlf none cr nel crnel ls)))
          (assert (memq h '(replace ignore raise)))
          (p c e h))))
     make-transcoder)))

(define %native-transcoder
  (make-transcoder (utf-8-codec)
                   (native-eol-style)
                   (error-handling-mode replace)))

(define (native-transcoder)
  %native-transcoder)

(define (bytevector->string bytevector transcoder)
  (get-string-all (open-bytevector-input-port bytevector transcoder)))

(define (string->bytevector string transcoder)
  (let-values ([(p extract) (open-bytevector-output-port transcoder)])
    (put-string p string)
    (flush-output-port p)
    (extract)))

(define (eof-object) (sys:eof-object))

(define (eof-object? x) (sys:eof-object? x))

;; XXX: Needs to be reworked
(define-record-type port
  (fields id transcoder
          ;; Flags bits:
          ;; 0     1 = output port
          ;; 1     1 = input port (combined ports exist)
          ;; 2     1 = textual, 0 = binary
          ;; 3     .-
          ;; 4     |  eol style (0-7)
          ;; 5     `-
          (mutable flags)
          (mutable sink) (mutable source)
          (mutable get-positioner)
          (mutable set-positioner)
          (mutable closer)
          (mutable extractor)
          (mutable file-descriptor)
          ;; maybe needs a string-buffer, too
          (mutable buffer)
          (mutable buffer-r)
          (mutable buffer-w)
          (mutable buffer-pos)        ;position before last read
          (mutable buffer-char)
          (mutable buffer-mode))       ;XXX: can be in flags
  (sealed #t) (opaque #t) #;(nongenerative))

(define ($port-buffer-mode-set! port mode)
  ;; It's the caller's responsibility to check that the mode is a
  ;; valid buffer-mode. The buffer mode can also only be changed
  ;; once, right after the port has been created.
  (port-buffer-mode-set! port mode)
  (case mode
    ((none)
     ;; Make the buffer only hold one value. This way there's no
     ;; need to check if the buffer mode is none every time the port
     ;; is used.
     (if (string? (port-buffer port))
         (port-buffer-set! port (make-string 1 0))
         (port-buffer-set! port (make-bytevector 1 0))))))

(define (textual-port? p)
  (and (port? p) (eqv? (fxand (port-flags p) #b100) #b100)))

(define (binary-port? p)
  (and (port? p) (eqv? (fxand (port-flags p) #b100) 0)))

(define (port-eol-style p)
  (vector-ref '#(none lf crlf cr nel crnel ls #f)
              (fxbit-field (port-flags p) 3 5)))

(define (transcoded-port p tc)
  (unless (and (binary-port? p) (port-buffer p))
    (assertion-violation 'transcoded-port
                         "Expected an open binary port as the first argument" p tc))
  (unless (transcoder? tc)
    (assertion-violation 'transcoded-port
                         "Expected a transcoder as the second argument" p tc))
  (let* ((eol-flags (case (transcoder-eol-style tc)
                      ((none) 0)      ;XXX: needed or not?
                      ((lf) 1)
                      ((crlf) 2)
                      ((cr) 3)
                      ((nel) 4)
                      ((crnel) 5)
                      ((ls) 6)
                      (else 0)))
         (ret (make-port (port-id p) tc (fxior #b100
                                               (fxarithmetic-shift-left eol-flags 3)
                                               (port-flags p))
                         (port-sink p) (port-source p)
                         (port-get-positioner p)
                         (port-set-positioner p)
                         (port-closer p)
                         #f
                         (port-file-descriptor p)
                         (port-buffer p)
                         (port-buffer-r p)
                         (port-buffer-w p)
                         (port-buffer-pos p)
                         (port-buffer-char p)
                         (port-buffer-mode p))))
    (port-sink-set! p #f)
    (port-source-set! p #f)
    (port-get-positioner-set! p #f)
    (port-set-positioner-set! p #f)
    (port-closer-set! p #f)
    (port-extractor-set! p #f)
    (port-file-descriptor-set! p #f)
    (port-buffer-set! p #f)
    (port-buffer-r-set! p 0)
    (port-buffer-w-set! p 0)
    ret))

(define (port-has-port-position? p)
  (and (port-get-positioner p) #t))

(define (port-position p)
  ;; TODO: and input-output-ports?
  (if (port-has-port-position? p)
      (if (input-port? p)
          (+ (port-buffer-r p) (port-buffer-pos p))
          (+ (port-buffer-w p) (port-buffer-pos p)))
      (assertion-violation 'port-position
                           "This port does not support port-position" p)))

(define (port-has-set-port-position!? p)
  (and (port-set-positioner p) #t))

(define (set-port-position! p pos)
  (assert (fx>=? pos 0))
  (when (output-port? p)
    (flush-output-port p))
  (cond ((port-has-set-port-position!? p)
         ((port-set-positioner p) pos)
         (port-buffer-pos-set! p pos)
         (port-buffer-r-set! p 0)
         (port-buffer-w-set! p 0))
        (else
         (assertion-violation 'set-port-position!
                              "This port does not support set-port-position!" p))))

(define (close-port p)
  ;; TODO: it would be swell if ports with no references to them
  ;; also could close the affected sink/source (which might be a
  ;; file descriptor). The question is if the buffer should be
  ;; flushed in those cases. What language is it that flushes all
  ;; output ports on exit?
  (assert (port? p))
  (when (port-buffer p)
    (when (output-port? p)
      (flush-output-port p))
    (port-sink-set! p #f)
    (port-source-set! p #f)
    (port-get-positioner-set! p #f)
    (port-set-positioner-set! p #f)
    (let ((closer (port-closer p)))
      (port-closer-set! p #f)
      (port-buffer-set! p #f)
      (port-buffer-r-set! p 0)
      (port-buffer-w-set! p 0)
      (when closer
        (closer))))
  (if #f #f))

(define (call-with-port port proc)
  (assert (port? port))
  (let-values ((v (proc port)))
    (close-port port)
    (apply values v)))

;;; Input ports

(define (input-port? p)
  (and (port? p) (fx=? #b10 (fxand (port-flags p) #b10))))

(define (port-eof? p)
  (assert (input-port? p))
  (if (textual-port? p)
      (eof-object? (lookahead-char p))
      (eof-object? (lookahead-u8 p))))

;; open-file-input-port is defined elsewhere.

(define open-bytevector-input-port
  (case-lambda
    ((bv)
     (open-bytevector-input-port bv #f))
    ((bv tc)
     (define pos 0)
     (define (read! buf start count)
       (let ((bv bv))
         (let ((n (fxmin (fx- (bytevector-length bv) pos)
                         count)))
           (bytevector-copy! bv pos buf start n)
           (set! pos (fx+ n pos))
           n)))
     (define (get-position) pos)
     (define (set-position! new-pos) (set! pos new-pos))
     (define (close)
       (set! bv 'closed))
     (assert (bytevector? bv))
     (let ((p (make-custom-binary-input-port
               "*bytevector*" read! get-position set-position! close)))
       (if tc (transcoded-port p tc) p)))))

(define (open-string-input-port str)
  (define pos 0)
  (define (read! buf start count)
    (let* ((str str)
           (n (fxmin (fx- (string-length str) pos)
                     count)))
      (do ((end (fx+ start n))
           (i start (fx+ i 1))
           (j pos (fx+ j 1)))
          ((fx=? i end)
           (set! pos (fx+ n pos))
           n)
        (string-set! buf i (string-ref str j)))))
  (define (get-position) pos)
  (define (set-position! new-pos) (set! pos new-pos))
  (define (close)
    (set! str 'closed))
  (assert (string? str))
  (make-custom-textual-input-port "*string*" read! get-position set-position! close))

(define (make-custom-binary-input-port id read! get-position set-position! close)
  (assert (string? id))
  (make-port (string-copy id) #f #b10
             #f read! get-position set-position! close #f #f
             (make-bytevector 4096) 0 0 0 #f
             'block))

(define (make-custom-textual-input-port id read! get-position set-position! close)
  (assert (string? id))
  (make-port (string-copy id) #f #b110
             #f read! get-position set-position! close #f #f
             (make-string 512) 0 0 0 #f
             'block))

;;; Binary input

(define get-u8
  (case-lambda
    (() (sys:get-u8 (current-input-port)))
    ((p)
     (let ((x (sys:lookahead-u8 p)))
       (cond ((sys:eof-object? x)
              (sys:eof-object))
             (else
              (port-buffer-r-set! p (fx+ (port-buffer-r p) 1))
              x))))))

(define lookahead-u8
  (case-lambda
    (() (sys:lookahead-u8 (current-input-port)))
    ((p)
     (assert (input-port? p))
     (let ((b (port-buffer p))
           (r (port-buffer-r p))
           (w (port-buffer-w p)))
       (cond ((fx=? r w)
              ;; Buffer empty, time to call the byte source
              (unless (bytevector? b)
                (assertion-violation 'lookahead-u8 "The port is closed" p))
              (let* ((pos (cond ((port-get-positioner p) => (lambda (x) (x)))
                                (else #f)))
                     (source (port-source p))
                     (req (bytevector-length b))
                     (bytes (source b 0 req)))
                (assert (fx<=? 0 bytes req))
                (cond ((eqv? bytes 0)
                       (eof-object))
                      (else
                       (port-buffer-pos-set! p pos)
                       (port-buffer-r-set! p 0)
                       (port-buffer-w-set! p bytes)
                       (bytevector-u8-ref b 0)))))
             (else
              (bytevector-u8-ref b r)))))))

(define (get-bytevector-n port n)
  ;; TODO: first try to empty the buffer, then use the source
  ;; directly.
  (unless (fx>=? n 0)
    (assertion-violation 'get-bytevector-n
                         "Expected a non-negative count" port n))
  (let ((buf (make-bytevector n)))
    (let lp ((i 0))
      (if (fx=? n i)
          buf
          (let ((b (sys:get-u8 port)))
            (cond
              ((sys:eof-object? b)
               (if (eqv? i 0)
                   (eof-object)
                   (let ((ret (make-bytevector i)))
                     (bytevector-copy! buf 0 ret 0 i)
                     ret)))
              (else
               (bytevector-u8-set! buf i b)
               (lp (fx+ i 1)))))))))

(define (get-bytevector-n! port buf start count)
  (unless (and (fixnum? start) (not (fxnegative? start))
               (fixnum? count) (not (fxnegative? count)))
    (assertion-violation 'get-bytevector-n!
                         "Expected an exact, non-negative start and count"
                         port buf start count))
  (let ((end (fx+ start count)))
    (unless (fx>=? (bytevector-length buf) end)
      (assertion-violation 'get-bytevector-n!
                           "Expected a bytevector that is at least start+count long"
                           port buf start count))
    (let lp ((i start))
      (if (fx=? i end)
          count
          (let ((b (get-u8 port)))
            (cond
              ((eof-object? b)
               (let ((consumed (fx- i start)))
                 (if (eqv? consumed 0)
                     (eof-object)
                     consumed)))
              (else
               (bytevector-u8-set! buf i b)
               (lp (fx+ i 1)))))))))

(define (get-bytevector-some p)
  (if (port-eof? p)                   ;XXX: Hmm.
      (eof-object)
      (call-with-bytevector-output-port
        (lambda (out)
          (let ((b (port-buffer p))
                (r (port-buffer-r p))
                (w (port-buffer-w p)))
            (cond ((fx=? r w)
                   ;; Buffer empty, time to call the byte source
                   (unless (bytevector? b)
                     (assertion-violation 'get-bytevector-some
                                          "The port is closed" p))
                   (let* ((pos (cond ((port-get-positioner p) =>
                                      (lambda (x) (x)))
                                     (else #f)))
                          (source (port-source p))
                          (req (bytevector-length b))
                          (bytes (source b 0 req)))
                     (assert (fx<=? 0 bytes req))
                     (cond ((eqv? bytes 0)
                            (error 'get-bytevector-some
                                   "The port was unexpectedly closed" p))
                           (else
                            (port-buffer-pos-set! p pos)
                            (port-buffer-r-set! p 0)
                            (port-buffer-w-set! p bytes)
                            (bytevector-u8-ref b 0)
                            ;; TODO: Here it would be great to just
                            ;; shorten the length of the buffer and
                            ;; return it immediately.
                            (do ((r 0 (fx+ r 1)))
                                ((fx=? r w)
                                 (port-buffer-r-set! p w))
                              (put-u8 out r))))))
                  (else
                   ;; Return everything we have buffered.
                   (do ((r r (fx+ r 1)))
                       ((fx=? r w)
                        (port-buffer-r-set! p w))
                     (put-u8 out (bytevector-u8-ref b r))))))))))

(define (get-bytevector-all ip)
  (let ((datum (get-bytevector-n ip 4096)))
    (if (eof-object? datum)
        datum
        (call-with-bytevector-output-port
          (lambda (op)
            (let lp ((datum datum))
              (put-bytevector op datum)
              (let ((datum (get-bytevector-n ip 4096)))
                (unless (eof-object? datum)
                  (lp datum)))))))))

;;; Textual input

(define (get-char p)
  (assert (and (input-port? p) (textual-port? p)))
  (let ((c (lookahead-char p)))
    (port-buffer-char-set! p #f)
    c))

(define (%read-utf8 n i lower-limit p)
  (let lp ((n n) (i i))
    (if (eqv? i 0)
        (if (or (fx<? n lower-limit)
                (fx<=? #xD800 n #xDFFF)
                (fx>? n #x10FFFF))
            #\xFFFD
            (integer->char n))
        (let ((b (lookahead-u8 p)))
          (cond ((and (fixnum? b)
                      (eqv? (fxand b #b11000000) #b10000000))
                 ;; Found a valid UTF-8 byte
                 (get-u8 p)
                 (lp (fxior (fxarithmetic-shift-left n 6)
                            (fxand b #b111111))
                     (fx- i 1)))
                (else
                 #\xFFFD))))))

(define (lookahead-char p)
  (assert (and (input-port? p) (textual-port? p)))
  (cond ((port-buffer-char p))
        ((port-transcoder p)
         ;; TODO: parse different linefeeds
         (case (transcoder-codec (port-transcoder p))
           ((utf-8-codec)
            ;; TODO: different error-handling-modes
            (let ((b (get-u8 p)))
              (if (eof-object? b)
                  (eof-object)
                  (let* ((c (cond
                              ((fx<? b #x80)
                               (integer->char b))
                              ((eqv? (fxand b #b11100000) #b11000000)
                               (%read-utf8 (fxand b #b00011111) 1 #x80 p))
                              ((eqv? (fxand b #b11110000) #b11100000)
                               (%read-utf8 (fxand b #b00001111) 2 #x800 p))
                              ((eqv? (fxand b #b11111000) #b11110000)
                               (%read-utf8 (fxand b #b00000111) 3 #x10000 p))
                              (else
                               #\xFFFD)))
                         ;; FIXME
                         (c (if (eqv? c #\return)
                                #\linefeed
                                c)))
                    (port-buffer-char-set! p c)
                    c))))
           ((latin-1-codec)
            (let ((u8 (get-u8 p)))
              (if (eof-object? u8)
                  (eof-object)
                  (let ((c (integer->char u8)))
                    (port-buffer-char-set! p c)
                    c))))
           (else
            (error 'lookahead-char "Unimplemented input codec"
                   (transcoder-codec (port-transcoder p))))))
        ((string? (port-buffer p))
         (let ((b (port-buffer p))
               (r (port-buffer-r p))
               (w (port-buffer-w p)))
           ;; XXX: duplicated in lookahead-u8
           (cond ((fx=? r w)
                  (let* ((pos (cond ((port-get-positioner p) => (lambda (x) (x)))
                                    (else #f)))
                         (source (port-source p))
                         (req (string-length b))
                         (chars (source b 0 req)))
                    (assert (fx<=? 0 chars req))
                    (cond ((eqv? chars 0)
                           (eof-object))
                          (else
                           (port-buffer-pos-set! p pos)
                           (port-buffer-r-set! p 1)
                           (port-buffer-w-set! p chars)
                           (let* ((c (string-ref b 0))
                                  ;; FIXME
                                  (c (if (eqv? c #\return) #\linefeed c)))
                             (port-buffer-char-set! p c)
                             c)))))
                 (else
                  (let* ((c (string-ref b r))
                         ;; FIXME
                         (c (if (eqv? c #\return) #\linefeed c)))
                    (port-buffer-r-set! p (fx+ r 1))
                    (port-buffer-char-set! p c)
                    c)))))
        (else
         ;; XXX: can report bad &who
         (assertion-violation 'lookahead-char "The port is closed" p))))

(define (get-string-n port n)
  (let ((buf (make-string n)))
    (let lp ((i 0))
      (if (fx=? n i)
          buf
          (let ((c (get-char port)))
            (cond
              ((eof-object? c)
               (if (eqv? i 0)
                   (eof-object)
                   (substring buf 0 i)))
              (else
               (string-set! buf i c)
               (lp (fx+ i 1)))))))))

(define (get-string-n! port buf start n)
  (unless (and (fixnum? start) (not (fxnegative? start))
               (fixnum? n) (not (fxnegative? n)))
    (assertion-violation 'get-string-n!
                         "Expected an exact, non-negative start and count"
                         port buf start n))
  (let ((end (fx+ start n)))
    (unless (fx>=? (string-length buf) end)
      (assertion-violation 'get-string-n!
                           "Expected a string that is at least start+count long"
                           port buf start n))
    (let lp ((i start) (read 0) (c (get-char port)))
      (cond ((fx=? i end)
             read)
            ((eof-object? c)
             (if (eqv? read 0)
                 (eof-object)
                 read))
            (else
             (string-set! buf i c)
             (lp (fx+ i 1) (fx+ read 1) (get-char port)))))))

(define (get-string-all p)
  (if (port-eof? p)
      (eof-object)
      (call-with-string-output-port
        (lambda (o)
          (let lp ()
            (let ((c (get-char p)))
              (unless (eof-object? c)
                (put-char o c)
                (lp))))))))

(define (get-line p)
  (if (port-eof? p)
      (eof-object)
      (call-with-string-output-port
        (lambda (o)
          (let lp ()
            (let ((c (get-char p)))
              (unless (or (eof-object? c)
                          (eqv? c #\linefeed))
                (put-char o c)
                (lp))))))))

;;; Output ports

(define (output-port? p)
  (and (port? p) (eqv? #b01 (fxand (port-flags p) #b01))))

(define (%sync-position p)
  (cond ((port-get-positioner p) =>
         (lambda (get-pos)
           (port-buffer-pos-set! p (get-pos))))
        (else (if #f #f))))

(define (flush-output-port p)
  (assert (output-port? p))
  (trace "flush-output-port r=" (port-buffer-r p) " w=" (port-buffer-w p))
  (let ((sink (port-sink p))
        (buffer (port-buffer p)))
    (let lp ((r (port-buffer-r p))
             (k (fx- (port-buffer-w p)
                     (port-buffer-r p))))
      (when (fxpositive? k)
        (let ((bytes (sink buffer r k)))
          (assert (fx>? bytes 0))
          (let ((r* (fx+ r bytes))
                (k* (fx- k bytes)))
            (port-buffer-r-set! p r*)
            (lp r* k*))))))
  (port-buffer-r-set! p 0)
  (port-buffer-w-set! p 0)
  (%sync-position p))

(define (output-port-buffer-mode p)
  (assert (output-port? p))
  (port-buffer-mode p))

;; open-file-output-port is defined elsewhere.

(define open-bytevector-output-port
  (case-lambda
    ((transcoder)
     (define buf #vu8())
     (define pos 0)
     (define (grow! minimum)
       (when (fx<? (bytevector-length buf) minimum)
         (let ((new (make-bytevector (max minimum (fx* (bytevector-length buf) 2)))))
           (bytevector-copy! buf 0 new 0 (bytevector-length buf))
           (set! buf new))))
     (define (bytevector-extractor)
       ;; TODO: the buffer could actually be shrunk by the GC
       (trace "extract flushes: " buf " " pos)
       (flush-output-port port)
       (trace "extract flushed: " buf " " pos)
       (let ((buffer buf)
             (position pos))
         (set! buf #vu8())
         (set! pos 0)
         (%sync-position port)
         (trace "extracting " position " bytes from " buffer)
         (if (fx=? (bytevector-length buffer) position)
             buffer
             (let ((ret (make-bytevector position)))
               (bytevector-copy! buffer 0 ret 0 position)
               ret))))
     (define (write! bv start count)
       (trace "-write! bv=" bv " start=" start " count=" count " buf=" buf)
       (grow! (fx+ pos count))
       (bytevector-copy! bv start buf pos count)
       (set! pos (+ pos count))
       (trace "+write! bv=" bv " start=" start " count=" count " buf=" buf)
       count)
     (define (get-position)
       pos)
     (define (set-position! pos)
       (error 'set-position! "TODO: bytevector output ports" pos))
     (define (close)
       (set! buf #vu8())
       (set! pos 0))
     (define port
       ;; TODO: instead of *bytevector* name it after the caller
       (let ((p (make-custom-binary-output-port "*bytevector*"
                                                write! get-position
                                                set-position! close)))
         (if transcoder
             (transcoded-port p transcoder)
             p)))
     (values port bytevector-extractor))
    (()
     (open-bytevector-output-port #f))))

(define call-with-bytevector-output-port
  (case-lambda
    ((proc)
     (call-with-bytevector-output-port proc #f))
    ((proc transcoder)
     (let-values (((port extractor) (open-bytevector-output-port transcoder)))
       (proc port)
       (let ((ret (extractor)))
         (close-port port)
         ret)))))

(define (open-string-output-port)
  (define buf "")
  (define pos 0)
  (define (grow! minimum)
    (when (fx<? (string-length buf) minimum)
      (do ((new (make-string (max minimum (fx* (string-length buf) 2))))
           (i 0 (fx+ i 1)))
          ((fx=? i (string-length buf))
           (set! buf new))
        (string-set! new i (string-ref buf i)))))
  (define (string-extractor)
    ;; TODO: the buffer could actually be shrunk by the GC
    (flush-output-port port)
    (let ((buffer buf)
          (position pos))
      (set! buf "")
      (set! pos 0)
      (trace "extracting " position " chars from '" buffer "'")
      (if (fx=? (string-length buffer) position)
          buffer
          (do ((ret (make-string position))
               (i 0 (fx+ i 1)))
              ((fx=? i position)
               ret)
            (string-set! ret i (string-ref buffer i))))))
  (define (write! str start count)
    (trace "-write! string=" str " start=" start " count=" count " buf='" buf "'")
    (grow! (fx+ pos count))
    (do ((i 0 (fx+ i 1)))
        ((fx=? i count))
      (string-set! buf (fx+ pos i) (string-ref str (fx+ start i))))
    (set! pos (+ pos count))
    (trace "+write! string=" str " start=" start " count=" count " buf='" buf "'")
    count)
  (define (close)
    (set! buf "")
    (set! pos 0))
  (define port
    (make-custom-textual-output-port "*string*" write! #f #f close))
  (port-extractor-set! port string-extractor)
  ;; TODO: port-position
  (values port string-extractor))

(define (call-with-string-output-port proc)
  (let-values (((port extractor) (open-string-output-port)))
    (proc port)
    (let ((ret (extractor)))
      (close-port port)
      ret)))

(define (make-custom-binary-output-port id write! get-position set-position! close)
  (assert (string? id))
  (make-port (string-copy id) #f #b01
             write! #f get-position set-position! close #f #f
             (make-bytevector 4096) 0 0 0 #f
             'block))

(define (make-custom-textual-output-port id write! get-position set-position! close)
  (assert (string? id))
  (make-port (string-copy id) #f #b101
             write! #f get-position set-position! close #f #f
             (make-string 4096) 0 0 0 #f
             'block))

;;; SRFI 6-style string ports

(define (open-output-string)
  (let-values ([(port extractor) (open-string-output-port)])
    port))

(define (get-output-string port)
  (let ((extractor (port-extractor port)))
    (unless extractor
      (assertion-violation 'get-output-string
                           "Expected a string output port" port))
    (extractor)))

;;; Binary output

(define (put-u8 p u8)
  (cond ((output-port? p)
         (let ((b (port-buffer p))
               (w (port-buffer-w p)))
           ;; There is always room for at least one byte.
           (unless (bytevector? b)
             (error 'put-u8 "The port is closed" p))
           (bytevector-u8-set! b w u8)
           (let ((w* (fx+ w 1)))
             (trace "put-u8 " u8 " b=" b " r=" r " w=" w " w*=" w*)
             (port-buffer-w-set! p w*)
             (when (fx=? w* (bytevector-length b))
               (flush-output-port p)))))
        (else
         (error 'put-u8 "Not an output port" p u8))))

(define put-bytevector
  (case-lambda
    ((p bv)
     (put-bytevector p bv 0 (bytevector-length bv)))
    ((p bv start)
     (put-bytevector p bv start (fx- (bytevector-length bv) start)))
    ((p bv start count)
     (let ((end (+ start count))
           (buf (port-buffer p))
           (mode (port-buffer-mode p)))
       (trace "put-bytevector " p " bv=" bv " start=" start " count=" count
              " end=" end " mode=" mode)
       (assert (fx<=? 0 start end (bytevector-length bv)))
       ;; XXX: a better strategy here, when the bytevector would fit
       ;; in the buffer but currently doesn't, would be to put as
       ;; much of the bytevector as possible into the buffer, flush,
       ;; and then put in the rest.
       (cond ((and (output-port? p)
                   (or (eq? mode 'none)
                       (and (eq? mode 'block)
                            (fx>? count (bytevector-length buf)))))
              ;; Bypass the buffering if the bytevector if larger
              ;; than the port's buffer. Send it to the sink.
              (trace "put-bytevector strategy: bypass")
              (flush-output-port p)
              (let ((sink (port-sink p)))
                (let lp ((r start)
                         (k count))
                  (when (fxpositive? k)
                    (let ((bytes (sink bv r k)))
                      (assert (fx>? bytes 0))
                      (let ((r* (fx+ r bytes))
                            (k* (fx- k bytes)))
                        (lp r* k*))))))
              (%sync-position p))
             ((and (output-port? p)
                   (eq? mode 'block)
                   (fx<? count (fx- (bytevector-length buf)
                                    (port-buffer-w p))))
              (trace "put-bytevector strategy: copy!")
              (bytevector-copy! bv start
                                buf (port-buffer-w p)
                                count)
              (let ((w* (fx+ (port-buffer-w p) count)))
                (port-buffer-w-set! p w*)
                (when (fx=? w* (bytevector-length buf))
                  (flush-output-port p))))
             (else
              ;; This is really because of the $debug-put-u8 thing.
              (trace "put-bytevector strategy: one-by-one")
              (do ((i start (fx+ i 1)))
                  ((fx=? i end))
                (put-u8 p (bytevector-u8-ref bv i)))))))))

;;; Textual output

(define (%put-utf8 p c)
  (define length-code #vu8(#x00 #x00 #xC0 #xE0 #xF0))
  (let ((i (char->integer c)))
    (define-syntax put              ;force inlining
      (lambda (x)
        (syntax-case x ()
          ((put len)
           #'(when (fx>=? len 1)
               (put-u8 p (fxior (fxarithmetic-shift-right i (fx* 6 (fx- len 1)))
                                (bytevector-u8-ref length-code len)))
               (when (fx>=? len 2)
                 (put-u8 p (fxand (fxior (fxarithmetic-shift-right i (fx* 6 (fx- len 2)))
                                         #x80)
                                  #xBF))
                 (when (fx>=? len 3)
                   (put-u8 p (fxand (fxior (fxarithmetic-shift-right i (fx* 6 (fx- len 3)))
                                           #x80)
                                    #xBF))
                   (when (fx>=? len 4)
                     (put-u8 p (fxand (fxior i #x80) #xBF))))))))))
    (cond ((fx<? i #x80) (put-u8 p i))
          ((fx<? i #x800) (put 2))
          ((fx<? i #x10000) (put 3))
          (else (put 4)))))

(define (put-char p c)
  ;; XXX: This should likely be reworked to do the UTF-8 encoding
  ;; later, so that it can be inlined.
  (assert (output-port? p))
  (unless (char? c)
    (assertion-violation 'put-char "Expected a character" p c))
  (cond ((port-transcoder p)
         (case (transcoder-codec (port-transcoder p))
           ((utf-8-codec)
            ;; XXX: output different linefeeds
            (cond ((eqv? c #\linefeed)
                   (case (port-eol-style p)
                     ((lf none)
                      (%put-utf8 p #\linefeed))
                     ((crlf)
                      (%put-utf8 p #\return)
                      (%put-utf8 p #\linefeed))
                     ((cr)
                      (%put-utf8 p #\return))
                     ((nel)
                      (%put-utf8 p #\x0085))
                     ((crnel)
                      (%put-utf8 p #\return)
                      (%put-utf8 p #\x0085))
                     ((ls)
                      (%put-utf8 p #\x2028))
                     (else (if #f #f)))
                   (when (eq? (port-buffer-mode p) 'line)
                     (flush-output-port p)))
                  (else
                   (%put-utf8 p c))))
           ((latin-1-codec)
            (cond ((eqv? c #\linefeed)
                   (case (port-eol-style p)
                     ((lf none)
                      (put-u8 p (char->integer #\linefeed)))
                     ((crlf)
                      (put-u8 p (char->integer #\return))
                      (put-u8 p (char->integer #\linefeed)))
                     ((cr)
                      (put-u8 p (char->integer #\return)))
                     ((nel)
                      (put-u8 p (char->integer #\x0085)))
                     ((crnel)
                      (put-u8 p (char->integer #\return))
                      (put-u8 p (char->integer #\x0085)))
                     ((ls)
                      (put-u8 p (char->integer #\?)))
                     (else (if #f #f)))
                   (when (eq? (port-buffer-mode p) 'line)
                     (flush-output-port p)))
                  (else
                   (let ((b (char->integer c)))
                     (if (fx<=? b #xff)
                         (put-u8 p b)
                         (put-u8 p (char->integer #\?)))))))
           (else
            (error 'put-char "Unimplemented output codec"
                   (transcoder-codec (port-transcoder p))))))
        ((string? (port-buffer p))
         (when (eqv? c #\linefeed)
           ;; FIXME
           (case (port-eol-style p)
             ((crlf)
              (put-char p #\return))))
         (let ((b (port-buffer p))
               (w (port-buffer-w p)))
           ;; There is always room for at least one char.
           (string-set! b w c)
           (let ((w* (fx+ w 1)))
             (trace "put-char " c " b=" b " r=" (port-buffer-r p) " w=" w " w*=" w*)
             (port-buffer-w-set! p w*)
             (when (or (fx=? w* (string-length b))
                       (and (eqv? c #\linefeed)
                            (eqv? (port-buffer-mode p) 'line)))
               (flush-output-port p)))))
        (else
         (assert (textual-port? p))
         (assertion-violation 'put-char "The port is closed" p c))))

(define put-string
  (case-lambda
    ((p bv)
     (put-string p bv 0 (string-length bv)))
    ((p bv start)
     (put-string p bv start (fx- (string-length bv) start)))
    ((p bv start count)
     ;; TODO: optimize
     (let ((end (+ start count)))
       (assert (fx<=? 0 start end (string-length bv)))
       (do ((i start (fx+ i 1)))
           ((fx=? i end))
         (put-char p (string-ref bv i)))))))

(define (put-datum tp d)
  (write d tp))

;;; Input/output ports

;; open-file-input/output-port is defined elsewhere.
;; make-custom-binary-input/output-port
;; make-custom-textual-input/output-port

;;; Simple I/O

(define (call-with-input-file filename proc)
  (let ((p (open-input-file filename)))
    (let-values ((ret (proc p)))
      (close-input-port p)
      (apply values ret))))

(define (call-with-output-file filename proc)
  (let ((p (open-output-file filename)))
    (let-values ((ret (proc p)))
      (close-output-port p)
      (apply values ret))))

(define (with-input-from-file filename thunk)
  (let ((p (open-input-file filename)))
    (define (swap-handler!)
      (let ((temp p))
        (set! p *curin*)
        (set! *curin* temp)))
    (let-values ((ret (dynamic-wind swap-handler! thunk swap-handler!)))
      (close-input-port p)
      (apply values ret))))

(define (with-output-to-file filename thunk)
  (let ((p (open-output-file filename)))
    (define (swap-handler!)
      (let ((temp p))
        (set! p *curout*)
        (set! *curout* temp)))
    (let-values ((ret (dynamic-wind swap-handler! thunk swap-handler!)))
      (close-output-port p)
      (apply values ret))))

(define (open-input-file filename)
  (open-file-input-port filename
                        (file-options)
                        (buffer-mode block)
                        (native-transcoder)))

(define (open-output-file filename)
  (open-file-output-port filename
                         (file-options)
                         (buffer-mode block)
                         (native-transcoder)))

(define (close-input-port p)
  (assert (input-port? p))
  (close-port p))

(define (close-output-port p)
  (assert (output-port? p))
  (close-port p))

(define read-char
  (case-lambda
    (() (get-char (current-input-port)))
    ((tp) (get-char tp))))

(define peek-char
  (case-lambda
    (() (lookahead-char (current-input-port)))
    ((tp) (lookahead-char tp))))

(define write-char
  (case-lambda
    ((ch) (put-char (current-output-port) ch))
    ((ch tp) (put-char tp ch))))

(define newline
  (case-lambda
    (() (write-char #\linefeed))
    ((p) (write-char #\linefeed p))))

(define-syntax %bytevector
  (lambda (x)
    (syntax-case x ()
      ((_ v)
       (string? (syntax->datum #'v))
       (string->utf8 (syntax->datum #'v))))))

(define (display-char-escape c p)
  (display "\\x" p)
  (display (number->string (char->integer c) 16) p)
  (put-char p #\;))

(define (write-symbol name p)
  ;; TODO: make tables and handle latin-1 ports
  (define (char-initial? c)
    (or (char<=? #\a c #\z)
        (char<=? #\A c #\Z)
        (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
        (and (fx>? (char->integer c) 127)
             (memq (char-general-category c)
                   '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co)))))
  (define (char-subsequent? c)
    (or (char<=? #\a c #\z)
        (char<=? #\A c #\Z)
        (char<=? #\0 c #\9)
        (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~
                  #\+ #\- #\. #\@))
        (and (fx>? (char->integer c) 127)
             (memq (char-general-category c)
                   '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co
                        Nd Mc Me)))))
  (cond ((eqv? 0 (string-length name))
         (display "||" p))                  ;not in R6RS
        ((member name '("+" "-" "..."))
         (display name p))
        (else
         (let ((c0 (string-ref name 0)))
           (if (or (char-initial? c0)
                   (and (eqv? c0 #\-)
                        (fx>=? (string-length name) 2)
                        (eqv? (string-ref name 1) #\>)))
               (put-char p c0)
               (display-char-escape c0 p)))
         (do ((i 1 (fx+ i 1)))
             ((fx=? i (string-length name)))
           (let ((c (string-ref name i)))
             (if (not (char-subsequent? c))
                 (display-char-escape c p)
                 (put-char p c)))))))

(define (display* v p write?)
  (define (display-hex i p)
    ;; Displays 24-bit integers in hexadecimal notation
    (define chars (%bytevector "0123456789ABCDEF"))
    (when (eqv? i 0) (put-char p #\0))
    (let lp ((s 24) (lz #t))
      (unless (fx<? s 0)
        (let* ((nibble (fxand (fxarithmetic-shift-right i s) #xf))
               (lz (and lz (eqv? nibble 0))))
          (when (not lz)
            (put-char p (integer->char (bytevector-u8-ref chars nibble))))
          (lp (fx- s 4) lz)))))
  (define (char-unprintable? c)
    ;; TODO: unicode
    ;; some are unprintable as chars, some in strings too.
    ;; which ones are printable depends on the encoding
    ;; of the output port!
    (let ((v (char->integer c)))
      (or (fx<? v (char->integer #\space))
          (fx<? #x7e v #xA0))))
  (define chars
    '#("nul" #f #f #f #f #f #f "alarm" "backspace" "tab"
       "linefeed" "vtab" "page" "return" #f #f #f #f #f #f #f #f
       #f #f #f #f #f "esc" #f #f #f #f "space" #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
       #f #f #f #f #f #f #f #f #f #f #f #f "delete"))
  (define string-escapes
    (%bytevector
     "XXXXXXXabtnvfrXXXXXXXXXXXXXXXXXXXX\"XXXXXXXXXXXXXXXXXXXXX\
        XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\\"))
  (cond
    ((char? v)
     (cond (write?
            (put-char p #\#)
            (put-char p #\\)
            (let ((i (char->integer v)))
              (cond ((and (fx<? i (vector-length chars))
                          (vector-ref chars i))
                     => (lambda (str) (put-string p str)))
                    ((char-unprintable? v)
                     (put-char p #\x)
                     (display-hex i p))
                    (else
                     (put-char p v)))))
           (else
            (put-char p v))))
    ((bytevector? v)
     (display "#vu8(" p)
     (let ((len (bytevector-length v)))
       (unless (eqv? len 0)
         (display (bytevector-u8-ref v 0) p)
         (do ((i 1 (fx+ i 1)))
             ((fx=? i len))
           (put-char p #\space)
           (display (bytevector-u8-ref v i) p))))
     (put-char p #\)))
    ((null? v)
     (put-char p #\()
     (put-char p #\)))
    ((boolean? v)
     (put-char p #\#)
     (if v (put-char p #\t) (put-char p #\f)))
    ((eof-object? v)
     (display "#!eof" p))
    (($void? v)
     (display "#<void #x" p)
     (display (number->string ($void->fixnum v) 16) p)
     (display ">" p))
    (($immsym? v)
     (let ((alphabet     (%bytevector "abcdefghijklmnopqrstuvwxyz-/<=>"))
           (end-alphabet (%bytevector "acdefghklmnopqrstvxy!*+-/08<=>?")))
       (let ((s ($immsym->fixnum v)))
         (let ((s (fxarithmetic-shift-right s 5))
               (c (fx- (fxand s #b11111) 1)))
           ;; The first character might need to be escaped
           (let ((ch (integer->char (bytevector-u8-ref (if (eqv? s 0) end-alphabet alphabet) c))))
             (if (and write? (memv ch '(#\- #\+ #\0 #\8)))
                 (display-char-escape ch p)
                 (put-char p ch)))
           ;; Output the first of the characters, if there are any
           (let lp ((s s))
             (unless (eqv? s 0)
               (let ((s (fxarithmetic-shift-right s 5))
                     (c (fx- (fxand s #b11111) 1)))
                 (cond ((eqv? s 0)
                        (put-char p (integer->char (bytevector-u8-ref end-alphabet c))))
                       (else
                        (put-char p (integer->char (bytevector-u8-ref alphabet c)))
                        (lp s))))))))))
    ((string? v)
     (if write? (put-char p #\"))
     (do ((len (string-length v))
          (i 0 (fx+ i 1)))
         ((fx=? i len))
       (let* ((c (string-ref v i))
              (v (char->integer c)))
         (cond ((and write? (fx<? v (bytevector-length string-escapes))
                     (not (eqv? (bytevector-u8-ref string-escapes v)
                                (char->integer #\X))))
                (put-char p #\\)
                (put-char p (integer->char (bytevector-u8-ref string-escapes v))))
               ((and write? (char-unprintable? c))
                (put-char p #\\) (put-char p #\x) (display-hex v p) (put-char p #\;))
               (else
                (put-char p c)))))
     (if write? (put-char p #\")))
    ((pair? v)
     (cond ((and (pair? (cdr v))
                 (null? (cddr v))
                 (assq (car v) '((quote . "'")
                                 (quasiquote . "`")
                                 (unquote . ",")
                                 (unquote-splicing . ",@")
                                 (syntax . "#'")
                                 (quasisyntax . "#`")
                                 (unsyntax . "#,")
                                 (unsyntax-splicing . "#,@"))))
            => (lambda (a)
                 (display (cdr a) p)
                 (display* (cadr v) p write?)))
           (else
            (put-char p #\()
            (let lp ((i v))
              (unless (null? i)
                (display* (car i) p write?)
                (let ((i (cdr i)))
                  (cond ((null? i))
                        ((pair? i)
                         (put-char p #\space)
                         (lp i))
                        (else
                         (put-char p #\space)
                         (put-char p #\.)
                         (put-char p #\space)
                         (display* i p write?))))))
            (put-char p #\)))))
    ((vector? v)
     (put-char p #\#) (put-char p #\()
     (let ((len (vector-length v)))
       (unless (eqv? len 0)
         (display* (vector-ref v 0) p write?)
         (do ((i 1 (fx+ i 1)))
             ((fx=? i len))
           (put-char p #\space)
           (display* (vector-ref v i) p write?))))
     (put-char p #\)))
    ((number? v)
     ($display-number v p #t))
    ((procedure? v)
     (let ((info ($procedure-info v)))
       (define (info? x) (and ($box? x) (eq? ($box-type x) 'info)))
       (define (info-free-length x) ($box-ref x 1))
       (define (info-name x) ($box-ref x 2))
       (define (info-source x) ($box-ref x 3))
       (cond ((info? info)
              (if (eq? (info-free-length info) 0)
                  (display "#<procedure " p)
                  (display "#<closure " p))
              (if (info-name info)
                  (display (info-name info) p)
                  (display "(anonymous)" p))
              (when (info-source info)
                (display #\space p)
                (display (car (info-source info)) p)
                (display #\: p)
                (display (cdr (info-source info)) p)))
             (else
              ;; really shouldn't happen
              (display "#<procedure @ " p)
              (display ($procedure-entry v) p))))
     (display ">" p))
    ((port? v)
     ;; [binary|textual]-[input|output|input/output]-port
     (display "#<" p)
     (if (textual-port? v)
         (display "textual-" p)
         (display "binary-" p))
     (if (input-port? v)
         (if (output-port? v)
             (display "input/output-port " p)
             (display "input-port " p))
         (display "output-port " p))
     (write (port-id v) p)
     (when (not (port-buffer v))
       (display " (closed)" p))
     (display #\> p))
    (($box? v)
     (let ((t ($box-type v)))
       (cond ((symbol? t)
              ;; FIXME: This case should be phased out
              (display "#<" p)
              (display ($box-type v) p)
              (when (eq? ($box-type v) 'rtd)
                (display " " p)
                (write ($box-ref v 2) p))
              (display ">" p))
             ((and ($box-header? t) (eqv? ($box-header-type t) #x03))
              ;; Symbol
              (let ((is-gensym (not (eqv? ($box-header-length t) 1)))
                    (symbol-name ($box-ref v 0)))
                ;; FIXME: this doesn't support write syntax.
                (cond ((and write? is-gensym)
                       ;; Gensym write syntax
                       (let ((unique-string ($box-ref v 1)))
                         (when (or (not symbol-name) (not unique-string))
                           ($gensym-generate-names! v)))
                       (let ((symbol-name ($box-ref v 0))
                             (unique-string ($box-ref v 1)))
                         (display "#{" p)
                         (display (utf8->string symbol-name) p)
                         (display " |" p)
                         (display (utf8->string unique-string) p)
                         (display "|}" p)))
                      (else
                       (when (not symbol-name)
                         ($gensym-generate-names! v))
                       (let* ((symbol-name ($box-ref v 0))
                              ;; FIXME: This is a wasteful conversion
                              (name (utf8->string symbol-name)))
                         (if write?
                             (write-symbol name p)
                             (display name p)))))))
             ((record-type-descriptor? t)
              (let ((writer (record-writer t)))
                (writer v p write)))
             (else
              (display "#<box " p)
              (if ($box-header? t)
                  (display ($box-header-type t) p)
                  (display t p))
              (display ">" p)))))
    (else
     (display "#<unknown>" p))))

(define display
  (case-lambda
    ((v) (display* v (current-output-port) #f))
    ((v p) (display* v p #f))))

(define write
  (case-lambda
    ((v)
     (write v (current-output-port)))
    ((v p)
     (let ((counter -1)
           (shared (find-shared v)))
       (if (zero? (hashtable-size shared))
           (display* v p #t)
           (letrec ((increment!
                     (lambda ()
                       (set! counter (fx+ counter 1))
                       counter)))
             ;; Cycles were detected.
             (write/c v p shared increment!)))))))

;; Print with cycles in pairs and vectors. TODO: what about boxes?
(define (write/c v p shared increment!)
  (define (write-prefix id)
    (put-char p #\#)
    (display id p)
    (put-char p #\=))
  (define (write-ref id)
    (put-char p #\#)
    (display id p)
    (put-char p #\#))
  (define (number! v)
    (let ((id (hashtable-ref shared v #f)))
      (when (eq? id 'must-assign)
        (let ((id (increment!)))
          (write-prefix id)
          (hashtable-set! shared v id)))
      id))
  (let ((id (number! v)))
    (cond
      ((fixnum? id)
       (write-ref id))
      ((pair? v)
       (cond ((and (pair? (cdr v))
                   (null? (cddr v))
                   (assq (car v) '((quote . "'")
                                   (quasiquote . "`")
                                   (unquote . ",")
                                   (unquote-splicing . ",@")
                                   (syntax . "#'")
                                   (quasisyntax . "#`")
                                   (unsyntax . "#,")
                                   (unsyntax-splicing . "#,@"))))
              => (lambda (a)
                   (display (cdr a) p)
                   (write/c (cadr v) p shared increment!)))
             (else
              ;; FIXME: cycles are not detected here
              (put-char p #\()
              (let lp ((i v))
                (unless (null? i)
                  (write/c (car i) p shared increment!)
                  (let ((i (cdr i)))
                    (cond ((null? i))
                          ((pair? i)
                           (let ((id (number! i)))
                             (cond ((fixnum? id)
                                    (put-char p #\space)
                                    (put-char p #\.)
                                    (put-char p #\space)
                                    (write-ref id))
                                   (else
                                    (put-char p #\space)
                                    (lp i)))))
                          (else
                           (put-char p #\space)
                           (put-char p #\.)
                           (put-char p #\space)
                           (write/c i p shared increment!))))))
              (put-char p #\)))))
      ((vector? v)
       (put-char p #\#) (put-char p #\()
       (let ((len (vector-length v)))
         (unless (eqv? len 0)
           (write/c (vector-ref v 0) p shared increment!)
           (do ((i 1 (fx+ i 1)))
               ((fx=? i len))
             (put-char p #\space)
             (write/c (vector-ref v i) p shared increment!))))
       (put-char p #\)))
      (else
       (write v p)))))

;; Build a hashtable that maps objects to 'must-assign if they
;; appear more than once in the structure.
(define (find-shared v)
  (let ((s (make-eq-hashtable)))
    (let lp ((v v))
      (when (or (pair? v) (vector? v))
        (cond ((hashtable-ref s v #f) =>
               (lambda (id)
                 (when (eq? id 'unassigned)
                   (hashtable-set! s v 'must-assign))))
              ((pair? v)
               (hashtable-set! s v 'unassigned)
               (lp (car v))
               (lp (cdr v)))
              ((vector? v)
               (when (fxpositive? (vector-length v))
                 (hashtable-set! s v 'unassigned)
                 (vector-for-each lp v))))
        (when (eq? (hashtable-ref s v #f) 'unassigned)
          ;; The object does not contain a reference to itself.
          (hashtable-set! s v #f))))
    s))

;;; File system

;; See (loko runtime init).

;;; Standard ports

;; These are registered with $init-standard-ports. Each new process
;; will get its own set of these.
(define *stdin-read*)
(define *stdout-write*)
(define *stderr-write*)

(define (standard-input-port)
  (make-custom-binary-input-port "*stdin*" *stdin-read* #f #f #f))

(define (standard-output-port)
  (let ((p (make-custom-binary-output-port "*stdout*" *stdout-write* #f #f #f)))
    ($port-buffer-mode-set! p (buffer-mode line))
    p))

(define (standard-error-port)
  (let ((p (make-custom-binary-output-port "*stderr*" *stderr-write* #f #f #f)))
    ($port-buffer-mode-set! p (buffer-mode none))
    p))

(define *curin*)
(define *curout*)
(define *curerr*)

;; XXX: the one-argument version of these is an extension. A better
;; extension is to make them proper parameters.

(define current-output-port
  (case-lambda
    (() *curout*)
    ((p)
     (assert (and (textual-port? p) (output-port? p)))
     (set! *curout* p))))

(define current-error-port
  (case-lambda
    (() *curerr*)
    ((p)
     (assert (and (textual-port? p) (output-port? p)))
     (set! *curerr* p))))

(define current-input-port
  (case-lambda
    (() *curin*)
    ((p)
     (assert (and (textual-port? p) (input-port? p)))
     (set! *curin* p))))

(define ($init-standard-ports stdin-read stdout-write stderr-write eolstyle)
  (set! *stdin-read* stdin-read)
  (set! *stdout-write* stdout-write)
  (set! *stderr-write* stderr-write)
  (let ((tc (make-transcoder (utf-8-codec) eolstyle (error-handling-mode replace))))
    (set! *curin* (transcoded-port (standard-input-port) tc))
    (set! *curout* (transcoded-port (standard-output-port) tc))
    (set! *curerr* (transcoded-port (standard-error-port) tc))))

($init-standard-ports (lambda x 0)
                      (lambda (bv start count)
                        (do ((end (+ start count))
                             (i start (fx+ i 1)))
                            ((fx=? i end) count)
                          ($debug-put-u8 (bytevector-u8-ref bv i))))
                      (lambda (bv start count) count)
                      (native-eol-style)))
