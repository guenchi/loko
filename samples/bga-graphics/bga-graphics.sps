#!/usr/bin/env scheme-script
;; Loko Scheme sample
;; Copyright © 2019 Göran Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs)
  (loko system unsafe))

;;; Bochs Graphics Array

;; I/O registers for the BGA device
(define bga-index #x01ce)
(define bga-data #x01cf)

;; Registers selected by the bga-index port
(define VBE_DISPI_INDEX_ID 0)
(define VBE_DISPI_INDEX_XRES 1)
(define VBE_DISPI_INDEX_YRES 2)
(define VBE_DISPI_INDEX_BPP 3)
(define VBE_DISPI_INDEX_ENABLE 4)
(define VBE_DISPI_INDEX_BANK 5)
(define VBE_DISPI_INDEX_VIRT_WIDTH 6)
(define VBE_DISPI_INDEX_VIRT_HEIGHT 7)
(define VBE_DISPI_INDEX_X_OFFSET 8)
(define VBE_DISPI_INDEX_Y_OFFSET 9)

;; Linear address of the banked memory. If you want to use the linear
;; framebuffer, you'll need to find the BAR in PCI configuration
;; space.
(define bank-address #xA0000)
(define bank-length #x10000)

;; Write a BGA register
(define (bga-write reg value)
  (put-i/o-u16 bga-index reg)
  (put-i/o-u16 bga-data value))

;; Read a BGA register
(define (bga-read reg)
  (put-i/o-u16 bga-index reg)
  (get-i/o-u16 bga-data))

;; Change the BGA graphics mode
(define (bga-set-mode width height bit-depth
                      enable-lfb? clear-memory?)
  (bga-write VBE_DISPI_INDEX_ENABLE 0)
  (bga-write VBE_DISPI_INDEX_XRES width)
  (bga-write VBE_DISPI_INDEX_YRES height)
  (bga-write VBE_DISPI_INDEX_BPP bit-depth)
  (bga-write VBE_DISPI_INDEX_ENABLE
             (fxior 1
                    (if enable-lfb? #x40 0)
                    (if clear-memory? 0 #x80))))

(define current-bank #f)
(define (bga-set-bank! n)
  (unless (eqv? n current-bank)
    (set! current-bank n)
    (bga-write VBE_DISPI_INDEX_BANK n)))

(display "Starting Bochs graphics\n")
(unless (fx<=? #xB0C0 (bga-read VBE_DISPI_INDEX_ID) #xB0CF)
  (error 'bga "No Bochs graphics detected"))

(define w 640)
(define h 480)

(bga-set-mode w h 32 #f #t)
(bga-set-bank! 0)

;;; Pixel drawing

(define (set-pixel x y c)
  (let ((pixel (fx* (fx+ x (fx* y w)) 4)))
    (let-values ([(bank offset) (fxdiv-and-mod pixel bank-length)])
      (bga-set-bank! bank)
      (put-mem-u32 (fx+ bank-address offset) c))))

;;; Color space

(define (color-rgb r g b)
  (fxior (fxarithmetic-shift-left g 16)
         (fxarithmetic-shift-left r 8)
         b))

(define (hsv->rgb H S V)
  (let* ((C (* V S))
         (X (* C (- 1 (abs (- (mod (/ H 60) 2) 1)))))
         (m (- V C)))
    (let-values (((R* G* B*)
                  (cond ((<= H 60) (values C X 0))
                        ((<= H 120) (values X C 0))
                        ((<= H 180) (values 0 C X))
                        ((<= H 240) (values 0 X C))
                        ((<= H 300) (values X 0 C))
                        (else (values C 0 X)))))
      (color-rgb (round (* 255 (+ R* m)))
                 (round (* 255 (+ G* m)))
                 (round (* 255 (+ B* m)))))))

;;; Bresenham's line drawing algorithm

(define-syntax let-swap
  (lambda (x)
    (syntax-case x ()
      ((_ () body)
       #'(let () body))
      ((_ ((a b) rest ...) body)
       #'(let ((b a) (a b))
           (let-swap (rest ...) body))))))

(define (draw-line! x0 y0 x1 y1 color)
  (let f ((x0 x0) (y0 y0) (x1 x1) (y1 y1) (steep #f))
    (cond
      ((fx<? (abs (fx- x0 x1)) (abs (fx- y0 y1)))
       (let-swap ((x0 y0)
                  (x1 y1))
         (f x0 y0 x1 y1 #t)))
      ((fx>? x0 x1)
       (let-swap ((x0 x1)
                  (y0 y1))
         (f x0 y0 x1 y1 steep)))
      (else
       (let* ((dx (fx- x1 x0))
              (dy (fx- y1 y0))
              (derror2 (fx* (abs dy) 2)))
         (let lp ((x x0) (y y0) (error2 0))
           (when (fx<=? x x1)
             (if steep
                 (set-pixel y x color)
                 (set-pixel x y color))
             (let ((x (fx+ x 1))
                   (error2 (fx+ error2 derror2)))
               (if (fx>? error2 dx)
                   (let ((y (fx+ y (if (fx>? y1 y0) 1 -1)))
                         (error2 (fx- error2 (fx* dx 2))))
                     (lp x y error2))
                   (lp x y error2))))))))))

;;; Models

(define-record-type model
  (fields verts faces))

(define (string-split str c)
  (let lp ((start 0) (end 0))
    (cond ((fx=? end (string-length str))
           (list (substring str start end)))
          ((char=? c (string-ref str end))
           (cons (substring str start end)
                 (lp (fx+ end 1) (fx+ end 1))))
          (else
           (lp start (fx+ end 1))))))

(define (load-wavefront-model filename)
  (call-with-input-file filename
    (lambda (p)
      (let lp ((vert* '())
               (face* '()))
        (let ((line (get-line p)))
          (cond
            ((eof-object? line)
             (let ((verts (list->vector (reverse vert*)))
                   (faces (list->vector (reverse face*))))
               (display (list "Loaded" filename "with"
                              (vector-length verts)
                              "verts and"
                              (vector-length faces)
                              "faces"))
               (newline)
               (make-model verts faces)))
            ((< (string-length line) 2)
             (lp vert* face*))
            ((and (char=? (string-ref line 0) #\v)
                  (char=? (string-ref line 1) #\space))
             (let ((v
                    (list->vector
                     (map (lambda (n)
                            (inexact (string->number n)))
                          (cdr (string-split line #\space))))))
               (lp (cons v vert*) face*)))
            ((and (char=? (string-ref line 0) #\f)
                  (char=? (string-ref line 1) #\space))
             (let ((f
                    (list->vector
                     (map
                      (lambda (s)
                        (- (string->number (car (string-split s #\/))) 1))
                      (cdr (string-split line #\space))))))
               (lp vert* (cons f face*))))
            (else
             (lp vert* face*))))))))

(define scale-factor 1.5)

(define (vec3-x v) (* scale-factor (vector-ref v 0)))
(define (vec3-y v) (* scale-factor (vector-ref v 1)))
(define (vec3-z v) (* scale-factor (vector-ref v 2)))

;;; Wireframe

(define (draw-wireframe model width height)
  (let ((faces (model-faces model))
        (verts (model-verts model))
        (width (inexact width))
        (height (inexact height)))
    (do ((i 0 (fx+ i 1)))
        ((fx=? i (vector-length faces)))
      (let ((face (vector-ref faces i)))
        (do ((j 0 (fx+ j 1)))
            ((fx=? j 3))
          (let ((v0 (vector-ref verts (vector-ref face j)))
                (v1 (vector-ref verts (vector-ref face (fxmod (fx+ j 1) 3)))))
            ;; y and z are swapped to rotate the model
            (let ((x0 (exact (flround (fl/ (fl* (fl+ 1.0 (vec3-x v0)) width) 2.0))))
                  (y0 (exact (flround (fl/ (fl* (fl+ 1.0 (vec3-z v0)) height) 2.0))))
                  (x1 (exact (flround (fl/ (fl* (fl+ 1.0 (vec3-x v1)) width) 2.0))))
                  (y1 (exact (flround (fl/ (fl* (fl+ 1.0 (vec3-z v1)) height) 2.0)))))
              (draw-line! x0 y0 x1 y1 (hsv->rgb (* 360 (/ i (vector-length faces)))
                                                1 1)))))))))

;;; Demo

(define text-model
  (load-wavefront-model "/boot/loko.obj"))

(draw-wireframe text-model 300 300)
