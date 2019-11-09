#!/usr/bin/env scheme-script
;; Loko Scheme sample
;; Copyright © 2019 Göran Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs)
  (loko system fibers)
  (loko drivers pci)
  (loko drivers video vbe)
  (loko system unsafe))

(define (print . x) (for-each display x) (newline))

(print "Starting VBE...")
(define VBE (make·pci·vbe (find probe·pci·vbe? (pci-scan-bus #f))))

(define (get-modes vbe)
  (let ((BIOS (vbe-bios vbe))
        (info (vbe-supervga-info vbe)))
    (map
     (lambda (mode)
       (let ((i (vesa-get-mode-info BIOS mode)))
         (print #\[ (number->string mode 16) #\] #\space
                (supervga-mode-XResolution i) #\x (supervga-mode-YResolution i)
                #\space (supervga-mode-BitsPerPixel i) " bpp "
                (assv (supervga-mode-MemoryModel i) supervga-memory-models)
                (if (supervga-mode-linear-framebuffer? i)
                    " LFB" " no-LFB"))
         i))
     (supervga-info-VideoModes info))))

(write (vbe-supervga-info VBE))
(newline)

(display "Getting modes\n")

;; FIXME: Do something smarter when choosing a mode
(let* ((modes (get-modes VBE))
       (mode (find (lambda (info)
                     (and (eqv? (supervga-mode-XResolution info) 1280)
                          (eqv? (supervga-mode-YResolution info) 1024)
                          (eqv? (supervga-mode-BitsPerPixel info) 32)
                          (eqv? (supervga-mode-MemoryModel info) 6)))
                   modes)))
  (when (not mode)
    (error #f "Did not find a 1280x1024x32 direct color graphics mode"))
  (print "Switching to mode " (number->string (supervga-mode-Number mode) 16))
  (print mode)
  (vesa-set-mode (vbe-bios VBE) (supervga-mode-Number mode) 'lfb #f #f))

(define modeinfo (vesa-get-mode-info (vbe-bios VBE)
                                     (vesa-get-mode (vbe-bios VBE))))

(define (clear-screen)
  (let* ((base (supervga-mode-PhysBasePtr modeinfo))
         (bpsl (supervga-mode-LinBytesPerScanLine modeinfo))
         (yres (supervga-mode-YResolution modeinfo))
         (end (* bpsl yres)))
    (do ((i 0 (fx+ i 8)))
        ((fx>=? i end))
      (put-mem-s61 (fx+ base i) 0))))

(define put-pixel
  (let ((PhysBasePtr (supervga-mode-PhysBasePtr modeinfo))
        (LinBytesPerScanLine (supervga-mode-LinBytesPerScanLine modeinfo))
        (YResolution (supervga-mode-YResolution modeinfo)))
    (let ((limit (fx- (fx* LinBytesPerScanLine YResolution) 1)))
      (case (supervga-mode-BitsPerPixel modeinfo)
        ((8)
         (lambda (x y c)
           (let* ((offset (fx+ (fx* LinBytesPerScanLine y) x))
                  (addr (fx+ PhysBasePtr offset)))
             (when (fx<=? 0 offset limit)
               (put-mem-u8 addr c)))))
        ((15 16)
         (lambda (x y c)
           (let* ((offset (fx+ (fx* LinBytesPerScanLine y) (fx* x 2)))
                  (addr (fx+ PhysBasePtr offset)))
             (when (fx<=? 0 offset limit)
               (put-mem-u16 addr c)))))
        ((24)
         (lambda (x y c)
           (let* ((offset (fx+ (fx* LinBytesPerScanLine y) (fx* x 3)))
                  (addr (fx+ PhysBasePtr offset)))
             (when (fx<=? 0 offset limit)
               (cond ((fxodd? addr)
                      ;; 0 _1_ 2 3
                      (put-mem-u8 addr (fxbit-field c 0 8))
                      ;; 0 1 _2 3_
                      (put-mem-u16 (fx+ addr 1) (fxbit-field c 8 24)))
                     (else
                      ;; _0 1_ 2 3
                      (put-mem-u16 addr (fxbit-field c 0 16))
                      ;; 0 1 _2_ 3
                      (put-mem-u8 (fx+ addr 2) (fxbit-field c 16 24))))))))
        (else
         (lambda (x y c)
           (let* ((offset (fx+ (fx* LinBytesPerScanLine y) (fx* x 4)))
                  (addr (fx+ PhysBasePtr offset)))
             (when (fx<=? 0 offset limit)
               (put-mem-u32 addr c)))))))))

;;http://groups.csail.mit.edu/graphics/classes/6.837/F98/Lecture6/Slide11.html
(define (circle cx cy radius c)
  (define fxasl fxarithmetic-shift-left)
  (let lp ((x 0)
           (y radius)
           (p (fxdiv (fx- 5 (fx* radius 5))
                     4)))
    (circle-point cx cy x y c)
    (when (fx<? x y)
      (let ((x (fx+ x 1)))
        (if (fxnegative? p)
            (lp x y (fx+ p (fx+ (fxasl x 1) 1)))
            (let ((y (fx- y 1)))
              (lp x y (fx+ p (fx+ (fxasl (fx- x y) 1) 1)))))))))

(define (circle-point cx cy x y c)
  (cond ((eqv? x 0)
         (put-pixel cx (fx+ cy y) c)
         (put-pixel cx (fx- cy y) c)
         (put-pixel (fx+ cx y) cy c)
         (put-pixel (fx- cx y) cy c))
        ((eqv? x y)
         (put-pixel (fx+ cx x) (fx+ cy y) c)
         (put-pixel (fx- cx x) (fx+ cy y) c)
         (put-pixel (fx+ cx y) (fx- cy y) c)
         (put-pixel (fx- cx y) (fx- cy y) c))
        ((fx<? x y)
         (put-pixel (fx+ cx x) (fx+ cy y) c)
         (put-pixel (fx- cx x) (fx+ cy y) c)
         (put-pixel (fx+ cx x) (fx- cy y) c)
         (put-pixel (fx- cx x) (fx- cy y) c)
         (put-pixel (fx+ cx y) (fx+ cy x) c)
         (put-pixel (fx- cx y) (fx+ cy x) c)
         (put-pixel (fx+ cx y) (fx- cy x) c)
         (put-pixel (fx- cx y) (fx- cy x) c))))

;;http://groups.csail.mit.edu/graphics/classes/6.837/F98/Lecture5/Slide20.html
(define (draw-line x0 y0 x1 y1 c)
  (let* ((dx (abs (fx- x1 x0)))
         (dy (abs (fx- y1 y0)))
         (sx (if (fx<? x0 x1) 1 -1))
         (sy (if (fx<? y0 y1) 1 -1)))
    (let lp ((err (fx- dx dy))
             (x x0)
             (y y0))
      (put-pixel x y c)
      (unless (and (eqv? x x1) (eqv? y y1))
        (let ((e2 (fxarithmetic-shift-left err 1)))
          (let-values (((err x)
                        (if (fx>? e2 (fx- dy))
                            (values (fx- err dy)
                                    (fx+ x sx))
                            (values err x))))
            (if (fx<? e2 dx)
                (lp (fx+ err dx) x (fx+ y sy))
                (lp err x y))))))))

(define height (supervga-mode-YResolution modeinfo))
(define width (supervga-mode-XResolution modeinfo))

(define (box x0 y0 x1 y1 c)
  (draw-line x0 y0 x1 y0 c)
  (draw-line x0 y0 x0 y1 c)
  (draw-line x0 y1 x1 y1 c)
  (draw-line x1 y1 x1 y0 c))

;;; FIXME: This is just nonsense... include the wireframe stuff and
;;; reuse here.


(define color-rgb
  (let ((rm (- (expt 2 (supervga-mode-RedMaskSize modeinfo)) 1))
        (rs (supervga-mode-RedFieldPosition modeinfo))
        (gm (- (expt 2 (supervga-mode-GreenMaskSize modeinfo)) 1))
        (gs (supervga-mode-GreenFieldPosition modeinfo))
        (bm (- (expt 2 (supervga-mode-BlueMaskSize modeinfo)) 1))
        (bs (supervga-mode-BlueFieldPosition modeinfo)))
    (lambda (r g b)
      (fxior (fxarithmetic-shift-left (round (* r rm)) rs)
             (fxarithmetic-shift-left (round (* g gm)) gs)
             (fxarithmetic-shift-left (round (* b bm)) bs)))))

(clear-screen)

(box 0 0 (fx- width 2) (fx- height 1) (color-rgb 1 0 0))

(box 10 10 (fx- width 11) (fx- height 11) (color-rgb 0 1 0))

(box 20 20 (fx- width 21) (fx- height 21) (color-rgb 0 0 1))

(circle 100 100 30 (color-rgb 1 0 0))

(circle 100 100 20 (color-rgb 0 1 0))

(circle 100 100 10 (color-rgb 0 0 1))



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
      (color-rgb (+ R* m)
                 (+ G* m)
                 (+ B* m)))))

(do ((w (fx- width 60))
     (i 0 (fx+ i 1)))
    ((fx=? i w))
  (draw-line (fx+ 30 i) 30 (fx+ 30 i) 50 (hsv->rgb (* 360 (/ i w)) 1 1)))

(let ((c (color-rgb 1 0 0))
      (black (color-rgb 0 0 0)))
  (let lp ()
    (do ((x 100 (fx+ x 1)))
        ((fx=? x (fx- width 100)))
      (circle x 200 30 black)
      (circle (fx+ x 1) 200 30 c)
      (sleep 1/60))
    (lp)))
