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

(display "Starting VBE...\n")
(define VBE (make·pci·vbe (find probe·pci·vbe? (pci-scan-bus #f))))

(define (vbe-print-modes vbe)
  (let ((BIOS (vbe-bios vbe))
        (info (vbe-supervga-info vbe)))
    (for-each
     (lambda (mode)
       (define (print . x) (for-each display x) (newline))
       (let ((i (vesa-get-mode-info BIOS mode)))
         (print #\[ (number->string mode 16) #\] #\space
                (supervga-mode-XResolution i) #\x (supervga-mode-YResolution i)
                #\space (supervga-mode-BitsPerPixel i) " bpp "
                (assv (supervga-mode-MemoryModel i) supervga-memory-models)
                (if (supervga-mode-linear-framebuffer? i)
                    " LFB" " no-LFB"))))
     (supervga-info-VideoModes info))))

(define BIOS (vbe-bios VBE))
(define info (vbe-supervga-info VBE))

(display "Getting modes\n")
(vbe-print-modes VBE)

;; FIXME: Do something smarter when choosing a mode
(vesa-set-mode BIOS #x192 'lfb #f #f)

(define modeinfo (vesa-get-mode-info BIOS (vesa-get-mode BIOS)))

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
        (YResolution (supervga-mode-YResolution modeinfo))
        (pixel-shift
         (case 32 ;;bpp
           ((8) 0)
           ((16) 1)
           ((24) #f)
           ((32) 2))))
    (let ((limit (fx+ PhysBasePtr (* LinBytesPerScanLine YResolution))))
      (lambda (x y c)
        (let ((addr (fx+ PhysBasePtr
                         (fx+ (fx* LinBytesPerScanLine y)
                              (fxarithmetic-shift-left x pixel-shift)))))
          (unless (fx<? 0 addr limit)
            (error 'put-pixel "Out of range" x y))
          ;; a bit more complicated for 24 bpp
          (put-mem-u32 addr c))))))

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

(clear-screen)

(box 0 0 (fx- width 1) (fx- height 1) #x00ff0000)

(box 10 10 (fx- width 11) (fx- height 11) #x0000ff00)

(box 20 20 (fx- width 21) (fx- height 21) #x000000ff)

(circle 100 100 30 #xff0000)

(circle 100 100 20 #x00ff00)

(circle 100 100 10 #x0000ff)


(define rgb
  (let ((rm (- (expt 2 (supervga-mode-RedMaskSize modeinfo)) 1))
        (rs (supervga-mode-RedFieldPosition modeinfo))
        (gm (- (expt 2 (supervga-mode-GreenMaskSize modeinfo)) 1))
        (gs (supervga-mode-GreenFieldPosition modeinfo))
        (bm (- (expt 2 (supervga-mode-BlueMaskSize modeinfo)) 1))
        (bs (supervga-mode-BlueFieldPosition modeinfo)))
    (lambda (r g b)
      (fxior (fxarithmetic-shift-left (fxand r rm) rs)
             (fxarithmetic-shift-left (fxand g gm) gs)
             (fxarithmetic-shift-left (fxand b bm) bs)))))

(do ((r 1 (fx+ r 1)))
    ((fx>=? r (/ 768 2)))
  (circle (/ 1024 2)
          (/ 768 2)
          r
          (rgb (exact (round (* (/ r (/ 768 2))
                                256)))
               (exact (round (* (- 1 (/ r (/ 768 2)))
                                256)))
               #x00)))
