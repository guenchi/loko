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

;;; Characters

;; The tables are generated with https://gitlab.com/weinholt/unicode-codegen

(library (loko libs chars)
  (export char? char->integer integer->char
          char=? char<? char>? char<=? char>=?

          char-upcase char-downcase char-titlecase char-foldcase
          char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
          char-alphabetic? char-numeric? char-whitespace?
          char-upper-case? char-lower-case? char-title-case?
          char-general-category)
  (import (except (rnrs)
                  char? char->integer integer->char
                  char=? char<? char>? char<=? char>=?
                  char-upcase char-downcase char-titlecase char-foldcase
                  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
                  char-alphabetic? char-numeric? char-whitespace?
                  char-upper-case? char-lower-case? char-title-case?
                  char-general-category)
          (prefix (rnrs) sys:))

(define (char? x) (sys:char? x))

(define (char->integer x) (sys:char->integer x))

(define (integer->char sv) (sys:integer->char sv))

(define-syntax define-char-predicate
  (lambda (x)
    (syntax-case x ()
      ((_ name same? ->obj)
       #'(define name
           (case-lambda
             ((x y)
              (if (and (char? x) (char? y))
                  (same? (->obj x) (->obj y))
                  (assertion-violation 'name
                                       "This procedure needs characters." x y)))
             ((x y z)
              (if (and (char? x) (char? y) (char? z))
                  (let ((x (->obj x)) (y (->obj y)) (z (->obj z)))
                    (and (same? x y) (same? y z)))
                  (assertion-violation 'name
                                       "This procedure needs characters." x y z)))
             ((x y . y*)
              (if (and (char? x) (char? y))
                  (and (same? (->obj x) (->obj y))
                       (let lp ((x y) (y* y*))
                         (or (null? y*)
                             (let ((y (car y*)))
                               (if (char? y)
                                   (and (same? (->obj x) (->obj y))
                                        (lp y (cdr y*)))
                                   (assertion-violation 'name
                                                        "This procedure needs characters." y))))))
                  (apply assertion-violation 'name
                         "This procedure needs characters." x y y*)))))))))

(define-char-predicate char=? eq? (lambda (x) x))

(define-char-predicate char<? fx<? char->integer)

(define-char-predicate char>? fx>? char->integer)

(define-char-predicate char<=? fx<=? char->integer)

(define-char-predicate char>=? fx>=? char->integer)

;;; (rnrs unicode)

;; char-upcase, char-downcase and char-titlecase are automatically
;; generated and can be found later in this file.

;; (char-foldcase #\ς) => #\σ
(define (char-foldcase c)
  (if (or (eqv? c #\x130) (eqv? c #\x131))
      c
      (char-downcase (char-upcase c))))

(define-char-predicate char-ci=? eq? (lambda (c) (char->integer (char-foldcase c))))

(define-char-predicate char-ci<? fx<? (lambda (c) (char->integer (char-foldcase c))))

(define-char-predicate char-ci>? fx>? (lambda (c) (char->integer (char-foldcase c))))

(define-char-predicate char-ci<=? fx<=? (lambda (c) (char->integer (char-foldcase c))))

(define-char-predicate char-ci>=? fx>=? (lambda (c) (char->integer (char-foldcase c))))

(define (char-alphabetic? c)
  (let ((i (char->integer c)))
    (if (fx<=? i (char->integer #\delete))
        (or (fx<=? (char->integer #\A) i (char->integer #\Z))
            (fx<=? (char->integer #\a) i (char->integer #\z)))
        (char-set-contains? char-set:r6rs-alphabetic c))))

(define (char-numeric? c)
  (let ((i (char->integer c)))
    (if (fx<=? i (char->integer #\delete))
        (fx<=? (char->integer #\0) i (char->integer #\9))
        (char-set-contains? char-set:r6rs-numeric c))))

(define (char-whitespace? c)
  (let ((i (char->integer c)))
    (if (fx<=? i (char->integer #\delete))
        (or (fx<=? #x09 i #x0D)
            (eq? c #\space))
        (char-set-contains? char-set:r6rs-whitespace c))))

(define (char-upper-case? c)
  (let ((i (char->integer c)))
    (if (fx<=? i (char->integer #\delete))
        (fx<=? (char->integer #\A) i (char->integer #\Z))
        (char-set-contains? char-set:r6rs-upper-case c))))

(define (char-lower-case? c)
  (let ((i (char->integer c)))
    (if (fx<=? i (char->integer #\delete))
        (fx<=? (char->integer #\a) i (char->integer #\z))
        (char-set-contains? char-set:r6rs-lower-case c))))

(define (char-title-case? c)
  (char-set-contains? char-set:r6rs-title-case c))

;; char-general-category is automatically generated and can be found
;; later in this file.

;;; Unicode tables

(define (char-set-contains? cs c)
  (let ((ci (char->integer c)))
    (let lp ((low 0)
             (high (string-length cs)))
      (if (fx<? low high)
          (let ((middle (fxarithmetic-shift-right (fx+ low high) 1)))
            (if (fx>=? ci (char->integer (string-ref cs middle)))
                (lp (fx+ middle 1) high)
                (lp low middle)))
          (fxodd? high)))))

(define char-set:r6rs-alphabetic "\x41;\x5B;\x61;\x7B;\xAA;\xAB;\xB5;\xB6;\xBA;\xBB;\
\xC0;\xD7;\xD8;\xF7;\xF8;\x2C2;\x2C6;\x2D2;\x2E0;\x2E5;\
\x2EC;\x2ED;\x2EE;\x2EF;\x345;\x346;\x370;\x375;\x376;\x378;\
\x37A;\x37E;\x37F;\x380;\x386;\x387;\x388;\x38B;\x38C;\x38D;\
\x38E;\x3A2;\x3A3;\x3F6;\x3F7;\x482;\x48A;\x530;\x531;\x557;\
\x559;\x55A;\x561;\x588;\x5B0;\x5BE;\x5BF;\x5C0;\x5C1;\x5C3;\
\x5C4;\x5C6;\x5C7;\x5C8;\x5D0;\x5EB;\x5F0;\x5F3;\x610;\x61B;\
\x620;\x658;\x659;\x660;\x66E;\x6D4;\x6D5;\x6DD;\x6E1;\x6E9;\
\x6ED;\x6F0;\x6FA;\x6FD;\x6FF;\x700;\x710;\x740;\x74D;\x7B2;\
\x7CA;\x7EB;\x7F4;\x7F6;\x7FA;\x7FB;\x800;\x818;\x81A;\x82D;\
\x840;\x859;\x860;\x86B;\x8A0;\x8B5;\x8B6;\x8BE;\x8D4;\x8E0;\
\x8E3;\x8EA;\x8F0;\x93C;\x93D;\x94D;\x94E;\x951;\x955;\x964;\
\x971;\x984;\x985;\x98D;\x98F;\x991;\x993;\x9A9;\x9AA;\x9B1;\
\x9B2;\x9B3;\x9B6;\x9BA;\x9BD;\x9C5;\x9C7;\x9C9;\x9CB;\x9CD;\
\x9CE;\x9CF;\x9D7;\x9D8;\x9DC;\x9DE;\x9DF;\x9E4;\x9F0;\x9F2;\
\x9FC;\x9FD;\xA01;\xA04;\xA05;\xA0B;\xA0F;\xA11;\xA13;\xA29;\
\xA2A;\xA31;\xA32;\xA34;\xA35;\xA37;\xA38;\xA3A;\xA3E;\xA43;\
\xA47;\xA49;\xA4B;\xA4D;\xA51;\xA52;\xA59;\xA5D;\xA5E;\xA5F;\
\xA70;\xA76;\xA81;\xA84;\xA85;\xA8E;\xA8F;\xA92;\xA93;\xAA9;\
\xAAA;\xAB1;\xAB2;\xAB4;\xAB5;\xABA;\xABD;\xAC6;\xAC7;\xACA;\
\xACB;\xACD;\xAD0;\xAD1;\xAE0;\xAE4;\xAF9;\xAFD;\xB01;\xB04;\
\xB05;\xB0D;\xB0F;\xB11;\xB13;\xB29;\xB2A;\xB31;\xB32;\xB34;\
\xB35;\xB3A;\xB3D;\xB45;\xB47;\xB49;\xB4B;\xB4D;\xB56;\xB58;\
\xB5C;\xB5E;\xB5F;\xB64;\xB71;\xB72;\xB82;\xB84;\xB85;\xB8B;\
\xB8E;\xB91;\xB92;\xB96;\xB99;\xB9B;\xB9C;\xB9D;\xB9E;\xBA0;\
\xBA3;\xBA5;\xBA8;\xBAB;\xBAE;\xBBA;\xBBE;\xBC3;\xBC6;\xBC9;\
\xBCA;\xBCD;\xBD0;\xBD1;\xBD7;\xBD8;\xC00;\xC04;\xC05;\xC0D;\
\xC0E;\xC11;\xC12;\xC29;\xC2A;\xC3A;\xC3D;\xC45;\xC46;\xC49;\
\xC4A;\xC4D;\xC55;\xC57;\xC58;\xC5B;\xC60;\xC64;\xC80;\xC84;\
\xC85;\xC8D;\xC8E;\xC91;\xC92;\xCA9;\xCAA;\xCB4;\xCB5;\xCBA;\
\xCBD;\xCC5;\xCC6;\xCC9;\xCCA;\xCCD;\xCD5;\xCD7;\xCDE;\xCDF;\
\xCE0;\xCE4;\xCF1;\xCF3;\xD00;\xD04;\xD05;\xD0D;\xD0E;\xD11;\
\xD12;\xD3B;\xD3D;\xD45;\xD46;\xD49;\xD4A;\xD4D;\xD4E;\xD4F;\
\xD54;\xD58;\xD5F;\xD64;\xD7A;\xD80;\xD82;\xD84;\xD85;\xD97;\
\xD9A;\xDB2;\xDB3;\xDBC;\xDBD;\xDBE;\xDC0;\xDC7;\xDCF;\xDD5;\
\xDD6;\xDD7;\xDD8;\xDE0;\xDF2;\xDF4;\xE01;\xE3B;\xE40;\xE47;\
\xE4D;\xE4E;\xE81;\xE83;\xE84;\xE85;\xE87;\xE89;\xE8A;\xE8B;\
\xE8D;\xE8E;\xE94;\xE98;\xE99;\xEA0;\xEA1;\xEA4;\xEA5;\xEA6;\
\xEA7;\xEA8;\xEAA;\xEAC;\xEAD;\xEBA;\xEBB;\xEBE;\xEC0;\xEC5;\
\xEC6;\xEC7;\xECD;\xECE;\xEDC;\xEE0;\xF00;\xF01;\xF40;\xF48;\
\xF49;\xF6D;\xF71;\xF82;\xF88;\xF98;\xF99;\xFBD;\x1000;\x1037;\
\x1038;\x1039;\x103B;\x1040;\x1050;\x1063;\x1065;\x1069;\x106E;\x1087;\
\x108E;\x108F;\x109C;\x109E;\x10A0;\x10C6;\x10C7;\x10C8;\x10CD;\x10CE;\
\x10D0;\x10FB;\x10FC;\x1249;\x124A;\x124E;\x1250;\x1257;\x1258;\x1259;\
\x125A;\x125E;\x1260;\x1289;\x128A;\x128E;\x1290;\x12B1;\x12B2;\x12B6;\
\x12B8;\x12BF;\x12C0;\x12C1;\x12C2;\x12C6;\x12C8;\x12D7;\x12D8;\x1311;\
\x1312;\x1316;\x1318;\x135B;\x135F;\x1360;\x1380;\x1390;\x13A0;\x13F6;\
\x13F8;\x13FE;\x1401;\x166D;\x166F;\x1680;\x1681;\x169B;\x16A0;\x16EB;\
\x16EE;\x16F9;\x1700;\x170D;\x170E;\x1714;\x1720;\x1734;\x1740;\x1754;\
\x1760;\x176D;\x176E;\x1771;\x1772;\x1774;\x1780;\x17B4;\x17B6;\x17C9;\
\x17D7;\x17D8;\x17DC;\x17DD;\x1820;\x1878;\x1880;\x18AB;\x18B0;\x18F6;\
\x1900;\x191F;\x1920;\x192C;\x1930;\x1939;\x1950;\x196E;\x1970;\x1975;\
\x1980;\x19AC;\x19B0;\x19CA;\x1A00;\x1A1C;\x1A20;\x1A5F;\x1A61;\x1A75;\
\x1AA7;\x1AA8;\x1B00;\x1B34;\x1B35;\x1B44;\x1B45;\x1B4C;\x1B80;\x1BAA;\
\x1BAC;\x1BB0;\x1BBA;\x1BE6;\x1BE7;\x1BF2;\x1C00;\x1C36;\x1C4D;\x1C50;\
\x1C5A;\x1C7E;\x1C80;\x1C89;\x1CE9;\x1CED;\x1CEE;\x1CF4;\x1CF5;\x1CF7;\
\x1D00;\x1DC0;\x1DE7;\x1DF5;\x1E00;\x1F16;\x1F18;\x1F1E;\x1F20;\x1F46;\
\x1F48;\x1F4E;\x1F50;\x1F58;\x1F59;\x1F5A;\x1F5B;\x1F5C;\x1F5D;\x1F5E;\
\x1F5F;\x1F7E;\x1F80;\x1FB5;\x1FB6;\x1FBD;\x1FBE;\x1FBF;\x1FC2;\x1FC5;\
\x1FC6;\x1FCD;\x1FD0;\x1FD4;\x1FD6;\x1FDC;\x1FE0;\x1FED;\x1FF2;\x1FF5;\
\x1FF6;\x1FFD;\x2071;\x2072;\x207F;\x2080;\x2090;\x209D;\x2102;\x2103;\
\x2107;\x2108;\x210A;\x2114;\x2115;\x2116;\x2119;\x211E;\x2124;\x2125;\
\x2126;\x2127;\x2128;\x2129;\x212A;\x212E;\x212F;\x213A;\x213C;\x2140;\
\x2145;\x214A;\x214E;\x214F;\x2160;\x2189;\x24B6;\x24EA;\x2C00;\x2C2F;\
\x2C30;\x2C5F;\x2C60;\x2CE5;\x2CEB;\x2CEF;\x2CF2;\x2CF4;\x2D00;\x2D26;\
\x2D27;\x2D28;\x2D2D;\x2D2E;\x2D30;\x2D68;\x2D6F;\x2D70;\x2D80;\x2D97;\
\x2DA0;\x2DA7;\x2DA8;\x2DAF;\x2DB0;\x2DB7;\x2DB8;\x2DBF;\x2DC0;\x2DC7;\
\x2DC8;\x2DCF;\x2DD0;\x2DD7;\x2DD8;\x2DDF;\x2DE0;\x2E00;\x2E2F;\x2E30;\
\x3005;\x3008;\x3021;\x302A;\x3031;\x3036;\x3038;\x303D;\x3041;\x3097;\
\x309D;\x30A0;\x30A1;\x30FB;\x30FC;\x3100;\x3105;\x312F;\x3131;\x318F;\
\x31A0;\x31BB;\x31F0;\x3200;\x3400;\x4DB6;\x4E00;\x9FEB;\xA000;\xA48D;\
\xA4D0;\xA4FE;\xA500;\xA60D;\xA610;\xA620;\xA62A;\xA62C;\xA640;\xA66F;\
\xA674;\xA67C;\xA67F;\xA6F0;\xA717;\xA720;\xA722;\xA789;\xA78B;\xA7AF;\
\xA7B0;\xA7B8;\xA7F7;\xA802;\xA803;\xA806;\xA807;\xA80B;\xA80C;\xA828;\
\xA840;\xA874;\xA880;\xA8C4;\xA8C5;\xA8C6;\xA8F2;\xA8F8;\xA8FB;\xA8FC;\
\xA8FD;\xA8FE;\xA90A;\xA92B;\xA930;\xA953;\xA960;\xA97D;\xA980;\xA9B3;\
\xA9B4;\xA9C0;\xA9CF;\xA9D0;\xA9E0;\xA9E5;\xA9E6;\xA9F0;\xA9FA;\xA9FF;\
\xAA00;\xAA37;\xAA40;\xAA4E;\xAA60;\xAA77;\xAA7A;\xAA7B;\xAA7E;\xAABF;\
\xAAC0;\xAAC1;\xAAC2;\xAAC3;\xAADB;\xAADE;\xAAE0;\xAAF0;\xAAF2;\xAAF6;\
\xAB01;\xAB07;\xAB09;\xAB0F;\xAB11;\xAB17;\xAB20;\xAB27;\xAB28;\xAB2F;\
\xAB30;\xAB5B;\xAB5C;\xAB66;\xAB70;\xABEB;\xAC00;\xD7A4;\xD7B0;\xD7C7;\
\xD7CB;\xD7FC;\xF900;\xFA6E;\xFA70;\xFADA;\xFB00;\xFB07;\xFB13;\xFB18;\
\xFB1D;\xFB29;\xFB2A;\xFB37;\xFB38;\xFB3D;\xFB3E;\xFB3F;\xFB40;\xFB42;\
\xFB43;\xFB45;\xFB46;\xFBB2;\xFBD3;\xFD3E;\xFD50;\xFD90;\xFD92;\xFDC8;\
\xFDF0;\xFDFC;\xFE70;\xFE75;\xFE76;\xFEFD;\xFF21;\xFF3B;\xFF41;\xFF5B;\
\xFF66;\xFFBF;\xFFC2;\xFFC8;\xFFCA;\xFFD0;\xFFD2;\xFFD8;\xFFDA;\xFFDD;\
\x10000;\x1000C;\x1000D;\x10027;\x10028;\x1003B;\x1003C;\x1003E;\x1003F;\x1004E;\
\x10050;\x1005E;\x10080;\x100FB;\x10140;\x10175;\x10280;\x1029D;\x102A0;\x102D1;\
\x10300;\x10320;\x1032D;\x1034B;\x10350;\x1037B;\x10380;\x1039E;\x103A0;\x103C4;\
\x103C8;\x103D0;\x103D1;\x103D6;\x10400;\x1049E;\x104B0;\x104D4;\x104D8;\x104FC;\
\x10500;\x10528;\x10530;\x10564;\x10600;\x10737;\x10740;\x10756;\x10760;\x10768;\
\x10800;\x10806;\x10808;\x10809;\x1080A;\x10836;\x10837;\x10839;\x1083C;\x1083D;\
\x1083F;\x10856;\x10860;\x10877;\x10880;\x1089F;\x108E0;\x108F3;\x108F4;\x108F6;\
\x10900;\x10916;\x10920;\x1093A;\x10980;\x109B8;\x109BE;\x109C0;\x10A00;\x10A04;\
\x10A05;\x10A07;\x10A0C;\x10A14;\x10A15;\x10A18;\x10A19;\x10A34;\x10A60;\x10A7D;\
\x10A80;\x10A9D;\x10AC0;\x10AC8;\x10AC9;\x10AE5;\x10B00;\x10B36;\x10B40;\x10B56;\
\x10B60;\x10B73;\x10B80;\x10B92;\x10C00;\x10C49;\x10C80;\x10CB3;\x10CC0;\x10CF3;\
\x11000;\x11046;\x11082;\x110B9;\x110D0;\x110E9;\x11100;\x11133;\x11150;\x11173;\
\x11176;\x11177;\x11180;\x111C0;\x111C1;\x111C5;\x111DA;\x111DB;\x111DC;\x111DD;\
\x11200;\x11212;\x11213;\x11235;\x11237;\x11238;\x1123E;\x1123F;\x11280;\x11287;\
\x11288;\x11289;\x1128A;\x1128E;\x1128F;\x1129E;\x1129F;\x112A9;\x112B0;\x112E9;\
\x11300;\x11304;\x11305;\x1130D;\x1130F;\x11311;\x11313;\x11329;\x1132A;\x11331;\
\x11332;\x11334;\x11335;\x1133A;\x1133D;\x11345;\x11347;\x11349;\x1134B;\x1134D;\
\x11350;\x11351;\x11357;\x11358;\x1135D;\x11364;\x11400;\x11442;\x11443;\x11446;\
\x11447;\x1144B;\x11480;\x114C2;\x114C4;\x114C6;\x114C7;\x114C8;\x11580;\x115B6;\
\x115B8;\x115BF;\x115D8;\x115DE;\x11600;\x1163F;\x11640;\x11641;\x11644;\x11645;\
\x11680;\x116B6;\x11700;\x1171A;\x1171D;\x1172B;\x118A0;\x118E0;\x118FF;\x11900;\
\x11A00;\x11A33;\x11A35;\x11A3F;\x11A50;\x11A84;\x11A86;\x11A98;\x11AC0;\x11AF9;\
\x11C00;\x11C09;\x11C0A;\x11C37;\x11C38;\x11C3F;\x11C40;\x11C41;\x11C72;\x11C90;\
\x11C92;\x11CA8;\x11CA9;\x11CB7;\x11D00;\x11D07;\x11D08;\x11D0A;\x11D0B;\x11D37;\
\x11D3A;\x11D3B;\x11D3C;\x11D3E;\x11D3F;\x11D42;\x11D43;\x11D44;\x11D46;\x11D48;\
\x12000;\x1239A;\x12400;\x1246F;\x12480;\x12544;\x13000;\x1342F;\x14400;\x14647;\
\x16800;\x16A39;\x16A40;\x16A5F;\x16AD0;\x16AEE;\x16B00;\x16B37;\x16B40;\x16B44;\
\x16B63;\x16B78;\x16B7D;\x16B90;\x16F00;\x16F45;\x16F50;\x16F7F;\x16F93;\x16FA0;\
\x16FE0;\x16FE2;\x17000;\x187ED;\x18800;\x18AF3;\x1B000;\x1B11F;\x1B170;\x1B2FC;\
\x1BC00;\x1BC6B;\x1BC70;\x1BC7D;\x1BC80;\x1BC89;\x1BC90;\x1BC9A;\x1BC9E;\x1BC9F;\
\x1D400;\x1D455;\x1D456;\x1D49D;\x1D49E;\x1D4A0;\x1D4A2;\x1D4A3;\x1D4A5;\x1D4A7;\
\x1D4A9;\x1D4AD;\x1D4AE;\x1D4BA;\x1D4BB;\x1D4BC;\x1D4BD;\x1D4C4;\x1D4C5;\x1D506;\
\x1D507;\x1D50B;\x1D50D;\x1D515;\x1D516;\x1D51D;\x1D51E;\x1D53A;\x1D53B;\x1D53F;\
\x1D540;\x1D545;\x1D546;\x1D547;\x1D54A;\x1D551;\x1D552;\x1D6A6;\x1D6A8;\x1D6C1;\
\x1D6C2;\x1D6DB;\x1D6DC;\x1D6FB;\x1D6FC;\x1D715;\x1D716;\x1D735;\x1D736;\x1D74F;\
\x1D750;\x1D76F;\x1D770;\x1D789;\x1D78A;\x1D7A9;\x1D7AA;\x1D7C3;\x1D7C4;\x1D7CC;\
\x1E000;\x1E007;\x1E008;\x1E019;\x1E01B;\x1E022;\x1E023;\x1E025;\x1E026;\x1E02B;\
\x1E800;\x1E8C5;\x1E900;\x1E944;\x1E947;\x1E948;\x1EE00;\x1EE04;\x1EE05;\x1EE20;\
\x1EE21;\x1EE23;\x1EE24;\x1EE25;\x1EE27;\x1EE28;\x1EE29;\x1EE33;\x1EE34;\x1EE38;\
\x1EE39;\x1EE3A;\x1EE3B;\x1EE3C;\x1EE42;\x1EE43;\x1EE47;\x1EE48;\x1EE49;\x1EE4A;\
\x1EE4B;\x1EE4C;\x1EE4D;\x1EE50;\x1EE51;\x1EE53;\x1EE54;\x1EE55;\x1EE57;\x1EE58;\
\x1EE59;\x1EE5A;\x1EE5B;\x1EE5C;\x1EE5D;\x1EE5E;\x1EE5F;\x1EE60;\x1EE61;\x1EE63;\
\x1EE64;\x1EE65;\x1EE67;\x1EE6B;\x1EE6C;\x1EE73;\x1EE74;\x1EE78;\x1EE79;\x1EE7D;\
\x1EE7E;\x1EE7F;\x1EE80;\x1EE8A;\x1EE8B;\x1EE9C;\x1EEA1;\x1EEA4;\x1EEA5;\x1EEAA;\
\x1EEAB;\x1EEBC;\x1F130;\x1F14A;\x1F150;\x1F16A;\x1F170;\x1F18A;\x20000;\x2A6D7;\
\x2A700;\x2B735;\x2B740;\x2B81E;\x2B820;\x2CEA2;\x2CEB0;\x2EBE1;\x2F800;\x2FA1E;\
")

(define char-set:r6rs-lower-case "\x61;\x7B;\xAA;\xAB;\xB5;\xB6;\xBA;\xBB;\xDF;\xF7;\
\xF8;\x100;\x101;\x102;\x103;\x104;\x105;\x106;\x107;\x108;\
\x109;\x10A;\x10B;\x10C;\x10D;\x10E;\x10F;\x110;\x111;\x112;\
\x113;\x114;\x115;\x116;\x117;\x118;\x119;\x11A;\x11B;\x11C;\
\x11D;\x11E;\x11F;\x120;\x121;\x122;\x123;\x124;\x125;\x126;\
\x127;\x128;\x129;\x12A;\x12B;\x12C;\x12D;\x12E;\x12F;\x130;\
\x131;\x132;\x133;\x134;\x135;\x136;\x137;\x139;\x13A;\x13B;\
\x13C;\x13D;\x13E;\x13F;\x140;\x141;\x142;\x143;\x144;\x145;\
\x146;\x147;\x148;\x14A;\x14B;\x14C;\x14D;\x14E;\x14F;\x150;\
\x151;\x152;\x153;\x154;\x155;\x156;\x157;\x158;\x159;\x15A;\
\x15B;\x15C;\x15D;\x15E;\x15F;\x160;\x161;\x162;\x163;\x164;\
\x165;\x166;\x167;\x168;\x169;\x16A;\x16B;\x16C;\x16D;\x16E;\
\x16F;\x170;\x171;\x172;\x173;\x174;\x175;\x176;\x177;\x178;\
\x17A;\x17B;\x17C;\x17D;\x17E;\x181;\x183;\x184;\x185;\x186;\
\x188;\x189;\x18C;\x18E;\x192;\x193;\x195;\x196;\x199;\x19C;\
\x19E;\x19F;\x1A1;\x1A2;\x1A3;\x1A4;\x1A5;\x1A6;\x1A8;\x1A9;\
\x1AA;\x1AC;\x1AD;\x1AE;\x1B0;\x1B1;\x1B4;\x1B5;\x1B6;\x1B7;\
\x1B9;\x1BB;\x1BD;\x1C0;\x1C6;\x1C7;\x1C9;\x1CA;\x1CC;\x1CD;\
\x1CE;\x1CF;\x1D0;\x1D1;\x1D2;\x1D3;\x1D4;\x1D5;\x1D6;\x1D7;\
\x1D8;\x1D9;\x1DA;\x1DB;\x1DC;\x1DE;\x1DF;\x1E0;\x1E1;\x1E2;\
\x1E3;\x1E4;\x1E5;\x1E6;\x1E7;\x1E8;\x1E9;\x1EA;\x1EB;\x1EC;\
\x1ED;\x1EE;\x1EF;\x1F1;\x1F3;\x1F4;\x1F5;\x1F6;\x1F9;\x1FA;\
\x1FB;\x1FC;\x1FD;\x1FE;\x1FF;\x200;\x201;\x202;\x203;\x204;\
\x205;\x206;\x207;\x208;\x209;\x20A;\x20B;\x20C;\x20D;\x20E;\
\x20F;\x210;\x211;\x212;\x213;\x214;\x215;\x216;\x217;\x218;\
\x219;\x21A;\x21B;\x21C;\x21D;\x21E;\x21F;\x220;\x221;\x222;\
\x223;\x224;\x225;\x226;\x227;\x228;\x229;\x22A;\x22B;\x22C;\
\x22D;\x22E;\x22F;\x230;\x231;\x232;\x233;\x23A;\x23C;\x23D;\
\x23F;\x241;\x242;\x243;\x247;\x248;\x249;\x24A;\x24B;\x24C;\
\x24D;\x24E;\x24F;\x294;\x295;\x2B9;\x2C0;\x2C2;\x2E0;\x2E5;\
\x345;\x346;\x371;\x372;\x373;\x374;\x377;\x378;\x37A;\x37E;\
\x390;\x391;\x3AC;\x3CF;\x3D0;\x3D2;\x3D5;\x3D8;\x3D9;\x3DA;\
\x3DB;\x3DC;\x3DD;\x3DE;\x3DF;\x3E0;\x3E1;\x3E2;\x3E3;\x3E4;\
\x3E5;\x3E6;\x3E7;\x3E8;\x3E9;\x3EA;\x3EB;\x3EC;\x3ED;\x3EE;\
\x3EF;\x3F4;\x3F5;\x3F6;\x3F8;\x3F9;\x3FB;\x3FD;\x430;\x460;\
\x461;\x462;\x463;\x464;\x465;\x466;\x467;\x468;\x469;\x46A;\
\x46B;\x46C;\x46D;\x46E;\x46F;\x470;\x471;\x472;\x473;\x474;\
\x475;\x476;\x477;\x478;\x479;\x47A;\x47B;\x47C;\x47D;\x47E;\
\x47F;\x480;\x481;\x482;\x48B;\x48C;\x48D;\x48E;\x48F;\x490;\
\x491;\x492;\x493;\x494;\x495;\x496;\x497;\x498;\x499;\x49A;\
\x49B;\x49C;\x49D;\x49E;\x49F;\x4A0;\x4A1;\x4A2;\x4A3;\x4A4;\
\x4A5;\x4A6;\x4A7;\x4A8;\x4A9;\x4AA;\x4AB;\x4AC;\x4AD;\x4AE;\
\x4AF;\x4B0;\x4B1;\x4B2;\x4B3;\x4B4;\x4B5;\x4B6;\x4B7;\x4B8;\
\x4B9;\x4BA;\x4BB;\x4BC;\x4BD;\x4BE;\x4BF;\x4C0;\x4C2;\x4C3;\
\x4C4;\x4C5;\x4C6;\x4C7;\x4C8;\x4C9;\x4CA;\x4CB;\x4CC;\x4CD;\
\x4CE;\x4D0;\x4D1;\x4D2;\x4D3;\x4D4;\x4D5;\x4D6;\x4D7;\x4D8;\
\x4D9;\x4DA;\x4DB;\x4DC;\x4DD;\x4DE;\x4DF;\x4E0;\x4E1;\x4E2;\
\x4E3;\x4E4;\x4E5;\x4E6;\x4E7;\x4E8;\x4E9;\x4EA;\x4EB;\x4EC;\
\x4ED;\x4EE;\x4EF;\x4F0;\x4F1;\x4F2;\x4F3;\x4F4;\x4F5;\x4F6;\
\x4F7;\x4F8;\x4F9;\x4FA;\x4FB;\x4FC;\x4FD;\x4FE;\x4FF;\x500;\
\x501;\x502;\x503;\x504;\x505;\x506;\x507;\x508;\x509;\x50A;\
\x50B;\x50C;\x50D;\x50E;\x50F;\x510;\x511;\x512;\x513;\x514;\
\x515;\x516;\x517;\x518;\x519;\x51A;\x51B;\x51C;\x51D;\x51E;\
\x51F;\x520;\x521;\x522;\x523;\x524;\x525;\x526;\x527;\x528;\
\x529;\x52A;\x52B;\x52C;\x52D;\x52E;\x52F;\x530;\x561;\x588;\
\x13F8;\x13FE;\x1C80;\x1C89;\x1D00;\x1DC0;\x1E01;\x1E02;\x1E03;\x1E04;\
\x1E05;\x1E06;\x1E07;\x1E08;\x1E09;\x1E0A;\x1E0B;\x1E0C;\x1E0D;\x1E0E;\
\x1E0F;\x1E10;\x1E11;\x1E12;\x1E13;\x1E14;\x1E15;\x1E16;\x1E17;\x1E18;\
\x1E19;\x1E1A;\x1E1B;\x1E1C;\x1E1D;\x1E1E;\x1E1F;\x1E20;\x1E21;\x1E22;\
\x1E23;\x1E24;\x1E25;\x1E26;\x1E27;\x1E28;\x1E29;\x1E2A;\x1E2B;\x1E2C;\
\x1E2D;\x1E2E;\x1E2F;\x1E30;\x1E31;\x1E32;\x1E33;\x1E34;\x1E35;\x1E36;\
\x1E37;\x1E38;\x1E39;\x1E3A;\x1E3B;\x1E3C;\x1E3D;\x1E3E;\x1E3F;\x1E40;\
\x1E41;\x1E42;\x1E43;\x1E44;\x1E45;\x1E46;\x1E47;\x1E48;\x1E49;\x1E4A;\
\x1E4B;\x1E4C;\x1E4D;\x1E4E;\x1E4F;\x1E50;\x1E51;\x1E52;\x1E53;\x1E54;\
\x1E55;\x1E56;\x1E57;\x1E58;\x1E59;\x1E5A;\x1E5B;\x1E5C;\x1E5D;\x1E5E;\
\x1E5F;\x1E60;\x1E61;\x1E62;\x1E63;\x1E64;\x1E65;\x1E66;\x1E67;\x1E68;\
\x1E69;\x1E6A;\x1E6B;\x1E6C;\x1E6D;\x1E6E;\x1E6F;\x1E70;\x1E71;\x1E72;\
\x1E73;\x1E74;\x1E75;\x1E76;\x1E77;\x1E78;\x1E79;\x1E7A;\x1E7B;\x1E7C;\
\x1E7D;\x1E7E;\x1E7F;\x1E80;\x1E81;\x1E82;\x1E83;\x1E84;\x1E85;\x1E86;\
\x1E87;\x1E88;\x1E89;\x1E8A;\x1E8B;\x1E8C;\x1E8D;\x1E8E;\x1E8F;\x1E90;\
\x1E91;\x1E92;\x1E93;\x1E94;\x1E95;\x1E9E;\x1E9F;\x1EA0;\x1EA1;\x1EA2;\
\x1EA3;\x1EA4;\x1EA5;\x1EA6;\x1EA7;\x1EA8;\x1EA9;\x1EAA;\x1EAB;\x1EAC;\
\x1EAD;\x1EAE;\x1EAF;\x1EB0;\x1EB1;\x1EB2;\x1EB3;\x1EB4;\x1EB5;\x1EB6;\
\x1EB7;\x1EB8;\x1EB9;\x1EBA;\x1EBB;\x1EBC;\x1EBD;\x1EBE;\x1EBF;\x1EC0;\
\x1EC1;\x1EC2;\x1EC3;\x1EC4;\x1EC5;\x1EC6;\x1EC7;\x1EC8;\x1EC9;\x1ECA;\
\x1ECB;\x1ECC;\x1ECD;\x1ECE;\x1ECF;\x1ED0;\x1ED1;\x1ED2;\x1ED3;\x1ED4;\
\x1ED5;\x1ED6;\x1ED7;\x1ED8;\x1ED9;\x1EDA;\x1EDB;\x1EDC;\x1EDD;\x1EDE;\
\x1EDF;\x1EE0;\x1EE1;\x1EE2;\x1EE3;\x1EE4;\x1EE5;\x1EE6;\x1EE7;\x1EE8;\
\x1EE9;\x1EEA;\x1EEB;\x1EEC;\x1EED;\x1EEE;\x1EEF;\x1EF0;\x1EF1;\x1EF2;\
\x1EF3;\x1EF4;\x1EF5;\x1EF6;\x1EF7;\x1EF8;\x1EF9;\x1EFA;\x1EFB;\x1EFC;\
\x1EFD;\x1EFE;\x1EFF;\x1F08;\x1F10;\x1F16;\x1F20;\x1F28;\x1F30;\x1F38;\
\x1F40;\x1F46;\x1F50;\x1F58;\x1F60;\x1F68;\x1F70;\x1F7E;\x1F80;\x1F88;\
\x1F90;\x1F98;\x1FA0;\x1FA8;\x1FB0;\x1FB5;\x1FB6;\x1FB8;\x1FBE;\x1FBF;\
\x1FC2;\x1FC5;\x1FC6;\x1FC8;\x1FD0;\x1FD4;\x1FD6;\x1FD8;\x1FE0;\x1FE8;\
\x1FF2;\x1FF5;\x1FF6;\x1FF8;\x2071;\x2072;\x207F;\x2080;\x2090;\x209D;\
\x210A;\x210B;\x210E;\x2110;\x2113;\x2114;\x212F;\x2130;\x2134;\x2135;\
\x2139;\x213A;\x213C;\x213E;\x2146;\x214A;\x214E;\x214F;\x2170;\x2180;\
\x2184;\x2185;\x24D0;\x24EA;\x2C30;\x2C5F;\x2C61;\x2C62;\x2C65;\x2C67;\
\x2C68;\x2C69;\x2C6A;\x2C6B;\x2C6C;\x2C6D;\x2C71;\x2C72;\x2C73;\x2C75;\
\x2C76;\x2C7E;\x2C81;\x2C82;\x2C83;\x2C84;\x2C85;\x2C86;\x2C87;\x2C88;\
\x2C89;\x2C8A;\x2C8B;\x2C8C;\x2C8D;\x2C8E;\x2C8F;\x2C90;\x2C91;\x2C92;\
\x2C93;\x2C94;\x2C95;\x2C96;\x2C97;\x2C98;\x2C99;\x2C9A;\x2C9B;\x2C9C;\
\x2C9D;\x2C9E;\x2C9F;\x2CA0;\x2CA1;\x2CA2;\x2CA3;\x2CA4;\x2CA5;\x2CA6;\
\x2CA7;\x2CA8;\x2CA9;\x2CAA;\x2CAB;\x2CAC;\x2CAD;\x2CAE;\x2CAF;\x2CB0;\
\x2CB1;\x2CB2;\x2CB3;\x2CB4;\x2CB5;\x2CB6;\x2CB7;\x2CB8;\x2CB9;\x2CBA;\
\x2CBB;\x2CBC;\x2CBD;\x2CBE;\x2CBF;\x2CC0;\x2CC1;\x2CC2;\x2CC3;\x2CC4;\
\x2CC5;\x2CC6;\x2CC7;\x2CC8;\x2CC9;\x2CCA;\x2CCB;\x2CCC;\x2CCD;\x2CCE;\
\x2CCF;\x2CD0;\x2CD1;\x2CD2;\x2CD3;\x2CD4;\x2CD5;\x2CD6;\x2CD7;\x2CD8;\
\x2CD9;\x2CDA;\x2CDB;\x2CDC;\x2CDD;\x2CDE;\x2CDF;\x2CE0;\x2CE1;\x2CE2;\
\x2CE3;\x2CE5;\x2CEC;\x2CED;\x2CEE;\x2CEF;\x2CF3;\x2CF4;\x2D00;\x2D26;\
\x2D27;\x2D28;\x2D2D;\x2D2E;\xA641;\xA642;\xA643;\xA644;\xA645;\xA646;\
\xA647;\xA648;\xA649;\xA64A;\xA64B;\xA64C;\xA64D;\xA64E;\xA64F;\xA650;\
\xA651;\xA652;\xA653;\xA654;\xA655;\xA656;\xA657;\xA658;\xA659;\xA65A;\
\xA65B;\xA65C;\xA65D;\xA65E;\xA65F;\xA660;\xA661;\xA662;\xA663;\xA664;\
\xA665;\xA666;\xA667;\xA668;\xA669;\xA66A;\xA66B;\xA66C;\xA66D;\xA66E;\
\xA681;\xA682;\xA683;\xA684;\xA685;\xA686;\xA687;\xA688;\xA689;\xA68A;\
\xA68B;\xA68C;\xA68D;\xA68E;\xA68F;\xA690;\xA691;\xA692;\xA693;\xA694;\
\xA695;\xA696;\xA697;\xA698;\xA699;\xA69A;\xA69B;\xA69E;\xA723;\xA724;\
\xA725;\xA726;\xA727;\xA728;\xA729;\xA72A;\xA72B;\xA72C;\xA72D;\xA72E;\
\xA72F;\xA732;\xA733;\xA734;\xA735;\xA736;\xA737;\xA738;\xA739;\xA73A;\
\xA73B;\xA73C;\xA73D;\xA73E;\xA73F;\xA740;\xA741;\xA742;\xA743;\xA744;\
\xA745;\xA746;\xA747;\xA748;\xA749;\xA74A;\xA74B;\xA74C;\xA74D;\xA74E;\
\xA74F;\xA750;\xA751;\xA752;\xA753;\xA754;\xA755;\xA756;\xA757;\xA758;\
\xA759;\xA75A;\xA75B;\xA75C;\xA75D;\xA75E;\xA75F;\xA760;\xA761;\xA762;\
\xA763;\xA764;\xA765;\xA766;\xA767;\xA768;\xA769;\xA76A;\xA76B;\xA76C;\
\xA76D;\xA76E;\xA76F;\xA779;\xA77A;\xA77B;\xA77C;\xA77D;\xA77F;\xA780;\
\xA781;\xA782;\xA783;\xA784;\xA785;\xA786;\xA787;\xA788;\xA78C;\xA78D;\
\xA78E;\xA78F;\xA791;\xA792;\xA793;\xA796;\xA797;\xA798;\xA799;\xA79A;\
\xA79B;\xA79C;\xA79D;\xA79E;\xA79F;\xA7A0;\xA7A1;\xA7A2;\xA7A3;\xA7A4;\
\xA7A5;\xA7A6;\xA7A7;\xA7A8;\xA7A9;\xA7AA;\xA7B5;\xA7B6;\xA7B7;\xA7B8;\
\xA7F8;\xA7FB;\xAB30;\xAB5B;\xAB5C;\xAB66;\xAB70;\xABC0;\xFB00;\xFB07;\
\xFB13;\xFB18;\xFF41;\xFF5B;\x10428;\x10450;\x104D8;\x104FC;\x10CC0;\x10CF3;\
\x118C0;\x118E0;\x1D41A;\x1D434;\x1D44E;\x1D455;\x1D456;\x1D468;\x1D482;\x1D49C;\
\x1D4B6;\x1D4BA;\x1D4BB;\x1D4BC;\x1D4BD;\x1D4C4;\x1D4C5;\x1D4D0;\x1D4EA;\x1D504;\
\x1D51E;\x1D538;\x1D552;\x1D56C;\x1D586;\x1D5A0;\x1D5BA;\x1D5D4;\x1D5EE;\x1D608;\
\x1D622;\x1D63C;\x1D656;\x1D670;\x1D68A;\x1D6A6;\x1D6C2;\x1D6DB;\x1D6DC;\x1D6E2;\
\x1D6FC;\x1D715;\x1D716;\x1D71C;\x1D736;\x1D74F;\x1D750;\x1D756;\x1D770;\x1D789;\
\x1D78A;\x1D790;\x1D7AA;\x1D7C3;\x1D7C4;\x1D7CA;\x1D7CB;\x1D7CC;\x1E922;\x1E944;\
")

(define char-set:r6rs-upper-case "\x41;\x5B;\xC0;\xD7;\xD8;\xDF;\x100;\x101;\x102;\x103;\
\x104;\x105;\x106;\x107;\x108;\x109;\x10A;\x10B;\x10C;\x10D;\
\x10E;\x10F;\x110;\x111;\x112;\x113;\x114;\x115;\x116;\x117;\
\x118;\x119;\x11A;\x11B;\x11C;\x11D;\x11E;\x11F;\x120;\x121;\
\x122;\x123;\x124;\x125;\x126;\x127;\x128;\x129;\x12A;\x12B;\
\x12C;\x12D;\x12E;\x12F;\x130;\x131;\x132;\x133;\x134;\x135;\
\x136;\x137;\x139;\x13A;\x13B;\x13C;\x13D;\x13E;\x13F;\x140;\
\x141;\x142;\x143;\x144;\x145;\x146;\x147;\x148;\x14A;\x14B;\
\x14C;\x14D;\x14E;\x14F;\x150;\x151;\x152;\x153;\x154;\x155;\
\x156;\x157;\x158;\x159;\x15A;\x15B;\x15C;\x15D;\x15E;\x15F;\
\x160;\x161;\x162;\x163;\x164;\x165;\x166;\x167;\x168;\x169;\
\x16A;\x16B;\x16C;\x16D;\x16E;\x16F;\x170;\x171;\x172;\x173;\
\x174;\x175;\x176;\x177;\x178;\x17A;\x17B;\x17C;\x17D;\x17E;\
\x181;\x183;\x184;\x185;\x186;\x188;\x189;\x18C;\x18E;\x192;\
\x193;\x195;\x196;\x199;\x19C;\x19E;\x19F;\x1A1;\x1A2;\x1A3;\
\x1A4;\x1A5;\x1A6;\x1A8;\x1A9;\x1AA;\x1AC;\x1AD;\x1AE;\x1B0;\
\x1B1;\x1B4;\x1B5;\x1B6;\x1B7;\x1B9;\x1BC;\x1BD;\x1C4;\x1C5;\
\x1C7;\x1C8;\x1CA;\x1CB;\x1CD;\x1CE;\x1CF;\x1D0;\x1D1;\x1D2;\
\x1D3;\x1D4;\x1D5;\x1D6;\x1D7;\x1D8;\x1D9;\x1DA;\x1DB;\x1DC;\
\x1DE;\x1DF;\x1E0;\x1E1;\x1E2;\x1E3;\x1E4;\x1E5;\x1E6;\x1E7;\
\x1E8;\x1E9;\x1EA;\x1EB;\x1EC;\x1ED;\x1EE;\x1EF;\x1F1;\x1F2;\
\x1F4;\x1F5;\x1F6;\x1F9;\x1FA;\x1FB;\x1FC;\x1FD;\x1FE;\x1FF;\
\x200;\x201;\x202;\x203;\x204;\x205;\x206;\x207;\x208;\x209;\
\x20A;\x20B;\x20C;\x20D;\x20E;\x20F;\x210;\x211;\x212;\x213;\
\x214;\x215;\x216;\x217;\x218;\x219;\x21A;\x21B;\x21C;\x21D;\
\x21E;\x21F;\x220;\x221;\x222;\x223;\x224;\x225;\x226;\x227;\
\x228;\x229;\x22A;\x22B;\x22C;\x22D;\x22E;\x22F;\x230;\x231;\
\x232;\x233;\x23A;\x23C;\x23D;\x23F;\x241;\x242;\x243;\x247;\
\x248;\x249;\x24A;\x24B;\x24C;\x24D;\x24E;\x24F;\x370;\x371;\
\x372;\x373;\x376;\x377;\x37F;\x380;\x386;\x387;\x388;\x38B;\
\x38C;\x38D;\x38E;\x390;\x391;\x3A2;\x3A3;\x3AC;\x3CF;\x3D0;\
\x3D2;\x3D5;\x3D8;\x3D9;\x3DA;\x3DB;\x3DC;\x3DD;\x3DE;\x3DF;\
\x3E0;\x3E1;\x3E2;\x3E3;\x3E4;\x3E5;\x3E6;\x3E7;\x3E8;\x3E9;\
\x3EA;\x3EB;\x3EC;\x3ED;\x3EE;\x3EF;\x3F4;\x3F5;\x3F7;\x3F8;\
\x3F9;\x3FB;\x3FD;\x430;\x460;\x461;\x462;\x463;\x464;\x465;\
\x466;\x467;\x468;\x469;\x46A;\x46B;\x46C;\x46D;\x46E;\x46F;\
\x470;\x471;\x472;\x473;\x474;\x475;\x476;\x477;\x478;\x479;\
\x47A;\x47B;\x47C;\x47D;\x47E;\x47F;\x480;\x481;\x48A;\x48B;\
\x48C;\x48D;\x48E;\x48F;\x490;\x491;\x492;\x493;\x494;\x495;\
\x496;\x497;\x498;\x499;\x49A;\x49B;\x49C;\x49D;\x49E;\x49F;\
\x4A0;\x4A1;\x4A2;\x4A3;\x4A4;\x4A5;\x4A6;\x4A7;\x4A8;\x4A9;\
\x4AA;\x4AB;\x4AC;\x4AD;\x4AE;\x4AF;\x4B0;\x4B1;\x4B2;\x4B3;\
\x4B4;\x4B5;\x4B6;\x4B7;\x4B8;\x4B9;\x4BA;\x4BB;\x4BC;\x4BD;\
\x4BE;\x4BF;\x4C0;\x4C2;\x4C3;\x4C4;\x4C5;\x4C6;\x4C7;\x4C8;\
\x4C9;\x4CA;\x4CB;\x4CC;\x4CD;\x4CE;\x4D0;\x4D1;\x4D2;\x4D3;\
\x4D4;\x4D5;\x4D6;\x4D7;\x4D8;\x4D9;\x4DA;\x4DB;\x4DC;\x4DD;\
\x4DE;\x4DF;\x4E0;\x4E1;\x4E2;\x4E3;\x4E4;\x4E5;\x4E6;\x4E7;\
\x4E8;\x4E9;\x4EA;\x4EB;\x4EC;\x4ED;\x4EE;\x4EF;\x4F0;\x4F1;\
\x4F2;\x4F3;\x4F4;\x4F5;\x4F6;\x4F7;\x4F8;\x4F9;\x4FA;\x4FB;\
\x4FC;\x4FD;\x4FE;\x4FF;\x500;\x501;\x502;\x503;\x504;\x505;\
\x506;\x507;\x508;\x509;\x50A;\x50B;\x50C;\x50D;\x50E;\x50F;\
\x510;\x511;\x512;\x513;\x514;\x515;\x516;\x517;\x518;\x519;\
\x51A;\x51B;\x51C;\x51D;\x51E;\x51F;\x520;\x521;\x522;\x523;\
\x524;\x525;\x526;\x527;\x528;\x529;\x52A;\x52B;\x52C;\x52D;\
\x52E;\x52F;\x531;\x557;\x10A0;\x10C6;\x10C7;\x10C8;\x10CD;\x10CE;\
\x13A0;\x13F6;\x1E00;\x1E01;\x1E02;\x1E03;\x1E04;\x1E05;\x1E06;\x1E07;\
\x1E08;\x1E09;\x1E0A;\x1E0B;\x1E0C;\x1E0D;\x1E0E;\x1E0F;\x1E10;\x1E11;\
\x1E12;\x1E13;\x1E14;\x1E15;\x1E16;\x1E17;\x1E18;\x1E19;\x1E1A;\x1E1B;\
\x1E1C;\x1E1D;\x1E1E;\x1E1F;\x1E20;\x1E21;\x1E22;\x1E23;\x1E24;\x1E25;\
\x1E26;\x1E27;\x1E28;\x1E29;\x1E2A;\x1E2B;\x1E2C;\x1E2D;\x1E2E;\x1E2F;\
\x1E30;\x1E31;\x1E32;\x1E33;\x1E34;\x1E35;\x1E36;\x1E37;\x1E38;\x1E39;\
\x1E3A;\x1E3B;\x1E3C;\x1E3D;\x1E3E;\x1E3F;\x1E40;\x1E41;\x1E42;\x1E43;\
\x1E44;\x1E45;\x1E46;\x1E47;\x1E48;\x1E49;\x1E4A;\x1E4B;\x1E4C;\x1E4D;\
\x1E4E;\x1E4F;\x1E50;\x1E51;\x1E52;\x1E53;\x1E54;\x1E55;\x1E56;\x1E57;\
\x1E58;\x1E59;\x1E5A;\x1E5B;\x1E5C;\x1E5D;\x1E5E;\x1E5F;\x1E60;\x1E61;\
\x1E62;\x1E63;\x1E64;\x1E65;\x1E66;\x1E67;\x1E68;\x1E69;\x1E6A;\x1E6B;\
\x1E6C;\x1E6D;\x1E6E;\x1E6F;\x1E70;\x1E71;\x1E72;\x1E73;\x1E74;\x1E75;\
\x1E76;\x1E77;\x1E78;\x1E79;\x1E7A;\x1E7B;\x1E7C;\x1E7D;\x1E7E;\x1E7F;\
\x1E80;\x1E81;\x1E82;\x1E83;\x1E84;\x1E85;\x1E86;\x1E87;\x1E88;\x1E89;\
\x1E8A;\x1E8B;\x1E8C;\x1E8D;\x1E8E;\x1E8F;\x1E90;\x1E91;\x1E92;\x1E93;\
\x1E94;\x1E95;\x1E9E;\x1E9F;\x1EA0;\x1EA1;\x1EA2;\x1EA3;\x1EA4;\x1EA5;\
\x1EA6;\x1EA7;\x1EA8;\x1EA9;\x1EAA;\x1EAB;\x1EAC;\x1EAD;\x1EAE;\x1EAF;\
\x1EB0;\x1EB1;\x1EB2;\x1EB3;\x1EB4;\x1EB5;\x1EB6;\x1EB7;\x1EB8;\x1EB9;\
\x1EBA;\x1EBB;\x1EBC;\x1EBD;\x1EBE;\x1EBF;\x1EC0;\x1EC1;\x1EC2;\x1EC3;\
\x1EC4;\x1EC5;\x1EC6;\x1EC7;\x1EC8;\x1EC9;\x1ECA;\x1ECB;\x1ECC;\x1ECD;\
\x1ECE;\x1ECF;\x1ED0;\x1ED1;\x1ED2;\x1ED3;\x1ED4;\x1ED5;\x1ED6;\x1ED7;\
\x1ED8;\x1ED9;\x1EDA;\x1EDB;\x1EDC;\x1EDD;\x1EDE;\x1EDF;\x1EE0;\x1EE1;\
\x1EE2;\x1EE3;\x1EE4;\x1EE5;\x1EE6;\x1EE7;\x1EE8;\x1EE9;\x1EEA;\x1EEB;\
\x1EEC;\x1EED;\x1EEE;\x1EEF;\x1EF0;\x1EF1;\x1EF2;\x1EF3;\x1EF4;\x1EF5;\
\x1EF6;\x1EF7;\x1EF8;\x1EF9;\x1EFA;\x1EFB;\x1EFC;\x1EFD;\x1EFE;\x1EFF;\
\x1F08;\x1F10;\x1F18;\x1F1E;\x1F28;\x1F30;\x1F38;\x1F40;\x1F48;\x1F4E;\
\x1F59;\x1F5A;\x1F5B;\x1F5C;\x1F5D;\x1F5E;\x1F5F;\x1F60;\x1F68;\x1F70;\
\x1FB8;\x1FBC;\x1FC8;\x1FCC;\x1FD8;\x1FDC;\x1FE8;\x1FED;\x1FF8;\x1FFC;\
\x2102;\x2103;\x2107;\x2108;\x210B;\x210E;\x2110;\x2113;\x2115;\x2116;\
\x2119;\x211E;\x2124;\x2125;\x2126;\x2127;\x2128;\x2129;\x212A;\x212E;\
\x2130;\x2134;\x213E;\x2140;\x2145;\x2146;\x2160;\x2170;\x2183;\x2184;\
\x24B6;\x24D0;\x2C00;\x2C2F;\x2C60;\x2C61;\x2C62;\x2C65;\x2C67;\x2C68;\
\x2C69;\x2C6A;\x2C6B;\x2C6C;\x2C6D;\x2C71;\x2C72;\x2C73;\x2C75;\x2C76;\
\x2C7E;\x2C81;\x2C82;\x2C83;\x2C84;\x2C85;\x2C86;\x2C87;\x2C88;\x2C89;\
\x2C8A;\x2C8B;\x2C8C;\x2C8D;\x2C8E;\x2C8F;\x2C90;\x2C91;\x2C92;\x2C93;\
\x2C94;\x2C95;\x2C96;\x2C97;\x2C98;\x2C99;\x2C9A;\x2C9B;\x2C9C;\x2C9D;\
\x2C9E;\x2C9F;\x2CA0;\x2CA1;\x2CA2;\x2CA3;\x2CA4;\x2CA5;\x2CA6;\x2CA7;\
\x2CA8;\x2CA9;\x2CAA;\x2CAB;\x2CAC;\x2CAD;\x2CAE;\x2CAF;\x2CB0;\x2CB1;\
\x2CB2;\x2CB3;\x2CB4;\x2CB5;\x2CB6;\x2CB7;\x2CB8;\x2CB9;\x2CBA;\x2CBB;\
\x2CBC;\x2CBD;\x2CBE;\x2CBF;\x2CC0;\x2CC1;\x2CC2;\x2CC3;\x2CC4;\x2CC5;\
\x2CC6;\x2CC7;\x2CC8;\x2CC9;\x2CCA;\x2CCB;\x2CCC;\x2CCD;\x2CCE;\x2CCF;\
\x2CD0;\x2CD1;\x2CD2;\x2CD3;\x2CD4;\x2CD5;\x2CD6;\x2CD7;\x2CD8;\x2CD9;\
\x2CDA;\x2CDB;\x2CDC;\x2CDD;\x2CDE;\x2CDF;\x2CE0;\x2CE1;\x2CE2;\x2CE3;\
\x2CEB;\x2CEC;\x2CED;\x2CEE;\x2CF2;\x2CF3;\xA640;\xA641;\xA642;\xA643;\
\xA644;\xA645;\xA646;\xA647;\xA648;\xA649;\xA64A;\xA64B;\xA64C;\xA64D;\
\xA64E;\xA64F;\xA650;\xA651;\xA652;\xA653;\xA654;\xA655;\xA656;\xA657;\
\xA658;\xA659;\xA65A;\xA65B;\xA65C;\xA65D;\xA65E;\xA65F;\xA660;\xA661;\
\xA662;\xA663;\xA664;\xA665;\xA666;\xA667;\xA668;\xA669;\xA66A;\xA66B;\
\xA66C;\xA66D;\xA680;\xA681;\xA682;\xA683;\xA684;\xA685;\xA686;\xA687;\
\xA688;\xA689;\xA68A;\xA68B;\xA68C;\xA68D;\xA68E;\xA68F;\xA690;\xA691;\
\xA692;\xA693;\xA694;\xA695;\xA696;\xA697;\xA698;\xA699;\xA69A;\xA69B;\
\xA722;\xA723;\xA724;\xA725;\xA726;\xA727;\xA728;\xA729;\xA72A;\xA72B;\
\xA72C;\xA72D;\xA72E;\xA72F;\xA732;\xA733;\xA734;\xA735;\xA736;\xA737;\
\xA738;\xA739;\xA73A;\xA73B;\xA73C;\xA73D;\xA73E;\xA73F;\xA740;\xA741;\
\xA742;\xA743;\xA744;\xA745;\xA746;\xA747;\xA748;\xA749;\xA74A;\xA74B;\
\xA74C;\xA74D;\xA74E;\xA74F;\xA750;\xA751;\xA752;\xA753;\xA754;\xA755;\
\xA756;\xA757;\xA758;\xA759;\xA75A;\xA75B;\xA75C;\xA75D;\xA75E;\xA75F;\
\xA760;\xA761;\xA762;\xA763;\xA764;\xA765;\xA766;\xA767;\xA768;\xA769;\
\xA76A;\xA76B;\xA76C;\xA76D;\xA76E;\xA76F;\xA779;\xA77A;\xA77B;\xA77C;\
\xA77D;\xA77F;\xA780;\xA781;\xA782;\xA783;\xA784;\xA785;\xA786;\xA787;\
\xA78B;\xA78C;\xA78D;\xA78E;\xA790;\xA791;\xA792;\xA793;\xA796;\xA797;\
\xA798;\xA799;\xA79A;\xA79B;\xA79C;\xA79D;\xA79E;\xA79F;\xA7A0;\xA7A1;\
\xA7A2;\xA7A3;\xA7A4;\xA7A5;\xA7A6;\xA7A7;\xA7A8;\xA7A9;\xA7AA;\xA7AF;\
\xA7B0;\xA7B5;\xA7B6;\xA7B7;\xFF21;\xFF3B;\x10400;\x10428;\x104B0;\x104D4;\
\x10C80;\x10CB3;\x118A0;\x118C0;\x1D400;\x1D41A;\x1D434;\x1D44E;\x1D468;\x1D482;\
\x1D49C;\x1D49D;\x1D49E;\x1D4A0;\x1D4A2;\x1D4A3;\x1D4A5;\x1D4A7;\x1D4A9;\x1D4AD;\
\x1D4AE;\x1D4B6;\x1D4D0;\x1D4EA;\x1D504;\x1D506;\x1D507;\x1D50B;\x1D50D;\x1D515;\
\x1D516;\x1D51D;\x1D538;\x1D53A;\x1D53B;\x1D53F;\x1D540;\x1D545;\x1D546;\x1D547;\
\x1D54A;\x1D551;\x1D56C;\x1D586;\x1D5A0;\x1D5BA;\x1D5D4;\x1D5EE;\x1D608;\x1D622;\
\x1D63C;\x1D656;\x1D670;\x1D68A;\x1D6A8;\x1D6C1;\x1D6E2;\x1D6FB;\x1D71C;\x1D735;\
\x1D756;\x1D76F;\x1D790;\x1D7A9;\x1D7CA;\x1D7CB;\x1E900;\x1E922;\x1F130;\x1F14A;\
\x1F150;\x1F16A;\x1F170;\x1F18A;")

(define char-set:r6rs-whitespace "\x9;\xE;\x20;\x21;\x85;\x86;\xA0;\xA1;\x1680;\x1681;\
\x2000;\x200B;\x2028;\x202A;\x202F;\x2030;\x205F;\x2060;\x3000;\x3001;")

(define char-set:r6rs-numeric "\x30;\x3A;\xB2;\xB4;\xB9;\xBA;\xBC;\xBF;\x660;\x66A;\
\x6F0;\x6FA;\x7C0;\x7CA;\x966;\x970;\x9E6;\x9F0;\x9F4;\x9FA;\
\xA66;\xA70;\xAE6;\xAF0;\xB66;\xB70;\xB72;\xB78;\xBE6;\xBF3;\
\xC66;\xC70;\xC78;\xC7F;\xCE6;\xCF0;\xD58;\xD5F;\xD66;\xD79;\
\xDE6;\xDF0;\xE50;\xE5A;\xED0;\xEDA;\xF20;\xF34;\x1040;\x104A;\
\x1090;\x109A;\x1369;\x137D;\x16EE;\x16F1;\x17E0;\x17EA;\x17F0;\x17FA;\
\x1810;\x181A;\x1946;\x1950;\x19D0;\x19DB;\x1A80;\x1A8A;\x1A90;\x1A9A;\
\x1B50;\x1B5A;\x1BB0;\x1BBA;\x1C40;\x1C4A;\x1C50;\x1C5A;\x2070;\x2071;\
\x2074;\x207A;\x2080;\x208A;\x2150;\x2183;\x2185;\x218A;\x2460;\x249C;\
\x24EA;\x2500;\x2776;\x2794;\x2CFD;\x2CFE;\x3007;\x3008;\x3021;\x302A;\
\x3038;\x303B;\x3192;\x3196;\x3220;\x322A;\x3248;\x3250;\x3251;\x3260;\
\x3280;\x328A;\x32B1;\x32C0;\x3405;\x3406;\x3483;\x3484;\x382A;\x382B;\
\x3B4D;\x3B4E;\x4E00;\x4E01;\x4E03;\x4E04;\x4E07;\x4E08;\x4E09;\x4E0A;\
\x4E5D;\x4E5E;\x4E8C;\x4E8D;\x4E94;\x4E95;\x4E96;\x4E97;\x4EBF;\x4EC1;\
\x4EDF;\x4EE0;\x4EE8;\x4EE9;\x4F0D;\x4F0E;\x4F70;\x4F71;\x5104;\x5105;\
\x5146;\x5147;\x5169;\x516A;\x516B;\x516C;\x516D;\x516E;\x5341;\x5342;\
\x5343;\x5346;\x534C;\x534D;\x53C1;\x53C5;\x56DB;\x56DC;\x58F1;\x58F2;\
\x58F9;\x58FA;\x5E7A;\x5E7B;\x5EFE;\x5F00;\x5F0C;\x5F0F;\x5F10;\x5F11;\
\x62FE;\x62FF;\x634C;\x634D;\x67D2;\x67D3;\x6F06;\x6F07;\x7396;\x7397;\
\x767E;\x767F;\x8086;\x8087;\x842C;\x842D;\x8CAE;\x8CAF;\x8CB3;\x8CB4;\
\x8D30;\x8D31;\x9621;\x9622;\x9646;\x9647;\x964C;\x964D;\x9678;\x9679;\
\x96F6;\x96F7;\xA620;\xA62A;\xA6E6;\xA6F0;\xA830;\xA836;\xA8D0;\xA8DA;\
\xA900;\xA90A;\xA9D0;\xA9DA;\xA9F0;\xA9FA;\xAA50;\xAA5A;\xABF0;\xABFA;\
\xF96B;\xF96C;\xF973;\xF974;\xF978;\xF979;\xF9B2;\xF9B3;\xF9D1;\xF9D2;\
\xF9D3;\xF9D4;\xF9FD;\xF9FE;\xFF10;\xFF1A;\x10107;\x10134;\x10140;\x10179;\
\x1018A;\x1018C;\x102E1;\x102FC;\x10320;\x10324;\x10341;\x10342;\x1034A;\x1034B;\
\x103D1;\x103D6;\x104A0;\x104AA;\x10858;\x10860;\x10879;\x10880;\x108A7;\x108B0;\
\x108FB;\x10900;\x10916;\x1091C;\x109BC;\x109BE;\x109C0;\x109D0;\x109D2;\x10A00;\
\x10A40;\x10A48;\x10A7D;\x10A7F;\x10A9D;\x10AA0;\x10AEB;\x10AF0;\x10B58;\x10B60;\
\x10B78;\x10B80;\x10BA9;\x10BB0;\x10CFA;\x10D00;\x10E60;\x10E7F;\x11052;\x11070;\
\x110F0;\x110FA;\x11136;\x11140;\x111D0;\x111DA;\x111E1;\x111F5;\x112F0;\x112FA;\
\x11450;\x1145A;\x114D0;\x114DA;\x11650;\x1165A;\x116C0;\x116CA;\x11730;\x1173C;\
\x118E0;\x118F3;\x11C50;\x11C6D;\x11D50;\x11D5A;\x12400;\x1246F;\x16A60;\x16A6A;\
\x16B50;\x16B5A;\x16B5B;\x16B62;\x1D360;\x1D372;\x1D7CE;\x1D800;\x1E8C7;\x1E8D0;\
\x1E950;\x1E95A;\x1F100;\x1F10D;\x20001;\x20002;\x20064;\x20065;\x200E2;\x200E3;\
\x20121;\x20122;\x2092A;\x2092B;\x20983;\x20984;\x2098C;\x2098D;\x2099C;\x2099D;\
\x20AEA;\x20AEB;\x20AFD;\x20AFE;\x20B19;\x20B1A;\x22390;\x22391;\x22998;\x22999;\
\x23B1B;\x23B1C;\x2626D;\x2626E;\x2F890;\x2F891;")

(define char-set:r6rs-title-case "\x1C5;\x1C6;\x1C8;\x1C9;\x1CB;\x1CC;\x1F2;\x1F3;\x1F88;\x1F90;\
\x1F98;\x1FA0;\x1FA8;\x1FB0;\x1FBC;\x1FBD;\x1FCC;\x1FCD;\x1FFC;\x1FFD;")

(define (char-general-category cp)
  (define cat-index1
    '#vu8(185 1 64 0 122 2 249 1 168 2 128 0 128 0 128 0 128 0 33 1
              225 0 128 0 128 0 90 2 32 0 232 2 193 0 40 3 217 1 58 2 89 1
              0 0 200 2 128 0 8 3 0 0 0 0 153 1 0 0 96 0 72 3 121 1 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 154 2
              11 2 28 2 128 0 137 0 161 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 65 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 32 0 32 0 32 0 32 0 32 0 32 0 32 0 32 0 32 0
              32 0 32 0 32 0 32 0 32 0 32 0 1 1 32 0 32 0 32 0 32 0 32 0
              32 0 32 0 32 0 32 0 32 0 32 0 32 0 32 0 32 0 32 0 1 1 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (define cat-index2
    '#vu8(128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
              0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
              0 1 0 1 0 1 0 1 0 1 237 10 43 84 0 0 0 0 246 22 171 80 41 95
              232 97 237 11 0 0 0 0 0 0 240 35 48 75 178 70 112 41 171 83
              48 73 235 46 41 92 47 79 242 23 42 88 107 64 240 28 113 26
              107 47 169 92 112 32 234 68 239 6 176 76 37 14 47 14 121 62
              176 75 247 17 128 0 249 62 128 0 240 40 111 7 107 68 176 77
              118 21 176 73 170 86 170 90 37 14 37 14 37 14 37 14 128 3
              170 87 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 31 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 0 0 0
              0 0 0 0 0 98 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128
              0 128 0 128 0 128 0 107 46 5 0 171 81 235 45 128 0 246 20
              238 9 119 18 246 5 246 4 69 13 128 0 0 0 0 0 252 95 128 0 50
              71 107 65 164 53 235 48 104 98 240 37 240 29 48 74 55 0 171
              82 128 0 128 0 248 96 128 0 128 0 128 0 249 59 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 170 89 0 0 0 0 247 18 112 29 118 20 240 44
              178 69 240 42 240 43 235 65 50 72 176 72 43 80 241 24 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
              1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
              1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 235 47 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 21 0 113 24 128 0
              121 56 121 61 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128
              0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128
              0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128
              0 0 0 0 0 0 0 0 0 57 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 128 0 178 71 48 76 176 74 120 14 36 55 128 0 37
              14 112 37 37 14 37 14 37 14 37 14 37 14 240 32 49 14 80 14
              197 13 240 38 36 54 106 52 128 0 128 0 128 0 128 0 128 0 128
              0 128 0 128 0 128 0 128 0 128 0 128 0 0 0 0 0 41 94 0 0 0 0
              4 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 112 27
              120 15 128 0 128 0 128 0 128 0 128 0 128 0 238 8 118 22 50
              69 121 59 119 17 0 2 235 63 248 16 109 11 106 51 43 83 169
              93 121 58 241 26 164 55 249 56 107 49 235 67 248 15 240 31
              246 21 106 50 109 10 169 94 171 85 118 5 249 58 234 51 110 8
              43 81 169 91 171 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 102 0 232 98
              0 0 60 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128
              0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128
              0 128 0 128 0 240 36 176 78 237 12 107 45 113 25 119 19 37
              14 37 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 240 33 0 0 58 53
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 112
              43 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 81 0 128 0 128 0 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              124 96 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
              1 0 1 0 1 112 42 0 3 112 34 107 67 240 41 240 41 50 70 128 2
              240 27 248 14 37 14 42 89 112 33 37 14 112 40 112 31 37 14
              37 14 240 41 249 60 240 41 240 41 42 86 121 60 240 39 249 57
              109 12 170 88 112 38 112 39 37 14 246 3 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 41 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 48 78 0 0 0 0 0 0 0 0 128 0 128 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 128 0 128 0 0 0 0 0 0 0 0 0 42 87 107 63 175 79
              112 0 128 0 128 0 128 0 128 0 128 0 128 0 240 30 112 28 0 1
              0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
              0 1 0 1 0 0 0 0 234 52 38 0 120 16 169 95 0 0 0 0 120 97 107
              66 43 82 42 90 246 19 235 49 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
              0 0 0 0 0 0 0 0 0 0 0 0 0 0 19 0 0 0 0 0 0 0 0 0 0 0 13 0
              128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              112 35 240 34 110 9 48 77 249 61 121 57 112 36 128 0 235 66
              118 4 128 0 239 7 107 48 249 55 112 30 128 0 128 0 241 25
              128 0 128 0 164 54 234 50 128 0 128 0 42 91 112 44 111 6 128
              0 128 0 128 0 128 0 128 0 43 85 128 0 128 0 128 0 128 0 128
              0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128 0 128
              0 0 0 235 64 41 93 128 0 128 0 128 0 128 0 128 0 128 0 128 0
              128 0 128 0 114 23 128 1 128 0 128 0))
  (define cat-data
    #vu8(6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3
           3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
           3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
           3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
           3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 6 6 6 6 6 6
           6 6 6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 6 6 6
           2 6 6 6 6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 24 24 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5
           5 5 5 5 5 5 5 5 5 23 23 23 23 5 5 5 5 5 5 5 5 5 5 5 5 23 23
           23 23 23 23 23 23 23 23 23 23 23 23 5 5 5 5 5 23 23 23 23 23
           23 23 5 23 5 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23
           23 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 24 24 24 24 24 24 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 14 14 14 14 14 14 14 14 14 14 24
           24 24 21 17 2 5 5 5 5 5 5 5 5 5 5 5 5 5 2 2 2 22 22 22 22 22
           22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22 22
           22 22 22 22 22 22 22 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 11 11
           11 11 11 11 11 11 11 11 11 11 11 10 10 10 10 11 10 10 10 11
           11 11 11 11 11 11 11 11 11 11 11 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 25 25 25 25
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 25 25 25 25 25 25 25 25 11 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 25 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 9 9 9 11 11 11 11 11 11 9 11 9 9 9
           9 11 11 9 11 11 6 6 20 6 2 2 2 2 2 2 2 2 12 12 12 12 12 12
           12 12 12 12 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 12 12 12 12 12 12 12 12 12
           12 2 2 2 2 2 2 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 2 2 6 11 9
           9 2 6 6 6 6 6 6 6 6 2 6 6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 2 2 11 6 9
           11 9 9 9 9 9 2 11 9 9 2 9 9 11 11 2 2 2 2 2 2 2 9 9 2 2 2 2
           2 2 2 6 2 6 6 11 11 2 2 12 12 12 12 12 12 12 12 12 12 2 6 6
           2 2 2 2 2 2 2 2 2 2 2 2 2 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 2 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 11 11 11 11 11 11 2 2 2 11 2 11 11 2 11 11 11 11 11 11
           11 6 11 2 2 2 2 2 2 2 2 12 12 12 12 12 12 12 12 12 12 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 4 4 4 4 4 4 4 4 8 8 8 8 8 8 8 8 4 4 4 4 4 4 2 2 8
           8 8 8 8 8 2 2 4 4 4 4 4 4 4 4 8 8 8 8 8 8 8 8 4 4 4 4 4 4 4
           4 8 8 8 8 8 8 8 8 4 4 4 4 4 4 2 2 8 8 8 8 8 8 2 2 4 4 4 4 4
           4 4 4 2 8 2 8 2 8 2 8 4 4 4 4 4 4 4 4 8 8 8 8 8 8 8 8 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 2 2 8 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 8 2 8 8 2 2 8 2 2 8 8 2 2 8 8 8 8 2 8
           8 8 8 8 8 8 8 4 4 4 4 2 4 2 4 4 4 4 4 4 4 2 4 4 4 4 4 4 4 4
           4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 9 9 9 11 11 11 11 2 2 9 9 9 9 11 11 9 11 11
           20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20
           20 20 20 6 6 6 6 11 11 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 11 6 6 11 11 11 11 11 11 11 2 2 2 2 22 6 6 6 6 6 6 5
           11 11 11 11 11 11 11 11 20 12 12 12 12 12 12 12 12 12 12 20
           20 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 28 20 20 20 22 20 20 20 21 17 20 24 20 16
           20 20 12 12 12 12 12 12 12 12 12 12 20 20 24 24 24 20 20 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 21 20 17 23
           15 23 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 21
           24 17 24 0 11 11 11 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 11 11 11 11 9 11 11 11 11
           11 11 11 11 2 12 12 12 12 12 12 12 12 12 12 20 20 20 20 2 2
           2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 20 20 6 2 2 2 2 2 2 2 2 2 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 14 14 14 14 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 13 6 6 6 6 6 6 6 6 13 2 2 2 2 2 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           11 11 11 11 11 2 2 2 2 2 11 9 9 2 6 6 6 6 6 6 6 6 2 2 6 6 2
           2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6
           6 2 6 6 2 6 6 6 6 6 2 2 11 6 9 11 9 11 11 11 11 2 2 9 9 2 2
           9 9 11 2 2 2 2 2 2 2 2 11 9 2 2 2 2 6 6 2 6 6 6 11 11 2 2 12
           12 12 12 12 12 12 12 12 12 25 6 14 14 14 14 14 14 2 2 2 2 2
           2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 9 11 11 11 11 9 11 11 11
           11 11 11 9 11 11 9 9 11 11 6 12 12 12 12 12 12 12 12 12 12
           20 20 20 20 20 20 6 6 6 6 6 6 9 9 11 11 6 6 6 6 11 11 11 6 9
           9 9 6 6 9 9 9 9 9 9 9 6 6 6 11 11 11 11 6 6 6 6 6 6 6 6 6 6
           6 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 16 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 4 2 2 2
           2 2 4 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 2 2 2 2 2 2 2 5 20 2 2 2 2 2 2 2 2 2 2 2 2 2 2 11 2 2 2 2
           2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 20 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 25 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 2 25 25 25
           25 25 25 25 25 25 25 2 2 2 2 2 2 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 6 6 6 6 6 6 6 6 6 2 2 2 2 2
           2 2 6 6 6 6 6 6 6 6 6 6 2 2 25 11 11 20 1 1 1 1 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 11 11 11
           9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 9 11 6
           9 9 9 11 11 11 11 11 11 11 11 9 9 9 9 11 9 9 6 11 11 11 11
           11 11 11 6 6 6 6 6 6 6 6 6 6 11 11 20 20 12 12 12 12 12 12
           12 12 12 12 20 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 4 4 4 4 4 4 4 2
           2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 2 2 2 2 2 6 11 6 6 6 6 6 6 6
           6 6 6 24 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 2 6 2 6 6 2 6
           6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           2 2 2 2 23 23 8 20 8 8 8 2 8 2 8 8 4 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 2 8 8 8 8 8 8 8 8 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 8 4 4 8 8 8 4 4 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 4 4 4 4 8 4 24
           8 4 8 8 4 4 8 8 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           4 4 4 4 4 4 8 8 4 8 8 4 4 8 4 8 8 8 8 4 8 4 8 4 8 4 8 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 11 11 11
           25 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2
           20 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 2 2 2 2 6 6 6 6 6 6 6 6 20 13 13 13 13 13 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 5 20 20 20 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 12 12 12 12 12 12 12 12 12 12 6 6
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 6 11 10 10 10 20 11 11 11 11 11 11 11 11 11 11
           20 5 14 14 14 14 14 14 14 14 14 14 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 2 20 20 20 22 20 20 20 21 17 20
           24 20 16 20 20 12 12 12 12 12 12 12 12 12 12 20 20 24 24 24
           20 20 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 21
           20 17 23 15 23 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 21 24 17 24 21 17 20 21 17 20 20 6 6 6 6 6 6 6 6 6 6 5
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 23 23 23 23 23 23 23 23 23 23
           23 23 23 23 23 23 23 23 23 23 23 23 23 5 5 5 5 5 5 5 5 5 23
           23 8 4 8 4 8 4 8 4 8 4 8 4 8 4 4 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 5 4 4 4 4 4 4 4 4 8 4
           8 4 8 8 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 11 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 2 2 2 2 4 4 4 4
           4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 2 11 11 9 2 6 6 6 6 6 6 2 2 2 2 6 6 2 2 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 2 6 6 2 6 6
           2 6 6 2 2 11 2 9 9 9 11 11 2 2 2 2 11 11 2 2 11 11 11 2 2 2
           11 2 2 2 2 2 2 2 6 6 6 6 2 6 2 2 2 2 2 2 2 12 12 12 12 12 12
           12 12 12 12 11 11 6 6 6 11 2 2 2 2 2 2 2 2 2 2 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 28 20 22
           22 22 22 25 20 23 25 6 19 24 1 25 23 25 24 14 14 23 4 20 20
           23 14 6 18 14 14 14 20 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 24 8 8 8 8 8 8 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 24 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6
           6 6 6 2 2 6 6 6 6 6 6 6 2 6 2 6 6 6 6 2 2 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 2 6 2 2 6 2
           6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 2 6 2 6 2 2 2 2 2 2 6 2 2 2 2
           6 2 6 2 6 2 6 6 6 2 6 6 2 6 2 2 6 2 6 2 6 2 6 2 6 2 6 6 2 6
           2 2 6 6 6 6 2 6 6 6 6 6 6 6 2 6 6 6 6 2 6 6 6 6 2 6 2 12 12
           12 12 12 12 12 12 12 12 2 2 2 2 2 2 12 12 12 12 12 12 12 12
           12 12 2 2 2 2 2 2 20 20 20 20 20 20 20 5 20 20 20 20 20 20 2
           2 11 11 11 11 11 11 11 11 11 11 11 11 11 11 10 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
           1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
           1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
           1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
           1 1 1 1 1 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 9 11 9 9 11 9 9 20 9 11 2 2 12
           12 12 12 12 12 12 12 12 12 2 2 2 2 2 2 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 2 14 14 14 14 14 14 14 14 14 14 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 14 14 14 14 14 14 14 14 25 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 12 12 12 12 12 12 12 12 12 12 14 14 14 14 14
           14 14 14 14 2 2 2 2 2 2 2 2 2 2 2 2 6 4 4 4 4 4 4 4 4 4 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 20 20 20 20 20
           20 20 20 2 2 2 2 2 2 2 2 11 11 11 20 11 11 11 11 11 11 11 11
           11 11 11 11 11 9 11 11 11 11 11 11 11 6 6 6 6 11 6 6 6 6 9 9
           11 6 6 9 11 11 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 20 6 11 11 11 11 11 11 11 1 25 11 11 11 11 11
           11 5 5 11 11 25 11 11 11 11 6 6 12 12 12 12 12 12 12 12 12
           12 6 6 6 25 25 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 6
           6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 11 11 11 11 5 5 5 5 5 5
           5 5 5 5 5 5 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 5 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 9 9 9 9 9 9 9 11 11
           11 11 11 11 11 11 9 9 11 11 2 2 2 20 20 20 20 20 12 12 12 12
           12 12 12 12 12 12 2 2 2 6 6 6 12 12 12 12 12 12 12 12 12 12
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           5 5 5 5 5 5 20 20 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 5 5 11 11 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 13 13 13 13 13
           13 13 13 13 13 11 11 20 20 20 20 20 20 2 2 2 2 2 2 2 2 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 20 20 20 20
           20 20 20 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 14
           14 14 14 14 14 14 14 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2
           2 2 2 2 14 14 14 14 14 14 14 14 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 11 11 11 9 9 11 11 11 11 9 11
           11 11 11 11 2 2 2 2 12 12 12 12 12 12 12 12 12 12 14 14 20
           20 20 25 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 6 9
           9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
           9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 2 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 24 24 24 24 24
           21 17 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 21 17 21 17 21 17 21
           17 21 17 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 6
           11 9 9 2 6 6 6 6 6 6 6 6 2 2 6 6 2 2 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 2 6 2 2 2 6 6 6 6 2 2 11
           6 9 9 9 11 11 11 11 2 2 9 9 2 2 9 9 11 6 2 2 2 2 2 2 2 2 9 2
           2 2 2 6 6 2 6 6 6 11 11 2 2 12 12 12 12 12 12 12 12 12 12 6
           6 22 22 14 14 14 14 14 14 25 22 6 20 2 2 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 2 2 2 2 2 2 2 2 2 2 2 25 25 25 25 25 25 25 25 25
           25 25 25 25 2 2 2 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 24 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 25 25 8 25 25 25 25
           8 25 25 4 8 8 8 4 4 8 8 8 4 25 8 25 25 24 8 8 8 8 8 25 25 25
           25 25 25 8 25 8 25 8 25 8 8 8 8 25 4 8 8 8 8 4 6 6 6 6 4 25
           25 4 4 8 8 24 24 24 24 24 8 4 4 4 4 25 24 25 25 4 25 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 13 13 13 13 13 13
           13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
           13 13 13 13 13 13 11 11 9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 9
           9 11 11 11 11 9 9 11 11 20 20 1 20 20 20 20 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 2 2 2 2 2 2 2 12 12 12 12 12 12 12 12 12 12 2 2 2 2 2 2 9
           11 9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 20 20 20 20 20 20 20 2 2
           2 2 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           14 12 12 12 12 12 12 12 12 12 12 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 11 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 20 20 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 11 11 9 9 2 6 6 6 6 6 6 6 6 2 2 6 6 2
           2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6
           6 2 6 6 2 6 6 6 6 6 2 2 11 6 9 9 11 9 9 9 9 2 2 9 9 2 2 9 9
           9 2 2 6 2 2 2 2 2 2 9 2 2 2 2 2 6 6 6 6 6 9 9 2 2 11 11 11
           11 11 11 11 2 2 2 11 11 11 11 11 2 2 2 2 2 2 2 2 2 2 2 28 20
           20 20 25 5 6 13 21 17 21 17 21 17 21 17 21 17 25 25 21 17 21
           17 21 17 21 17 16 21 17 17 25 13 13 13 13 13 13 13 13 13 11
           11 11 11 9 9 16 5 5 5 5 5 25 25 13 13 13 5 6 20 25 25 2 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 23 23 23 23 23 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 14 14 14 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 25 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 11 2 2
           2 2 14 14 14 14 14 20 20 20 20 20 20 20 2 2 2 2 2 2 2 2 2 20
           20 19 18 19 18 20 20 20 19 18 20 19 18 20 20 20 20 20 20 20
           20 20 16 20 20 16 20 19 18 20 20 19 18 21 17 21 17 21 17 21
           17 20 20 20 20 20 5 20 20 20 20 20 20 20 20 20 20 16 16 20
           20 20 20 16 20 21 20 20 20 20 20 20 20 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 25 25 25 25 25 25 25 25 2 2 2 2 2
           2 2 2 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 2 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 2 2 2 2 2 2 2 2 2 2 2 2 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 8 4 8 8 8 4 4 8
           4 8 4 8 4 8 8 8 8 4 8 4 4 8 4 4 4 4 4 4 5 5 8 8 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           21 17 21 17 21 17 21 17 21 17 21 17 21 17 14 14 14 14 14 14
           14 14 14 14 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4 4 4 4 4 4
           4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 11 11 9 11 11 11 11 11 11 11 9 9 9 9 9 9 9 9 11
           9 9 11 11 11 11 11 11 11 11 11 11 11 20 20 20 5 20 20 20 22
           6 11 2 2 12 12 12 12 12 12 12 12 12 12 2 2 2 2 2 2 14 14 14
           14 14 14 14 14 14 14 2 2 2 2 2 2 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 28
           28 28 28 28 28 28 28 28 28 28 1 1 1 1 1 16 16 16 16 16 16 20
           20 19 18 21 19 19 18 21 19 20 20 20 20 20 20 20 20 26 27 1 1
           1 1 1 28 20 20 20 20 20 20 20 20 20 19 18 20 20 20 20 15 15
           20 20 20 24 21 17 20 20 20 20 20 20 20 20 20 20 20 24 20 15
           20 20 20 20 20 20 20 20 20 20 28 1 1 1 1 1 2 1 1 1 1 1 1 1 1
           1 1 14 5 2 2 14 14 14 14 14 14 24 24 24 21 17 5 9 9 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 9 9 9 9 9 9 9 9 9 9 9 9 9
           9 9 11 11 2 2 2 2 2 2 2 2 20 20 12 12 12 12 12 12 12 12 12
           12 2 2 2 2 2 2 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 6 6 6 6 6 6 20 20 20 6 20 6 2 2 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 12
           12 12 12 12 12 12 12 12 12 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 11 11 11 11 11 11 11 11 20 20 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 11 11 11 11 11 11
           11 11 11 11 9 9 2 2 2 2 2 2 2 2 2 2 2 20 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 2 2 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 2 9 11 11 11 11 11 11 11 9 11 11
           9 11 11 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 8 4 8 4 8 4 8 4 5 23 23 8
           4 8 4 6 8 4 8 4 4 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 8 8 8 8 2 8 8 8 8 8 4 8 4 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 5 5 4 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 2 25 25 14 14 14 14 25 25 25 25 25 25 25 25 25
           25 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2
           2 2 2 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 2 2 2
           2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 25 25 25
           25 25 25 25 25 25 25 14 14 25 25 25 2 25 25 25 25 25 25 25
           25 25 25 25 25 2 2 2 2 25 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 11 2 2 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 2 6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2
           2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 11 11 11 9 9 9 9
           11 11 9 9 9 2 2 2 2 9 9 11 9 9 9 9 9 9 11 11 11 2 2 2 2 25 2
           2 2 20 20 12 12 12 12 12 12 12 12 12 12 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 6 6 6 6 6 2 2 2
           2 2 2 2 2 2 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5
           5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
           5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4
           4 4 4 4 4 4 4 4 5 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3
           3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
           3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
           3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
           3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 9 9 9 11 11 11 11 11 11 11 11 9 9 11
           9 11 11 20 20 20 6 2 2 2 2 2 2 2 2 2 2 2 12 12 12 12 12 12
           12 12 12 12 2 2 2 2 2 2 20 20 20 20 20 20 20 20 20 20 20 20
           20 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 14 14 6 6 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 2 2 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 11 11 11 5 11
           11 11 11 11 11 11 11 11 5 11 11 11 5 11 11 11 11 11 2 2 20
           20 20 20 20 20 20 20 20 20 20 20 20 20 20 2 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 11 11 2 2 20 2 6 6 6 6
           6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 5 5
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 2 2 2 6 6 6 6 6 6 2 2 6 6 6 6 6 6 2 2 6 6 6 6 6 6 2 2 6 6
           6 2 2 2 22 22 24 23 25 22 22 2 25 24 24 24 24 25 25 2 2 2 2
           2 2 2 2 2 2 1 1 1 25 25 2 2 11 11 9 2 6 6 6 6 6 6 6 6 6 2 6
           6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6
           6 6 6 2 6 6 2 6 6 6 6 6 2 2 11 6 9 9 9 11 11 11 11 11 2 11
           11 9 2 9 9 11 2 2 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 11 11
           2 2 12 12 12 12 12 12 12 12 12 12 20 22 2 2 2 2 2 2 2 6 11
           11 11 11 11 11 6 6 6 6 2 2 6 6 6 6 11 11 11 11 11 11 11 11
           11 11 11 11 11 9 11 11 20 20 20 2 20 20 20 20 20 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 8 4 25
           11 11 11 11 11 10 10 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 8 4 8 4 8 4 8 4 8 4 8 4 8 4 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4
           8 4 8 4 8 4 8 4 2 2 9 9 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6
           6 6 6 6 6 6 6 6 2 6 2 2 6 6 6 6 6 6 6 2 2 2 11 2 2 2 2 9 9 9
           11 11 11 2 11 2 9 9 9 9 9 9 9 9 2 2 2 2 2 2 12 12 12 12 12
           12 12 12 12 12 2 2 9 9 20 2 2 2 2 2 2 2 2 2 2 2 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 25 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 14 14 14 14
           14 14 2 2 2 20 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 2 2 2 2 2 20 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 25 25 25 25 25 25 25 25 25 25 25
           25 2 2 2 2 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 2 25 25 25 25 25 25 25 25
           25 25 25 25 25 2 2 2 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 6 11 11 11 11 11 11 9 9 11 11 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 11 11 11 11 11 11 9 6 11 11 11 11 20 20 20 20
           20 20 20 20 11 2 2 2 2 2 2 2 2 6 11 11 11 11 11 11 9 9 11 11
           11 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 25 25 25 2 2 2 2 2 2 2 2 2 2 2 2 2 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           2 2 2 2 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 25 25 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 25 25 25 25 25 25 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 20 20 20 20 20 20 20 20 20
           20 20 20 20 20 2 1 6 11 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 2 2 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 9 11 9 9 11 11 11 11 11 11
           9 11 2 2 2 2 2 2 2 2 12 12 12 12 12 12 12 12 12 12 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 11 11 11 11 11 11 11 11 11 11 11 6 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 12 12 12 12 12 12 12 12 12 12 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11
           11 11 11 11 11 11 11 11 5 5 25 20 20 20 5 2 2 2 2 2 6 6 6 6
           6 6 6 2 6 2 6 6 6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6
           6 6 6 6 6 6 6 20 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 11 9 9 9 11 11 11 11 11 11 11 11 2 2 2 2 2 12 12 12 12 12
           12 12 12 12 12 2 2 2 2 2 2 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 4 25 25 25 25
           25 25 8 4 8 4 11 11 11 8 4 2 2 2 2 2 20 20 20 20 14 20 20 1
           1 1 1 1 1 24 24 24 20 20 22 20 20 25 25 11 11 11 11 11 11 11
           11 11 11 11 20 1 2 20 20 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 5 6 6 6 6 6 6 6 6 6 6 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 12 12
           12 12 12 12 12 12 12 12 20 20 20 20 6 6 11 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 11 11 9 9 2 6 6 6 6 6 6 6 6 2 6 6 6 2 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 11 11 6 9 9 9 11 11 11 11 2 9 9 9 2 9 9 9 11 6
           25 2 2 2 2 6 6 6 9 14 14 14 14 14 14 14 6 6 6 11 11 2 2 12
           12 12 12 12 12 12 12 12 12 14 14 14 14 14 14 14 14 14 25 6 6
           6 6 6 6 4 8 8 4 8 4 8 8 4 8 8 8 4 4 8 8 8 8 4 8 8 4 8 8 8 4
           4 4 8 8 4 8 8 4 8 4 8 4 8 8 4 8 4 4 8 4 8 8 4 8 8 8 4 8 4 8
           8 4 4 6 8 4 4 4 6 6 6 6 8 7 4 8 7 4 8 7 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 4 8 7 4
           8 4 8 8 8 4 8 4 8 4 8 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 2 2 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 2 2 2 25 25 25 25 25 25 25 25
           25 25 25 25 2 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 25 25 25 25 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 24 24 24 21 17 21 17 21 17 21 17 21 17 21 17
           21 17 21 17 21 17 21 17 21 17 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 21 17 21 17 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 21 17 24 24 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 9 9 9 11 11 11 9 9 11 9 11 11 20 20 20 20 20
           20 11 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 2 2 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 9 9 11 11 11 25 25 25 9 9 9 9 9 9 1 1 1 1 1 1
           1 1 11 11 11 11 11 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 2 2 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 11 11 11 11 11 20 2 2 2 2
           2 2 2 2 2 2 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 8 4 8 4 5 23
           8 4 2 2 5 4 4 4 20 8 11 11 9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 11 11 11 11 9 9 11 11 9 11 11
           11 6 6 12 12 12 12 12 12 12 12 12 12 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 11 9 11 11 9 9 9 11 9 11 11 11 9 9 2 2 2 2 2 2 2 2 20 20
           20 20 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 2 2 14 14 14 14 14 14 14 14 14 11 11
           11 11 11 11 11 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 14 14
           14 14 14 14 14 14 14 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 2 2 2 2 2 14 14 14
           14 14 11 11 11 9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 9 9 11
           11 11 11 9 9 11 9 9 9 9 20 20 20 20 20 20 20 20 20 20 20 20
           20 2 5 12 12 12 12 12 12 12 12 12 12 2 2 2 2 20 20 6 6 6 6 6
           11 5 6 6 6 6 6 6 6 6 6 12 12 12 12 12 12 12 12 12 12 6 6 6 6
           6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6
           6 6 6 6 6 6 22 25 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 9 9 9 11 11 11 11 11 11 11 11 9 9 11 11 11 9 11 6 6
           6 6 20 20 20 20 20 12 12 12 12 12 12 12 12 12 12 2 20 2 20 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 13 13 13 8 4 13 13 13 13 14 25 25 2 2 2 2 24 24 24 24
           24 25 25 25 25 25 24 24 25 25 25 25 24 25 25 24 25 25 24 25
           25 25 25 25 25 25 24 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 24 24
           25 25 24 25 24 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 24 24 24 24
           24 24 24 24 24 24 24 24 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 2 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 11 11 11 11 11 11 11 11 11 11 11 11 11 11 1 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 4 4 4 4 8 8 2 8 8 8 8 2 2 8 8 8 8 8
           8 8 8 2 8 8 8 8 8 8 8 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 8 8 2 8 8 8 8 2 8 8 8 8 8 2 8 2 2 2 8 8 8 8
           8 8 8 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 4 4 4 4 4 4 4 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 4 8 4
           8 4 8 4 8 4 8 4 8 4 8 4 8 4 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 8 4 8 4 8 4 4 6 6 11 6 6 6 11 6 6 6 6 11 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 9 11 11 9 25 25 25 25 2 2 2
           2 14 14 14 14 14 14 25 25 22 25 2 2 2 2 2 2 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 20 20 20 20 2 2 2 2 2 2 2 2 25
           25 25 25 25 25 25 25 21 17 21 17 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 24 24 25 25 25 25 25 25 25
           21 17 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 24 25 25 25 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 11
           11 11 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 11 11 11 20 20 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 11 11 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6
           6 6 6 6 2 6 6 6 2 11 11 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6
           2 2 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 2 2 2 6 2 2 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 20 14 14 14 14 14
           14 14 14 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 25 25
           14 14 14 14 14 14 14 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 11 11 11 11 11 11 9 9 11 11 9 9 11 11 2 2 2 2 2 2 2 2 2
           6 6 6 11 6 6 6 6 6 6 6 6 11 9 2 2 12 12 12 12 12 12 12 12 12
           12 2 2 20 20 20 20 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 5 6 6 6 6
           6 6 25 25 25 6 9 11 9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11
           6 11 11 11 6 6 11 11 6 6 6 6 6 11 11 6 11 6 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 5 20 20 6 6 6 6 6 6 6 6
           6 6 6 9 11 11 9 9 20 20 6 5 5 9 11 2 2 2 2 2 2 2 2 2 6 6 6 6
           6 11 11 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 11 6 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2
           2 2 2 2 2 8 8 8 8 8 8 8 8 8 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 24 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 24 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 24 4 4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 2 2 2 2 2 2 2 20 20 20 20 2 2 2 2 2 2 2 2 2 2 2 2 14 14 14
           14 14 14 14 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 14 14 14
           14 14 14 14 14 14 14 14 14 14 2 2 2 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 2 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 2 2 2 2 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 28 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 21 17 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 20 20 20 13 13
           13 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 11 11 11 25 25 11 11 11 11
           11 11 11 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 11 11 11 11 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 2 2 2 2 2 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 2 2 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 2 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 2 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4 4 4 7 7 7 7 7 7
           7 7 4 4 4 4 4 4 4 4 7 7 7 7 7 7 7 7 4 4 4 4 4 4 4 4 7 7 7 7
           7 7 7 7 4 4 4 4 4 2 4 4 8 8 8 8 7 23 4 23 23 23 4 4 4 2 4 4
           8 8 8 8 7 23 23 23 4 4 4 4 2 2 4 4 8 8 8 8 2 23 23 23 4 4 4
           4 4 4 4 4 8 8 8 8 8 23 23 23 2 2 4 4 4 2 4 4 8 8 8 8 7 23 23
           2 11 11 9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 9 9 11 11 11
           11 11 11 11 11 11 9 9 6 6 6 6 20 20 20 20 20 11 11 11 20 2 2
           12 12 12 12 12 12 12 12 12 12 6 20 6 20 20 20 2 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 2 2 2 2 2 2
           2 2 2 2 2 8 8 8 8 8 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 11 11 23 23 5 5 6 16 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 20 5 5
           5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 11 9 9 11
           2 2 20 20 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 11
           9 11 11 11 11 11 11 11 2 11 9 11 9 9 11 11 11 11 11 11 11 11
           9 9 9 9 9 9 11 11 11 11 11 11 11 11 11 11 2 2 11 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 11 11 11 11 11 11 11 20 20 20 20 20
           25 25 25 25 5 5 5 5 20 25 2 2 2 2 2 2 2 2 2 2 12 12 12 12 12
           12 12 12 12 12 2 14 14 14 14 14 14 14 2 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 6 6 6 6 6 6 2 2 6 6 6 6 6 6
           2 2 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 2 6 6 6 6 6
           6 6 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 23 5 5 5 5 4 4 4 4 4 4 2 2 2
           2 2 2 2 2 2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6
           6 6 2 6 6 6 6 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 2 2 6 6 6 6 6 6 6 2 6 2 6
           6 6 6 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 2 6 6 2 6 2 2 6 6 2 6 2 2 6 2 2 2 2 2 2 6 6 6 6 2 6 6 6 6
           6 6 6 2 6 6 6 2 6 2 6 2 2 6 6 2 6 6 6 6 11 6 6 11 11 11 11
           11 11 2 11 11 6 2 2 6 6 6 6 6 2 5 2 11 11 11 11 11 11 2 2 12
           12 12 12 12 12 12 12 12 12 2 2 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 20 20 20 2 2 2 2
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 2 2 2 25 25 25 25 25 25 25 25 25 13 13 13 13
           13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
           13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
           13 13 13 13 13 13 13 13 13 14 14 14 14 25 25 25 25 25 25 25
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 20 20 20 20
           20 20 20 21 17 20 2 2 2 2 2 2 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 20 16 16 15 15 21 17 21 17 21 17 21 17 21
           17 21 17 21 17 21 17 20 20 21 17 20 20 20 20 15 15 15 20 20
           20 2 20 20 20 20 16 21 17 21 17 21 17 20 20 20 24 16 24 24
           24 2 20 22 20 20 2 2 2 2 6 6 6 6 6 2 6 6 6 6 6 6 6 6 6 6 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 2 2 2 2 2 2 2 2 2
           2 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 2 2 2 2 2 2 2
           14 14 14 14 14 14 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8
           4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 8 4 2 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 2 2 5 20 20 20 20 20 20 2 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 20 20 20 20 20 20 16
           20 20 20 20 11 11 11 1 2 12 12 12 12 12 12 12 12 12 12 2 2 2
           2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 2 2 2 2 2 2 2 2 6 6 11 9 9 11 11 9 9 9 9 9 9 11 6 9 12 12
           12 12 12 12 12 12 12 12 9 9 9 11 25 25 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 8 2
           2 2 2 2 8 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 20 5 6 6 6 11 11 11
           11 11 20 11 11 6 6 6 6 6 11 11 11 11 11 11 11 11 11 11 11 2
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 2 25 25 25
           25 25 25 25 25 11 25 25 25 25 25 25 2 25 25 20 20 20 20 20
           25 25 25 25 20 20 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 11 11 11 11 11 11 11 2 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 2 2 11 11 11 11
           11 11 11 2 11 11 2 11 11 11 11 11 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 11 9 9 9 2 6 6 6 6 6 6 6 6 2 6 6 6 2
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 2 2 2 6 11 11 11 9 9 9 9 2 11 11 11 2 11
           11 11 11 2 2 2 2 2 2 2 11 11 2 6 6 6 2 2 2 2 2 6 6 11 11 2 2
           12 12 12 12 12 12 12 12 12 12 2 2 2 2 2 2 2 2 14 14 14 14 14
           14 14 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 24 24 24 24 24 24 24 24 24 24
           24 24 24 24 24 24 24 24 24 24 24 25 25 24 24 24 24 24 24 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 2 2 25
           25 25 25 25 25 25 25 25 25 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 24 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 24 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 24 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 24 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 12 12
           12 12 12 12 12 12 12 12 2 2 2 2 20 20 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 25 25 25 25 11 25 25 20 20 20 20 20 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 11 11 11 11 11 2 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 11
           11 11 11 9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 11 9 11 11 11 11
           11 9 11 9 9 9 9 9 11 9 9 6 6 6 6 6 6 6 2 2 2 2 12 12 12 12
           12 12 12 12 12 12 20 20 20 20 20 20 20 25 25 25 25 25 25 25
           25 25 25 11 11 11 11 11 11 11 11 11 25 25 25 25 25 25 25 25
           25 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2
           2 2 2 2 2 2 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 2
           6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 2 6 6 6 6 6 6
           6 2 6 6 6 6 6 6 6 2 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 24 25 25 25 25 25
           25 25 25 25 24 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 24
           24 24 24 24 24 24 24 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 2 2 2 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 5 5 5 5 5 5 20 20 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 1 4 4 4 4 4
           4 4 4 4 24 4 4 4 4 4 4 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 24 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 24 4 4 4 4 4 4 8 4 2 2 12 12 12 12 12 12 12 12 12 12 12
           12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12
           12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 12 6 6
           6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 9 11 11 11 11 11 11 11 2 11 11
           11 11 11 11 9 11 6 20 20 20 20 20 2 2 2 2 2 2 2 2 2 2 12 12
           12 12 12 12 12 12 12 12 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 2 2 2 20 20 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           25 25 25 20 20 20 20 20 20 20 20 20 20 20 20 20 20 20 25 20
           25 25 25 11 11 25 25 25 25 25 25 12 12 12 12 12 12 12 12 12
           12 14 14 14 14 14 14 14 14 14 14 25 11 25 11 25 11 21 17 21
           17 9 9 6 6 6 6 6 6 6 6 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 9 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2
           2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2
           2 2 2 2 12 12 12 12 12 12 12 12 12 12 14 2 2 2 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
           25 25 25 25 25 25 25 25 25 25 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
           5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 2 11 11 11 11 11 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4
           4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
           4 4 4 11 11 11 11 11 11 11 2 2 2 2 2 12 12 12 12 12 12 12 12
           12 12 2 2 2 2 20 20 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4 4 4 2 20 16 2 2 25 25 22
           2 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11
           11 11 11 11 11 11 16 11 20 11 11 20 11 11 20 11 2 2 2 2 2 2
           2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2
           2 2 2 2 6 6 6 20 20 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2
           2 11 6 2 6 6 6 6 6 6 2 2 2 6 6 6 2 6 6 6 6 2 2 2 6 6 2 6 2 6
           6 2 2 2 6 6 2 2 2 6 6 6 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2
           2 9 9 11 9 9 2 2 2 9 9 9 2 9 9 9 11 2 2 6 2 2 2 2 2 2 9 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 12 12 12 12 12 12 12 12 12 12 14 14
           14 25 25 25 25 25 25 22 25 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 2 6 6 6 6 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 11 11 11 20 20 20
           20 20 20 20 20 20 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2
           2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
           2 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
           14 14 14 14 14 14 14 14 14 14 14 14 2 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 17 21 2 2 2 2 2 2
           2 2 2 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 25
           25 25 25 25 25 25 25 25 25 2 2 2 2 2 2 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
           8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 2 2 4 4 4 4 4 4 2 2 6 11 11 11
           2 11 11 2 2 2 2 2 11 11 11 11 6 6 6 6 2 6 6 6 2 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 11 11 11 2
           2 2 2 11 14 14 14 14 14 14 14 14 2 2 2 2 2 2 2 2 20 20 20 20
           20 20 20 20 20 2 2 2 2 2 2 2 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
           6 6 6 6 6 6 6 6 6 6 6 6 6 14 14 20 13 13 13 13 13 13 13 13
           13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
           13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
           13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
           13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
           13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13 13
           13 13 13 2 20 20 20 20 20 2 2 2 2 2 2 2 2 2 2 2))
  (define cats
    '#(Cc Cf Cn Co Ll Lm Lo Lt Lu Mc Me Mn Nd Nl No Pc Pd Pe Pf
          Pi Po Ps Sc Sk Sm So Zl Zp Zs))
  (define data-width 7)
  (define index-width 5)
  (define fxasr fxarithmetic-shift-right)
  (define fxasl fxarithmetic-shift-left)
  (define (bf n start end)
    (let ([mask (fxnot (fxasl -1 end))])
      (fxasr (fxand n mask) start)))
  (assert (eq? (native-endianness) 'little))
  (let* ([i (char->integer cp)]
         [l2 (bytevector-u16-native-ref
              cat-index1
              (fx* 2 (bf i (+ data-width index-width) 22)))]
         [l3 (bytevector-u16-native-ref
              cat-index2
              (fx* 2
                   (fx+ l2
                        (bf i data-width (+ data-width index-width)))))]
         [idx (bytevector-u8-ref
               cat-data
               (fx+ l3 (bf i 0 data-width)))])
    (vector-ref cats idx)))

(define (char-upcase c)
  (define index1
    '#(0 46 178 47 111 46 46 136 46 46 46 46 46 46 46 46 46 46 46
         46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46
         46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46
         46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46
         46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46
         46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46
         46 46 46 46 46 46 46 46 46))
  (define index2
    '#(4016 999 1247 1434 3096 3304 0 0 0 0 0 0 0 0 0 0 0 0 0 2186
            0 0 0 0 0 0 0 0 3669 4724 2441 1690 0 307 0 0 744 0 0 0 0 0
            0 0 4480 696 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0 2888 0 0 0 0 1934 0 0 0 0 0 0 0 64 0 0 0
            0 0 0 0 0 0 0 0 440 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4272 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
            0 3860 2697 0 0 0 3477 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
            0))
  (define data
    '#vu8(35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 56 56 56 56
             56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56
             56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56
             56 56 56 56 56 56 56 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 42 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 40
             40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 35 35 35 35 36
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 44 44 44 44 44 44 44 44
             44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 88 88 88 88
             88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88
             88 88 88 88 88 88 88 88 88 88 88 88 88 88 35 88 35 35 35 35
             35 88 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 41 41 41 41 41 41 41 41
             41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 77 35 36 35 36 35 36 35 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             35 36 35 36 35 36 78 19 35 35 36 35 36 35 35 36 35 35 35 36
             35 35 35 35 35 36 35 35 27 35 35 35 36 20 35 35 35 21 35 35
             36 35 36 35 36 35 35 36 35 35 35 35 36 35 35 36 35 35 35 36
             35 36 35 35 36 35 35 35 36 35 31 35 35 35 35 35 36 37 35 36
             37 35 36 37 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             59 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             35 36 37 35 36 35 35 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             35 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             35 35 35 35 35 35 35 36 35 35 10 10 35 36 35 35 35 35 36 35
             36 35 36 35 36 35 36 11 13 12 70 67 35 66 66 35 64 35 65 0
             35 35 35 66 1 35 68 35 5 2 35 69 71 2 15 3 35 35 71 35 14 72
             35 35 73 35 35 35 35 35 35 35 16 35 35 75 35 35 75 35 35 35
             4 75 57 74 74 58 35 35 35 35 35 76 35 35 35 35 35 35 35 35
             35 35 6 7 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             29 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 36 35 36 35 35 35 36 35 35 35 21 21 21 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 47 46 46 46 35 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 44 43 44 44 44 44 44 44 44 44 44 56 55 55 35 54
             52 35 35 35 49 51 38 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 61 60 34 63 35 62 35 35 36
             35 35 36 35 35 35 35 33 33 33 33 33 33 33 33 35 35 35 35 35
             35 35 35 33 33 33 33 33 33 35 35 35 35 35 35 35 35 35 35 33
             33 33 33 33 33 33 33 35 35 35 35 35 35 35 35 33 33 33 33 33
             33 33 33 35 35 35 35 35 35 35 35 33 33 33 33 33 33 35 35 35
             35 35 35 35 35 35 35 35 33 35 33 35 33 35 33 35 35 35 35 35
             35 35 35 33 33 33 33 33 33 33 33 35 35 35 35 35 35 35 35 30
             30 28 28 28 28 26 26 22 22 25 25 23 23 35 35 33 33 33 33 33
             33 33 33 35 35 35 35 35 35 35 35 33 33 33 33 33 33 33 33 35
             35 35 35 35 35 35 35 33 33 33 33 33 33 33 33 35 35 35 35 35
             35 35 35 33 33 35 32 35 35 35 35 35 35 35 35 35 35 87 35 35
             35 35 32 35 35 35 35 35 35 35 35 35 35 35 35 33 33 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 33 33 35 35 35 34 35 35 35
             35 35 35 35 35 35 35 35 35 35 32 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 48 48 48 48 48 48 48 48 48
             48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
             48 48 48 48 48 48 48 48 48 48 48 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 48 48 48 48 48 48 48 48 48 48 48 48 48
             48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
             48 48 48 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 38 38 38 38 38 38 35 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 35 35 35 35 53 35 35 35 35 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 35 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 36 35 35 35 35 35 35 35 35 35 35 36 35 36 35
             35 36 35 36 35 36 35 36 35 36 35 35 35 35 36 35 35 35 35 36
             35 36 35 35 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             35 36 35 36 35 35 35 35 35 35 35 35 35 35 35 36 35 36 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 44 44 44 44 44 44 44 44 44 60 60 60 60 60 60 60
             60 60 60 60 60 60 60 60 60 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 35 35 35 35 35 35 35 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 35 36 35 36 35 36 35 36 35 36 35 36 35 36
             39 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 50 50
             50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
             50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 79 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 91 91 91 91 91 91 91 91 91 91 91 91 91 91
             91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91
             91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91
             91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91
             91 91 91 91 91 91 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 86 85 84 82 82 83
             81 80 9 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 18 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 44 44 44
             44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             35 44 44 44 44 44 44 44 24 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45
             45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
             50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
             50 50 50 50 50 50 50 50 50 50 50 50 35 35 36 35 35 35 90 89
             35 36 35 36 35 36 35 35 35 35 35 35 36 35 35 36 35 35 35 35
             35 35 35 35 35 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35 36 35
             36 35 36 35 36 35 35 35 35 35 35 35 35 36 35 36 35 35 35 35
             36 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 8 35 35 35 17 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
             35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35))
  (define offsets
    '#(-42319 -42315 -42308 -42305 -42282 -42280 -42261 -42258
              -35332 -35266 -10815 -10783 -10782 -10780 -10749 -10743
              -10727 -3814 -743 -195 -163 -130 -128 -126 -121 -112 -100
              -97 -86 -84 -74 -56 -9 -8 -7 0 1 2 8 15 16 26 28 31 32 34 37
              38 40 47 48 54 57 59 62 63 64 69 71 79 80 86 96 116 202 203
              205 206 207 209 210 211 213 214 217 218 219 232 300 928 6181
              6236 6242 6243 6244 6253 6254 7205 7264 10792 10795 38864))
  (define data-width 8)
  (define index-width 6)
  (define fxasr fxarithmetic-shift-right)
  (define fxasl fxarithmetic-shift-left)
  (define (bf n start end)
    (let ([mask (fxnot (fxasl -1 end))])
      (fxasr (fxand n mask) start)))
  (assert (eq? (native-endianness) 'little))
  (let ([i (char->integer c)])
    (if (fx<=? (char->integer #\a) i (char->integer #\z))
        (integer->char
         (fxand
          i
          (fxnot (fxxor (char->integer #\A) (char->integer #\a)))))
        (let* ([l2 (vector-ref
                    index1
                    (bf i (+ data-width index-width) 22))]
               [l3 (vector-ref
                    index2
                    (fx+ l2
                         (bf i data-width (+ data-width index-width))))]
               [idx (bytevector-u8-ref data (fx+ l3 (bf i 0 data-width)))])
          (integer->char (fx- i (vector-ref offsets idx)))))))

(define (char-downcase c)
  (define index1
    '#(65 0 110 1 170 0 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          0 0 0 0 0 0 0 0 0 0 0))
  (define index2
    '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 3270 3334 846 2317 2461 2093 1357 0 0 0 0 0 0 0 0 0
         0 1101 0 0 96 0 0 0 0 0 0 0 0 0 0 2717 590 0 3833 0 0 3047 0
         0 0 0 0 0 0 3590 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1837 342 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 4089 0 0 0 0 0 0 0 1645 0 0 0 0
         0 0 0 0 0 0 0 1453 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2973 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (define data
    '#vu8(38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0 0 0 0 0 35 35 35 35 35 35 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 38 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 38 38 38 38 38 38 38 38 38 37 38 37 38
             68 37 38 37 38 37 38 37 38 37 38 38 38 38 37 38 71 38 38 37
             38 37 38 38 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 74 76 75 73 74 38 69 72 70 4 37 38 37 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 40 40 40 40 40 40 40 40 38
             38 38 38 38 38 38 38 40 40 40 40 40 40 38 38 38 38 38 38 38
             38 38 38 40 40 40 40 40 40 40 40 38 38 38 38 38 38 38 38 40
             40 40 40 40 40 40 40 38 38 38 38 38 38 38 38 40 40 40 40 40
             40 38 38 38 38 38 38 38 38 38 38 38 40 38 40 38 40 38 40 38
             38 38 38 38 38 38 38 40 40 40 40 40 40 40 40 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 40
             40 40 40 40 40 40 40 38 38 38 38 38 38 38 38 40 40 40 40 40
             40 40 40 38 38 38 38 38 38 38 38 40 40 40 40 40 40 40 40 38
             38 38 38 38 38 38 38 40 40 44 44 41 38 38 38 38 38 38 38 38
             38 38 38 45 45 45 45 41 38 38 38 38 38 38 38 38 38 38 38 40
             40 47 47 38 38 38 38 38 38 38 38 38 38 38 38 40 40 48 48 39
             38 38 38 38 38 38 38 38 38 38 38 51 51 50 50 41 38 38 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 55 38 37 38 37 38 37 38 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 49
             37 38 37 38 37 38 38 38 11 37 38 37 38 14 37 38 15 15 37 38
             38 20 17 16 37 38 15 13 38 10 12 37 38 38 38 10 9 38 8 37 38
             37 38 37 38 6 37 38 6 38 38 37 38 6 37 38 7 7 37 38 37 38 5
             37 38 38 38 37 38 38 38 38 38 38 38 36 37 38 36 37 38 36 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 38 36 37 38
             37 38 46 42 37 38 37 38 37 38 37 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 3 3 3 3 3 3 3 3 3 3 3 3 3 3
             3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 38 3 38 38
             38 38 38 3 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 38 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
             25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
             25 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 30 30 30 30 30 30 30 30 30 30
             30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30
             30 30 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 23 23 23 23 23 23 23 23 23 23
             23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23
             23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23
             23 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 19 19 19 19 19 19 19 19 19 19
             19 19 19 19 19 19 30 30 30 30 30 30 30 30 30 30 30 30 30 30
             30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             38 38 38 38 38 38 38 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 34 37 38 37 38 37 38 37 38 37 38 37 38 37 38 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 52 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 38 38 38 38 38 38 1 37 38 53 2 38 38 37 38
             54 22 21 37 38 37 38 37 38 37 38 37 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 37 38 37 38 38 38 37 38 38 38 38
             38 38 38 38 18 38 38 38 38 38 38 27 38 28 28 28 38 23 38 24
             24 38 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 38
             30 30 30 30 30 30 30 30 30 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 35 38 38 38 38 38 38 38 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 38 38 38
             38 43 38 38 37 38 39 37 38 38 52 52 52 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 38 38 38 38 38 38 38 38 58 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37
             38 37 38 37 38 37 38 37 38 29 29 29 29 29 29 29 29 29 29 29
             29 29 29 29 29 29 29 29 29 29 29 29 29 29 29 29 29 29 29 29
             29 29 29 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32
             32 32 32 32 32 32 32 32 32 32 32 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 30
             30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30
             30 30 30 30 30 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30
             30 30 30 30 30 38 30 30 30 30 30 30 30 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 25 25 25 25 25 25 25 25 25 25 25 25 25 25
             25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
             25 25 25 25 25 25 25 25 25 25 25 25 25 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 37 38 62 56 61 38 38 37 38 37 38 37 38 64 63 66 65 38
             37 38 38 37 38 38 38 38 38 38 38 38 67 67 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38 37 38
             37 38 37 38 37 38 37 38 37 38 37 38 37 38 38 38 38 38 38 38
             38 37 38 37 38 38 38 38 37 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 57 38 38 38 60 59 38 38 38 38 38 38 31
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 33 33 33 33 33 33 33 33 33 33 33 33 33 33 33
             33 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             37 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26
             26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26
             26 26 26 26 26 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26
             26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 26 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38 38
             38))
  (define offsets
    '#(-38864 -10795 -10792 -7264 -928 -219 -218 -217 -214 -213
              -211 -210 -209 -207 -206 -205 -203 -202 -116 -80 -79 -71 -69
              -64 -63 -48 -40 -38 -37 -34 -32 -28 -26 -16 -15 -8 -2 -1 0 7
              8 9 56 60 74 86 97 100 112 121 126 128 130 163 195 199 3814
              7517 7615 8262 8383 10727 10743 10749 10780 10782 10783
              10815 35332 42258 42261 42280 42282 42305 42308 42315
              42319))
  (define data-width 8)
  (define index-width 6)
  (define fxasr fxarithmetic-shift-right)
  (define fxasl fxarithmetic-shift-left)
  (define (bf n start end)
    (let ([mask (fxnot (fxasl -1 end))])
      (fxasr (fxand n mask) start)))
  (let ([i (char->integer c)])
    (if (fx<=? (char->integer #\A) i (char->integer #\Z))
        (integer->char
         (fxior i (fxxor (char->integer #\A) (char->integer #\a))))
        (let* ([l2 (vector-ref
                    index1
                    (bf i (+ data-width index-width) 22))]
               [l3 (vector-ref
                    index2
                    (fx+ l2
                         (bf i data-width (+ data-width index-width))))]
               [idx (bytevector-u8-ref data (fx+ l3 (bf i 0 data-width)))])
          (integer->char (fx- i (vector-ref offsets idx)))))))

(define (char-titlecase c)
  (define index1
    '#(196 0 132 1 65 0 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0))
  (define index2
    '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 3737 0 0 0 0 4470 0 0 0 0 0 0 0 2749 0 0 0 0 0 0 0 0
         0 0 0 398 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3226 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4254
         1234 0 0 0 4062 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 622
         1673 3004 2429 1929 263 0 0 0 0 0 0 0 0 0 0 0 0 0 8 0 0 0 0
         0 0 0 0 1418 1013 3481 2185 0 878 0 0 3828 0 0 0 0 0 0 0
         4722 2685 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (define data
    '#vu8(36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 38 38 38 38
             38 38 36 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
             50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 44
             44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 44 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 18 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 44 44 44 44 44 36 44 44 44 44 44 44 44 24 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 42 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 40 40 40 40 40 40 40 40 40 40
             40 40 40 40 40 40 36 36 36 36 37 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 8 36 36 36 17 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 36 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 36 36 36 36 36 36 36 36 36 37 36 37 36
             36 37 36 37 36 37 36 37 36 37 36 36 36 36 37 36 36 36 36 37
             36 37 36 36 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 36 36 36 36 36 36 36 36 36 36 37 36 37 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 86 85 84 82 82 83 81 80 9 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 77 36 37 36 37 36 37 36 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 36 37 36 37 36 37
             78 19 36 36 37 36 37 36 36 37 36 36 36 37 36 36 36 36 36 37
             36 36 27 36 36 36 37 20 36 36 36 21 36 36 37 36 37 36 37 36
             36 37 36 36 36 36 37 36 36 37 36 36 36 37 36 37 36 36 37 36
             36 36 37 36 31 36 36 36 36 35 36 37 35 36 37 35 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 59 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 35 36 37 36 37 36
             36 36 37 36 37 36 37 36 37 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 44 44 44
             44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 44 44 44 44 44 60 60 60 60 60 60 60 60 60 60 60
             60 60 60 60 60 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             36 36 36 36 36 36 36 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 36 37 36 37 36 37 36 37 36 37 36 37 36 37 39 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 33 33 33 33 33 33 33 33 36 36 36 36 36 36 36
             36 33 33 33 33 33 33 36 36 36 36 36 36 36 36 36 36 33 33 33
             33 33 33 33 33 36 36 36 36 36 36 36 36 33 33 33 33 33 33 33
             33 36 36 36 36 36 36 36 36 33 33 33 33 33 33 36 36 36 36 36
             36 36 36 36 36 36 33 36 33 36 33 36 33 36 36 36 36 36 36 36
             36 33 33 33 33 33 33 33 33 36 36 36 36 36 36 36 36 30 30 28
             28 28 28 26 26 22 22 25 25 23 23 36 36 33 33 33 33 33 33 33
             33 36 36 36 36 36 36 36 36 33 33 33 33 33 33 33 33 36 36 36
             36 36 36 36 36 33 33 33 33 33 33 33 33 36 36 36 36 36 36 36
             36 33 33 36 32 36 36 36 36 36 36 36 36 36 36 87 36 36 36 36
             32 36 36 36 36 36 36 36 36 36 36 36 36 33 33 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 33 33 36 36 36 34 36 36 36 36 36
             36 36 36 36 36 36 36 36 32 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 29 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 37 36 37 36 36 36 37 36 36 36 21 21 21 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 47 46 46 46 36 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 44 43 44 44 44 44 44 44 44 44 44 56 55 55 36 54 52 36
             36 36 49 51 38 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 61 60 34 63 36 62 36 36 37 36 36
             37 36 36 36 36 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88
             88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88
             88 88 88 36 88 36 36 36 36 36 88 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56
             56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56 56
             56 56 56 56 56 56 56 56 56 56 56 56 36 36 36 36 36 36 36 36
             36 36 36 36 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 36 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 36 36 36
             36 36 36 36 37 36 36 10 10 36 37 36 36 36 36 37 36 37 36 37
             36 37 36 37 11 13 12 70 67 36 66 66 36 64 36 65 0 36 36 36
             66 1 36 68 36 5 2 36 69 71 2 15 3 36 36 71 36 14 72 36 36 73
             36 36 36 36 36 36 36 16 36 36 75 36 36 75 36 36 36 4 75 57
             74 74 58 36 36 36 36 36 76 36 36 36 36 36 36 36 36 36 36 6 7
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 45 45 45
             45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45
             45 45 45 45 45 45 45 45 45 45 45 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 36 36 36 36 53 36 36 36 36 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37
             36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 44
             44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
             44 44 44 44 44 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 41 41 41 41 41 41 41
             41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 41 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 79 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 91 91 91 91 91 91 91 91 91
             91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91
             91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91
             91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91 91
             91 91 91 91 91 91 91 91 91 91 91 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 48 48 48 48 48 48 48 48 48 48 48 48 48
             48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
             48 48 48 48 48 48 48 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
             48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36
             36 36 36 36 36 36 36 50 50 50 50 50 50 50 50 50 50 50 50 50
             50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
             50 50 50 50 50 50 50 50 50 50 50 50 50 50 36 36 37 36 36 36
             90 89 36 37 36 37 36 37 36 36 36 36 36 36 37 36 36 37 36 36
             36 36 36 36 36 36 36 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36 37 36
             37 36 37 36 37 36 37 36 36 36 36 36 36 36 36 37 36 37 36 36
             36 36 37 36 36 36 36 36 36 36 36 36 36 36 36))
  (define offsets
    '#(-42319 -42315 -42308 -42305 -42282 -42280 -42261 -42258
              -35332 -35266 -10815 -10783 -10782 -10780 -10749 -10743
              -10727 -3814 -743 -195 -163 -130 -128 -126 -121 -112 -100
              -97 -86 -84 -74 -56 -9 -8 -7 -1 0 1 8 15 16 26 28 31 32 34
              37 38 40 47 48 54 57 59 62 63 64 69 71 79 80 86 96 116 202
              203 205 206 207 209 210 211 213 214 217 218 219 232 300 928
              6181 6236 6242 6243 6244 6253 6254 7205 7264 10792 10795
              38864))
  (define data-width 8)
  (define index-width 6)
  (define fxasr fxarithmetic-shift-right)
  (define fxasl fxarithmetic-shift-left)
  (define (bf n start end)
    (let ([mask (fxnot (fxasl -1 end))])
      (fxasr (fxand n mask) start)))
  (let ([i (char->integer c)])
    (let* ([l2 (vector-ref
                index1
                (bf i (+ data-width index-width) 22))]
           [l3 (vector-ref
                index2
                (fx+ l2 (bf i data-width (+ data-width index-width))))]
           [idx (bytevector-u8-ref data (fx+ l3 (bf i 0 data-width)))])
      (integer->char (fx- i (vector-ref offsets idx)))))))
