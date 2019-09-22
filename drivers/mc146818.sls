;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: AGPL-3.0-or-later
#!r6rs

;;; Real-time-clock driver

;; The RTC in the IBM PC. Maybe a Motorola MC146818B. Or is it a
;; DS1387? Either way it's a product of its time and is showing its
;; age. It's embedded in the motherboard now, e.g. in the ICH chip.

;; "Real-Time Clock Plus RAM (RTC)" MC146818A/D
;; <https://www.nxp.com/docs/en/data-sheet/MC146818A.pdf>

;; "Accessing the Real Time Clock Registers and the NMI Enable Bit"
;; <ftp://download.intel.com/design/intarch/PAPERS/321088.pdf>

;; "IBM PC Real Time Clock should run in UT"
;; <https://www.cl.cam.ac.uk/~mgk25/mswish/ut-rtc.html>

;; TODO: Consider setting the chip to 24-hour mode and binary.

(library (loko drivers rtc)
  (export
    driver-rtc)
  (import
    (rnrs (6))
    (loko system fibers)
    (loko system unsafe))

;; TODO: Figure out what API is needed and add channels
(define (driver-rtc)
  ;; I/O ports
  (define reg-index #x70)
  (define reg-data #x71)
  (define reg-ext-index #x72)
  (define reg-ext-data #x73)
  (define bit7 #b10000000)

  ;; Read the RTC RAM
  (define (rtc-read i)
    ;; If bit7 = 0 then NMI is disabled.
    (put-i/o-u8 reg-index (fx+ bit7 i))
    (get-i/o-u8 reg-data))

  ;; Write the RTC RAM
  (define (rtc-write i x)
    (put-i/o-u8 reg-index (fx+ bit7 i))
    (put-i/o-u8 reg-data x))

  ;; RTC RAM indices
  (define RTC_Seconds       #x00)
  (define RTC_Seconds_Alarm #x01)
  (define RTC_Minutes       #x02)
  (define RTC_Minutes_Alarm #x03)
  (define RTC_Hours         #x04)
  (define RTC_Hours_Alarm   #x05)
  (define RTC_Day_of_Week   #x06)
  (define RTC_Day_of_Month  #x07)
  (define RTC_Month         #x08)
  (define RTC_Year          #x09)
  (define RTC_RegA          #x0A)
  (define RTC_RegB          #x0B)
  (define RTC_RegC          #x0C)
  (define RTC_RegD          #x0D)
  (define RTC_Century       #x32)       ;maybe

  ;; Cached values
  (define A (rtc-read RTC_RegA))
  (define B (rtc-read RTC_RegB))
  (define C (rtc-read RTC_RegC))
  (define D (rtc-read RTC_RegD))
  (define century
    (let ((c (decode-byte (rtc-read RTC_Century))))
      (if (fx<? c 19) 2000 (fx* 100 c))))

  ;; Decode a number which based on bit 2 of RegB can be BCD or binary.
  (define (decode-byte x)
    (if (fxzero? (fxand B #b100))
        (fx+ (fx* (fxarithmetic-shift-right x 4) 10)
             (fxand x #xf))
        x))

  ;; The chip can be made to report the hours in 24-hour or 12-hour
  ;; format.
  (define (decode-hour x)
    (let ((h (decode-byte (fxbit-field x 0 7))))
      (if (fxzero? (fxand B #b10))
          ;; In 12-hour mode the chip divides the day into the four
          ;; parts shown here.
          (if (fxbit-set? x 7)          ;later part of the day?
              (if (eqv? h 12)
                  h
                  (fx+ h 12))
              (if (eqv? h 12)
                  0
                  h))
          h)))                          ;already in 24-hour time

  ;; The day of the week is stored off-by-two.
  (define (decode-day-of-week x)
    (fxmod (fx- (decode-byte x) 2) 7))

  (define (read-date)
    (let lp ()
      (unless (fxzero? (fxand (rtc-read #xA) #b10000000))
        (lp)))

    (list (vector (decode-hour (rtc-read RTC_Hours))
                  (decode-byte (rtc-read RTC_Minutes))
                  (decode-byte (rtc-read RTC_Seconds)))
          (decode-day-of-week (rtc-read RTC_Day_of_Week))
          (vector (fx+ century (decode-byte (rtc-read RTC_Year)))
                  (decode-byte (rtc-read RTC_Month))
                  (decode-byte (rtc-read RTC_Day_of_Month)))))

  (when (not (fxbit-set? D 7))
    (display "The RTC reports a problem with the battery\n"))

  (write (read-date))
  (newline))

)
