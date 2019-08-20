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

;;; Thin wrappers around Linux system calls.

(library (loko arch amd64 linux-syscalls)
  (export
    make-syscall-error

    sys_accept4
    sys_bind
    sys_clock_getres
    sys_clock_gettime
    sys_close
    ;; sys_dup, sys_dup2 and sys_dup3: use fcntl
    sys_epoll_create1
    sys_epoll_ctl
    sys_epoll_pwait
    sys_exit
    sys_faccessat
    sys_fcntl
    sys_fork
    sys_fstat
    sys_getdents64
    sys_getsockopt
    sys_ioctl
    sys_listen
    sys_lseek
    sys_lstat
    sys_mmap
    sys_open
    sys_read
    sys_readlink
    sys_recvfrom
    sys_rt_sigaction
    sys_rt_sigprocmask
    sys_sendto
    sys_setsockopt
    sys_shmat
    sys_shutdown
    sys_sigaltstack
    sys_signalfd4
    sys_socket
    sys_stat
    sys_timer_create
    sys_timer_settime
    sys_wait4
    sys_write
    )
  (import
    (rnrs (6))
    (loko system unsafe)
    (loko arch amd64 linux-numbers))

(define-condition-type &syscall-errno &error
   make-syscall-error* syscall-errno-condition?
   (function condition-syscall-function)
   (errno condition-syscall-errno)
   (symbol condition-syscall-symbol)
   (message condition-syscall-message))

(define (make-syscall-error who errno)
  (cond ((and (fx<? errno (vector-length errno-list)) (vector-ref errno-list errno))
         => (lambda (x)
              (make-syscall-error* who errno (car x) (cdr x))))
        (else (make-syscall-error* who errno #f #f))))

;; TODO: extra "safe" variants of syscalls that check bytevector limits

(define-syntax define-syscall
  (lambda (x)
    (define (symcat prefix name)
      (string->symbol (string-append prefix (symbol->string (syntax->datum name)))))
    (syntax-case x ()
      ((_ (name arg ...))
       (with-syntax ([__NR (datum->syntax #'name (symcat "__NR_" #'name))]
                     [sys_name (datum->syntax #'name (symcat "sys_" #'name))]
                     [(tmp ...) (generate-temporaries #'(arg ...))])
         #'(define sys_name
             (case-lambda
               [(arg ...)
                ;; Default: raise errors automatically
                (let ((tmp arg) ...)
                  (let retry ()
                    (let ((v (syscall __NR tmp ...)))
                      (if (fx<=? -4095 v -1)
                          (let ((errno (fx- v)))
                            (if (eqv? errno EINTR)
                                (retry)   ;see "Worse is Better" for an explanation
                                (raise (condition (make-syscall-error 'name errno)
                                                  (make-irritants-condition (list tmp ...))))))
                          v))))]
               [(arg ... k-failure)
                ;; Let the caller handle errors
                (let ((v (syscall __NR arg ...)))
                  (if (fx<=? -4095 v -1)
                      (k-failure (fx- v))
                      v))]
               #;
               [(arg ... k-failure k-success)
                (let ((v (syscall __NR arg ...)))
                  (if (fx<=? -4095 v -1)
                      (k-failure (fx- v))
                      (k-success v)))])))))))

(define-syscall (accept4 fd *upeer_sockaddr *upeer_addrlen flags))
(define-syscall (bind sockfd *umyaddr addrlen))
(define-syscall (clock_getres clk-id *tp))
(define-syscall (clock_gettime clk-id *tp))
(define-syscall (connect fd *uservaddr addrlen))
(define-syscall (close fd))
(define-syscall (epoll_create1 flags))
(define-syscall (epoll_ctl epfd op fd *event))
(define-syscall (epoll_pwait epfd *events maxevents timeout *sigmask))
(define-syscall (exit status))
(define-syscall (faccessat dfd *filename mode))
(define-syscall (fcntl fd cmd maybe-arg))
(define-syscall (fork))
(define-syscall (fstat fd *statbuf))
(define-syscall (getdents64 fd *dirent count))
(define-syscall (getsockopt fd level optname *optval *optlen))
(define-syscall (ioctl fd request *buf))
(define-syscall (listen fd backlog))
(define-syscall (lseek fd offset whence))
(define-syscall (lstat *pathname *statbuf))
(define-syscall (mmap addr length prot flags fd offset))
(define-syscall (open *pathname flags mode))
(define-syscall (preadv fd *vec vlen pos_l pos_h))
(define-syscall (pwritev fd *vec vlen pos_l pos_h))
(define-syscall (read fd *buf count))
(define-syscall (readlink *path *buf bufsiz))
(define-syscall (recvfrom fd *ubuf size flags *addr *addr_len))
(define-syscall (rt_sigaction signal act oact sigsetsize))
(define-syscall (rt_sigprocmask how *set *oldset sigsetsize))
(define-syscall (sendto fd *buff len flags *addr addr_len))
(define-syscall (setsockopt fd level optname *optval optlen))
(define-syscall (shmat shmid *shmaddr shmflg))
(define-syscall (shutdown fd how))
(define-syscall (sigaltstack uss uoss))
(define-syscall (signalfd4 ufd *user_mask sizemask flags))
(define-syscall (socket family type protocol))
(define-syscall (stat *pathname *statbuf))
(define-syscall (timer_create clock event-spec created-id))
(define-syscall (timer_settime timer flags new-setting old-setting))
(define-syscall (wait4 pid *status options *rusage))
(define-syscall (write fd *buf count))
)
