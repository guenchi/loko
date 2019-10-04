#!/usr/bin/env scheme-script
;; Loko Scheme sample
;; Copyright © 2019 Göran Weinholt
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (ip-address)
  (struct pack)
  (only (loko) port-file-descriptor-set!)
  (loko system fibers)

  ;; XXX: It would be preferable to package up the syscalls in a nicer
  ;; library
  (loko system unsafe)
  (loko arch amd64 linux-syscalls)
  (loko arch amd64 linux-numbers))

;;; Syscall interface

(define NULL 0)

(define (sockaddr_in6->string buf buflen)
  (let ((ip (make-bytevector 16)))
    (bytevector-copy! buf offsetof-sockaddr_in6-sin6_addr ip 0 16)
    (let ((port (unpack "!S" buf offsetof-sockaddr_in6-sin6_port)))
      (string-append "[" (ipv6->string ip) "]:" (number->string port)))))

(define (sendto fd buf start count)
  (define flags 0)
  (assert (fx<=? 0 (fx+ start count) (bytevector-length buf)))
  (sys_sendto fd (fx+ (bytevector-address buf) start) count
              flags NULL 0
              (lambda (errno)
                (cond ((eqv? errno EAGAIN)
                       (wait-for-writable fd)
                       (sendto fd buf start count))
                      ((eqv? errno EINTR)
                       (sendto fd buf start count))
                      (else
                       (raise (condition
                               (make-syscall-error 'sendto errno)
                               (make-irritants-condition (list fd)))))))))

(define (recvfrom fd buf start count)
  (assert (fx<=? 0 (fx+ start count) (bytevector-length buf)))
  (sys_recvfrom fd (bytevector-address buf) (bytevector-length buf)
                0 NULL 0
                (lambda (errno)
                  (cond ((eqv? errno EAGAIN)
                         (wait-for-readable fd)
                         (recvfrom fd buf start count))
                        ((eqv? errno EINTR)
                         (recvfrom fd buf start count))
                        (else
                         (raise (condition
                                 (make-syscall-error 'recvfrom errno)
                                 (make-irritants-condition (list fd)))))))))

(define (accept fd addr addrlen flags)
  (sys_accept4 fd (bytevector-address addr) (bytevector-address addrlen) flags
               (lambda (errno)
                 (cond ((eqv? errno EAGAIN)
                        (wait-for-readable fd)
                        (accept fd addr addrlen flags))
                       ((eqv? errno EINTR)
                        (accept fd addr addrlen flags))
                       (else
                        (raise (condition
                                (make-syscall-error 'accept4 errno)
                                (make-irritants-condition
                                 (list fd addr addrlen flags)))))))))

(define (setsockopt fd level optname optval)
  (sys_setsockopt fd level optname (bytevector-address optval) (bytevector-length optval)))

;;; Socket ports

(define (fdes->binary-input-port fd)
  (define (read! bv start count)
    (recvfrom fd bv start count))
  (define (close)
    (sys_close fd))
  (let ((p (make-custom-binary-input-port (string-append "fd " (number->string fd))
                                          read! #f #f close)))
    (port-file-descriptor-set! p fd)
    p))

(define (fdes->textual-input-port fd)
  (transcoded-port (fdes->binary-input-port fd)
                   (native-transcoder)))

(define (fdes->binary-output-port fd)
  (define (write! bv start count)
    (sendto fd bv start count))
  (define (close)
    (sys_close fd))
  (let ((p (make-custom-binary-output-port (string-append "fd " (number->string fd))
                                           write! #f #f close)))
    (port-file-descriptor-set! p fd)
    p))

(define (fdes->textual-output-port fd)
  (transcoded-port (fdes->binary-output-port fd)
                   (native-transcoder)))

#;
(define (fdes->binary-input/output-port fd)
  (define (read! bv start count)
    (recvfrom fd bv start count))
  (define (write! bv start count)
    (sendto fd bv start count))
  (define (close)
    (sys_close fd))
  (let ((p (make-custom-binary-input/output-port (string-append "fd " (number->string fd))
                                                 read! write! #f #f close)))
    (port-file-descriptor-set! p fd)
    p))

;;; Web server

(define (http-client-handler in out peername)
  ;; XXX: This isn't the right way to do things, but it happens to
  ;; accidentally work most of the time.
  (let lp ()
    (let ((req (get-bytevector-some in)))
      (unless (eof-object? req)
        ;; Commented out to work around #20:
        ;; (write (utf8->string req))
        ;; (newline)
        (display "HTTP/1.1 200 OK\r\n" out)
        (display "Content-Length: 6\r\n" out)
        ;; (display "Connection: close\r\n" out)
        (display "\r\n" out)
        (display "SCHEME" out)
        (flush-output-port out)
        (lp)))))

(define (test-server port)
  (define backlog 1000)
  (define sfd (sys_socket AF_INET6 (fxior SOCK_STREAM SOCK_CLOEXEC SOCK_NONBLOCK) IPPROTO_IP))
  (setsockopt sfd SOL_SOCKET SO_REUSEADDR (pack "l" 1))
  ;; bind
  (let ((addr (make-bytevector sizeof-sockaddr_in6)))
    (bytevector-u16-native-set! addr offsetof-sockaddr_in6-sin6_family AF_INET6)
    (bytevector-u16-set! addr offsetof-sockaddr_in6-sin6_port port (endianness big))
    (sys_bind sfd (bytevector-address addr) (bytevector-length addr)))
  ;; listen
  (sys_listen sfd backlog)
  ;; accept loop
  (display "Waiting for connections on port ")
  (display port)
  (newline)
  (let ((addr (make-bytevector sizeof-sockaddr_in6))
        (addrlen (pack "q" sizeof-sockaddr_in6)))
    (let lp ()
      ;; TODO: Catch or protect against EMFILE
      (let* ((fd (accept sfd addr addrlen (bitwise-ior SOCK_NONBLOCK SOCK_CLOEXEC)))
             (peername (sockaddr_in6->string addr (unpack "q" addr))))
        (display "New connection from ")
        (display peername)
        (newline)
        (spawn-fiber (lambda ()
                       ;; Disable Nagle's algorithm
                       (setsockopt fd SOL_TCP TCP_NODELAY (pack "l" 1))
                       (let ((out (fdes->textual-output-port fd))
                             (in (fdes->binary-input-port fd)))
                         (guard (exn
                                 ((condition? exn)
                                  (write exn)
                                  (newline)
                                  (close-port in)))
                           (http-client-handler in out peername)
                           (close-port in)))))
        (yield-current-task)
        (lp))))
  (sys_close sfd))

(run-fibers
 (lambda ()
   (test-server 3000)))
