#!/usr/bin/env scheme-script
;; -*- coding: utf-8; mode: scheme -*-
;; Copyright © 2018, 2019 Göran Weinholt
;; SPDX-License-Identifier: MIT

;;; C header snarfer for syscall ABIs

#|
cat /lib/modules/`uname -r`/source/include/uapi/asm-generic/errno* | \
  scheme --program tools/header-snarfer.scm loko arch | \
  tee header-snarfer.c && \
  gcc -Wall header-snarfer.c -o header-snarfer && \
  ./header-snarfer | tee src/loko/arch/amd64/linux-numbers.sls && \
  /bin/rm -f header-snarfer.c header-snarfer
|#

(import (rnrs))

(define instructions
  '((c-include "asm/unistd.h")
    __NR_arch_prctl
    __NR_clock_gettime
    __NR_close
    __NR_epoll_create1
    __NR_epoll_ctl
    __NR_epoll_pwait
    __NR_epoll_wait
    __NR_exit
    __NR_faccessat
    __NR_fork
    __NR_ioctl
    __NR_lseek
    __NR_mmap
    __NR_open
    __NR_preadv
    __NR_pwritev
    __NR_read
    __NR_rt_sigaction
    __NR_rt_sigprocmask
    __NR_rt_sigreturn
    __NR_shmat
    __NR_sigaltstack
    __NR_signalfd
    __NR_stat
    __NR_timer_create
    __NR_timer_settime
    __NR_wait4
    __NR_write

    (c-include "asm/errno.h")
    EPERM
    ENOENT
    ESRCH
    EINTR
    EIO
    ENXIO
    E2BIG
    ENOEXEC
    EBADF
    ECHILD
    EAGAIN
    ENOMEM
    EACCES
    EFAULT
    ENOTBLK
    EBUSY
    EEXIST
    EXDEV
    ENODEV
    ENOTDIR
    EISDIR
    EINVAL
    ENFILE
    EMFILE
    ENOTTY
    ETXTBSY
    EFBIG
    ENOSPC
    ESPIPE
    EROFS
    EMLINK
    EPIPE
    EDOM
    ERANGE
    EDEADLK
    ENAMETOOLONG
    ENOLCK
    ENOSYS

    ENOTEMPTY
    ELOOP
    EWOULDBLOCK
    ENOMSG
    EIDRM
    ECHRNG
    EL2NSYNC
    EL3HLT
    EL3RST
    ELNRNG
    EUNATCH
    ENOCSI
    EL2HLT
    EBADE
    EBADR
    EXFULL
    ENOANO
    EBADRQC
    EBADSLT
    EDEADLOCK
    EBFONT
    ENOSTR
    ENODATA
    ETIME
    ENOSR
    ENONET
    ENOPKG
    EREMOTE
    ENOLINK
    EADV
    ESRMNT
    ECOMM
    EPROTO
    EMULTIHOP
    EDOTDOT
    EBADMSG
    EOVERFLOW
    ENOTUNIQ
    EBADFD
    EREMCHG
    ELIBACC
    ELIBBAD
    ELIBSCN
    ELIBMAX
    ELIBEXEC
    EILSEQ
    ERESTART
    ESTRPIPE
    EUSERS
    ENOTSOCK
    EDESTADDRREQ
    EMSGSIZE
    EPROTOTYPE
    ENOPROTOOPT
    EPROTONOSUPPORT
    ESOCKTNOSUPPORT
    EOPNOTSUPP
    EPFNOSUPPORT
    EAFNOSUPPORT
    EADDRINUSE
    EADDRNOTAVAIL
    ENETDOWN
    ENETUNREACH
    ENETRESET
    ECONNABORTED
    ECONNRESET
    ENOBUFS
    EISCONN
    ENOTCONN
    ESHUTDOWN
    ETOOMANYREFS
    ETIMEDOUT
    ECONNREFUSED
    EHOSTDOWN
    EHOSTUNREACH
    EALREADY
    EINPROGRESS
    ESTALE
    EUCLEAN
    ENOTNAM
    ENAVAIL
    EISNAM
    EREMOTEIO
    EDQUOT
    ENOMEDIUM
    EMEDIUMTYPE
    ECANCELED
    ENOKEY
    EKEYEXPIRED
    EKEYREVOKED
    EKEYREJECTED
    EOWNERDEAD
    ENOTRECOVERABLE
    ERFKILL
    EHWPOISON

    (c-include "asm/ioctls.h")
    (fmt "#x%x")
    TCGETS
    TCSETS
    TCSETSW
    TCSETSF
    TIOCGWINSZ

    (c-include "asm/termios.h")
    (struct winsize ws_row ws_col ws_xpixel ws_ypixel)

    (c-include "unistd.h")
    STDIN_FILENO
    STDOUT_FILENO
    STDERR_FILENO

    (ifdef "__amd64__")
    (c-include "asm/prctl.h")
    (fmt "#x%x")
    ARCH_SET_GS
    ARCH_SET_FS
    ARCH_GET_FS
    ARCH_GET_GS
    (endif)

    (c-include "asm/termbits.h")
    (fmt "#o%o")
    (struct termios c_iflag c_oflag c_cflag c_lflag c_line c_cc)
    (comment "c_iflag")
    IGNBRK
    BRKINT
    IGNPAR
    PARMRK
    INPCK
    ISTRIP
    INLCR
    IGNCR
    ICRNL
    IUCLC
    IXON
    IXANY
    IXOFF
    IMAXBEL
    (comment "c_oflag")
    OPOST
    (comment "c_cflag")
    CS8
    (comment "c_lflag")
    ISIG
    ICANON
    XCASE
    ECHO
    ECHOE
    ECHOK
    ECHONL
    NOFLSH
    TOSTOP
    ECHOCTL
    ECHOPRT
    ECHOKE
    FLUSHO
    PENDIN
    IEXTEN
    EXTPROC
    (comment "c_cc") (fmt "%d")
    NCCS
    VINTR
    VQUIT
    VERASE
    VKILL
    VEOF
    VTIME
    VMIN
    VSWTC
    VSTART
    VSTOP
    VSUSP
    VEOL
    VREPRINT
    VDISCARD
    VWERASE
    VLNEXT
    VEOL2
    (comment "tcsetattr")
    TCSANOW
    TCSADRAIN
    TCSAFLUSH

    (c-include "asm/signal.h") (fmt "%d")
    SIGHUP
    SIGINT
    SIGQUIT
    SIGILL
    SIGTRAP
    SIGABRT
    SIGIOT
    SIGBUS
    SIGFPE
    SIGKILL
    SIGUSR1
    SIGSEGV
    SIGUSR2
    SIGPIPE
    SIGALRM
    SIGTERM
    SIGSTKFLT
    SIGCHLD
    SIGCONT
    SIGSTOP
    SIGTSTP
    SIGTTIN
    SIGTTOU
    SIGURG
    SIGXCPU
    SIGXFSZ
    SIGVTALRM
    SIGPROF
    SIGWINCH
    SIGIO
    (fmt "#x%x")
    SA_NOCLDSTOP
    SA_NOCLDWAIT
    SA_SIGINFO
    SA_ONSTACK
    SA_RESTART
    SA_NODEFER
    SA_RESETHAND
    SA_RESTORER                         ;obsolete?
    (struct sigaction sa_handler sa_flags sa_restorer sa_mask)
    (struct sigaltstack ss_sp ss_flags ss_size)
    (sizeof sigset_t)

    (c-include "asm/sigcontext.h")
    (ifdef __amd64__)
    (struct sigcontext_64 r8 r9 r10 r11 r12 r13 r14 r15
            di si bp bx dx ax cx sp ip flags err trapno cr2)
    (endif)

    (c-include "asm/ucontext.h")
    (struct ucontext uc_flags uc_link uc_stack uc_mcontext uc_sigmask)

    (c-include "asm/siginfo.h")
    (fmt "#x%x")
    SI_USER
    SI_KERNEL
    (fmt "%d")
    SEGV_MAPERR
    SEGV_ACCERR
    BUS_ADRALN

    SIGEV_SIGNAL
    (struct sigevent sigev_value sigev_signo sigev_notify)

    (c-include "linux/mman.h")
    (fmt "#x%x")
    MAP_SHARED
    MAP_PRIVATE
    MAP_FIXED
    MAP_ANONYMOUS
    MAP_32BIT
    MAP_GROWSDOWN
    MAP_STACK
    PROT_NONE
    PROT_READ
    PROT_WRITE
    PROT_EXEC

    (c-include "linux/shm.h")
    (fmt "#o%o")
    SHM_REMAP

    (c-include "linux/time.h")
    CLOCK_REALTIME
    CLOCK_MONOTONIC
    CLOCK_THREAD_CPUTIME_ID
    (struct itimerspec it_interval it_value)
    (struct timespec tv_sec tv_nsec)

    (c-include "linux/auxvec.h")
    AT_NULL
    AT_IGNORE
    AT_EXECFD
    AT_PHDR
    AT_PHENT
    AT_PHNUM
    AT_PAGESZ
    AT_BASE
    AT_FLAGS
    AT_ENTRY
    AT_NOTELF
    AT_UID
    AT_EUID
    AT_GID
    AT_EGID
    AT_CLKTCK
    AT_PLATFORM
    AT_HWCAP
    AT_SECURE
    AT_BASE_PLATFORM
    AT_RANDOM
    AT_HWCAP2
    AT_EXECFN
    AT_SYSINFO_EHDR
    AT_VECTOR_SIZE_ARCH

    (c-include "asm/fcntl.h")
    AT_FDCWD
    (fmt "#o%o")
    O_RDONLY
    O_WRONLY
    O_RDWR
    O_CREAT
    O_EXCL
    O_NOCTTY
    O_TRUNC
    O_APPEND
    O_NONBLOCK
    O_DSYNC
    O_DIRECT
    O_LARGEFILE
    O_DIRECTORY
    O_NOFOLLOW
    O_NOATIME
    O_CLOEXEC
    O_SYNC
    O_PATH
    O_TMPFILE
    (fmt "#x%x")
    AT_SYMLINK_NOFOLLOW
    AT_REMOVEDIR
    AT_SYMLINK_FOLLOW
    AT_NO_AUTOMOUNT
    AT_EMPTY_PATH
    AT_STATX_SYNC_TYPE
    AT_STATX_SYNC_AS_STAT
    AT_STATX_FORCE_SYNC
    AT_STATX_DONT_SYNC

    (c-include "linux/wait.h")
    WNOHANG
    WUNTRACED
    WEXITED
    WCONTINUED
    WNOWAIT

    (c-include "linux/eventpoll.h")
    (struct epoll_event events data)
    (fmt "#x%x")
    EPOLLIN
    EPOLLPRI
    EPOLLOUT
    EPOLLERR
    EPOLLHUP
    EPOLLNVAL
    EPOLLRDNORM
    EPOLLRDBAND
    EPOLLWRNORM
    EPOLLWRBAND
    EPOLLMSG
    EPOLLRDHUP
    EPOLLEXCLUSIVE
    EPOLLWAKEUP
    EPOLLONESHOT
    EPOLLET

    (c-include "linux/fs.h")
    (fmt "%d")
    SEEK_SET
    SEEK_CUR
    SEEK_END
    SEEK_DATA
    SEEK_HOLE

    ;; Not really part of the Linux ABI, but says how to use exit()
    (c-include "sysexits.h")
    EX_OK
    EX_USAGE
    EX_DATAERR
    EX_NOINPUT
    EX_NOUSER
    EX_NOHOST
    EX_UNAVAILABLE
    EX_SOFTWARE
    EX_OSERR
    EX_OSFILE
    EX_CANTCREAT
    EX_IOERR
    EX_TEMPFAIL
    EX_PROTOCOL
    EX_NOPERM
    EX_CONFIG

    ))

(define (print . x) (for-each display x) (newline))

(define (print-include x)
  (when (pair? x)
    (print "#ifdef " (cadr x)))
  (print "#include <" (if (pair? x) (car x) x) ">")
  (when (pair? x)
    (print "#endif")))

(define (includes instructions)
  (for-each
   (lambda (x)
     (when (pair? x)
       (case (car x)
         ((ifdef)
          (print "#ifdef " (cadr x)))
         ((c-include)
          (print-include (cadr x)))
         ((endif)
          (print "#endif")))))
   instructions))

(define (lib-start prefix instructions imports)
  (print "    printf(\"(library (\");")
  (for-each (lambda (x)
              (print "    printf(\"" x " \");"))
            prefix)
  (print "#ifdef __amd64__")
  (print "    printf(\"amd64\");")
  (print "#endif")
  (print "#ifdef __aarch64__")
  (print "    printf(\"aarch64\");")
  (print "#endif")
  (print "    struct utsname uts;")
  (print "    uname(&uts);")
  (print "    for (char *p=uts.sysname; *p; p++) *p = tolower(*p);")
  (print "    printf(\" %s\", uts.sysname);")
  (print "    printf(\"-numbers)\\n\");")
  (print "    printf(\"  (export\");")
  (for-each
   (lambda (x)
     (if (pair? x)
         (case (car x)
           ((ifdef)
            (print "#ifdef " (cadr x)))
           ((endif)
            (print "#endif"))
           ((struct)
            (let ((struct (cadr x))
                  (members (cddr x)))
              (print "    printf(\"\\n    sizeof-" struct "\");")
              (for-each
               (lambda (member)
                 (print "    printf(\"\\n    offsetof-" struct "-" member "\");"))
               members)))
           ((sizeof)
            (let ((type (cadr x)))
              (print "    printf(\"\\n    sizeof-" type "\");"))))
         (print "    printf(\"\\n    " x "\");")))
   (cons 'errno-list instructions))
  (print "    printf(\")\\n\");")
  (print "    printf(\"  (import\");")
  (for-each (lambda (imp)
              (print "    printf(\" " imp "\");"))
            imports)
  (print "    printf(\")\\n\");")
  (printf)
  (printf "(define-syntax define-inlined")
  (printf "  (syntax-rules ()")
  (printf "    ((_ name v)")
  (printf "     (define-syntax name (identifier-syntax v)))))"))

(define (lib-end)
  (print "    printf(\")\\n\");"))

(define (printf-const fmt name)
  (print "    printf(\"(define-inlined " name " " fmt ")\\n\", " name ");"))

(define (printf-struct struct members)
  (print "    printf(\"(define-inlined sizeof-" struct
         " %lu)\\n\", sizeof(struct " struct "));")
  (for-each
   (lambda (member)
     (print "    printf(\"(define-inlined offsetof-" struct "-" member
         " %lu)\\n\", offsetof(struct " struct ", " member "));"))
   members))

(define (printf-sizeof type)
  (print "    printf(\"(define-inlined sizeof-" type
         " %lu)\\n\", sizeof(" type "));"))

(define (printf . x*)
  (print "    printf(\"" (apply string-append
                                (map (lambda (x)
                                       (if (symbol? x)
                                           (symbol->string x)
                                           x))
                                     x*))
         "\\n\");"))

(define (return v)
  (print "    return " v ";"))

(print-include "ctype.h")                     ;tolower
(print-include "stdio.h")                     ;printf
(print-include "sys/utsname.h")               ;uname
(includes instructions)
(print "#define offsetof(TYPE, MEMBER) __builtin_offsetof (TYPE, MEMBER)")

(print "int main(int argc, char *argv[]) {")

(printf ";; -*- mode: scheme; coding: utf-8 -*-")
(printf ";; Automatically generated by header-snarfer.scm")
(printf ";; This file is not a copyrightable work. These definitions")
(printf ";; are required for interoperability. If you disagree:")
(printf ";; SPDX-License-Identifier: MIT")
(printf "#!r6rs")
(printf "")

(lib-start (cdr (command-line)) instructions '((rnrs (6))))

(let lp ((format "%d") (x* instructions))
  (unless (null? x*)
    (let ((x (car x*)))
      (cond ((pair? x)
             (case (car x)
               ((fmt)
                (lp (cadr x) (cdr x*)))
               ((ifdef)
                (print "#ifdef " (cadr x))
                (lp format (cdr x*)))
               ((endif)
                (print "#endif")
                (lp format (cdr x*)))
               ((c-include)
                (printf "\\n;;; " (cadr x))
                (lp "%d" (cdr x*)))
               ((struct)
                (printf-struct (cadr x) (cddr x))
                (lp format (cdr x*)))
               ((sizeof)
                (printf-sizeof (cadr x))
                (lp format (cdr x*)))
               ((comment)
                (printf ";; " (cadr x))
                (lp format (cdr x*)))
               (else
                (lp format (cdr x*)))))
            (else
             (printf-const format x)
             (lp format (cdr x*)))))))

;; Suddenly something completely different.
(let ((table (make-eqv-hashtable)))
  (display "header-snarfer: Getting errno definitions from stdin...\n"
           (current-error-port))
  (printf)
  (let lp-next ((max-errno 0))
    (let ((line (get-line (current-input-port))))
      (cond
        ((eof-object? line)
         (printf "(define errno-list")
         (printf "  '#(")
         (do ((i 0 (+ i 1)))
             ((> i max-errno))
           (cond ((hashtable-ref table i #f) =>
                  (lambda (sym.str)
                    (printf "     "
                            "(" (car sym.str) " . "
                            "\\\"" (cdr sym.str) "\\\""
                            ")")))
                 (else
                  (printf "     #f"))))
         (printf "))"))
        (else
         (let* ((p (open-string-input-port line))
                (ch (get-char p)))
           (cond ((eqv? ch #\#)
                  (let ((x (get-datum p)))
                    (case x
                      ((define)
                       (let* ((sym (get-datum p))
                              (num (get-datum p))
                              (/* (get-datum p)))
                         (let lp ()
                           (let ((c (peek-char p)))
                             (when (and (char? c) (char-whitespace? c))
                               (get-char p)
                               (lp))))
                         (let ((comment (get-line p)))
                           (cond ((number? num)
                                  (let ((comment (substring comment 0 (- (string-length comment) 3))))
                                    (hashtable-set! table num (cons sym comment))
                                    (lp-next (max num max-errno))))
                                 (else
                                  (lp-next max-errno))))))
                      (else
                       (lp-next max-errno)))))
                 (else
                  (lp-next max-errno)))))))))

(lib-end)
(return 0)
(print "}")
