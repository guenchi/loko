;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright © 2019 Göran Weinholt
#!r6rs

;;; SRFI-170 (POSIX) for Loko on Linux

(library (loko posix)
  (export
    ;; Aliases for the real names
    (rename (EPERM errno/perm)
            (E2BIG errno/e2big)
            (EACCES errno/eacces)
            (EADDRINUSE errno/eaddrinuse)
            (EADDRNOTAVAIL errno/eaddrnotavail)
            (EAFNOSUPPORT errno/eafnosupport)
            (EAGAIN errno/eagain)
            (EALREADY errno/ealready)
            (EBADF errno/ebadf)
            (EBADMSG errno/ebadmsg)
            (EBUSY errno/ebusy)
            (ECANCELED errno/ecanceled)
            (ECHILD errno/echild)
            (ECONNABORTED errno/econnaborted)
            (ECONNREFUSED errno/econnrefused)
            (ECONNRESET errno/econnreset)
            (EDEADLK errno/edeadlk)
            (EDESTADDRREQ errno/edestaddrreq)
            (EDOM errno/edom)
            (EDQUOT errno/edquot)
            (EEXIST errno/eexist)
            (EFAULT errno/efault)
            (EFBIG errno/efbig)
            (EHOSTUNREACH errno/ehostunreach)
            (EIDRM errno/eidrm)
            (EILSEQ errno/eilseq)
            (EINPROGRESS errno/einprogress)
            (EINTR errno/eintr)
            (EINVAL errno/einval)
            (EIO errno/eio)
            (EISCONN errno/eisconn)
            (EISDIR errno/eisdir)
            (ELOOP errno/eloop)
            (EMFILE errno/emfile)
            (EMLINK errno/emlink)
            (EMSGSIZE errno/emsgsize)
            (EMULTIHOP errno/emultihop)
            (ENAMETOOLONG errno/enametoolong)
            (ENETDOWN errno/enetdown)
            (ENETRESET errno/enetreset)
            (ENETUNREACH errno/enetunreach)
            (ENFILE errno/enfile)
            (ENOBUFS errno/enobufs)
            (ENODATA errno/enodata)
            (ENODEV errno/enodev)
            (ENOENT errno/enoent)
            (ENOEXEC errno/enoexec)
            (ENOLCK errno/enolck)
            (ENOLINK errno/enolink)
            (ENOMEM errno/enomem)
            (ENOMSG errno/enomsg)
            (ENOPROTOOPT errno/enoprotoopt)
            (ENOSPC errno/enospc)
            (ENOSR errno/enosr)
            (ENOSTR errno/enostr)
            (ENOSYS errno/enosys)
            (ENOTCONN errno/enotconn)
            (ENOTDIR errno/enotdir)
            (ENOTEMPTY errno/enotempty)
            (ENOTRECOVERABLE errno/enotrecoverable)
            (ENOTSOCK errno/enotsock)
            (EOPNOTSUPP errno/enotsup)
            (ENOTTY errno/enotty)
            (ENXIO errno/enxio)
            (EOPNOTSUPP errno/eopnotsupp)
            (EOVERFLOW errno/eoverflow)
            (EOWNERDEAD errno/eownerdead)
            (EPERM errno/eperm)
            (EPIPE errno/epipe)
            (EPROTO errno/eproto)
            (EPROTONOSUPPORT errno/eprotonosupport)
            (EPROTOTYPE errno/eprototype)
            (ERANGE errno/erange)
            (EROFS errno/erofs)
            (ESPIPE errno/espipe)
            (ESRCH errno/esrch)
            (ESTALE errno/estale)
            (ETIME errno/etime)
            (ETIMEDOUT errno/etimedout)
            (ETXTBSY errno/etxtbsy)
            (EWOULDBLOCK errno/ewouldblock)
            (EXDEV errno/exdev))

    errno-error
    syscall-error?
    syscall-error:errno
    syscall-error:message
    syscall-error:procedure
    syscall-error:data

    fdes->textual-input-port
    fdes->binary-input-port
    fdes->textual-output-port
    fdes->binary-output-port
    (rename (port-file-descriptor port-fdes))
    dup->fdes
    close-fdes

    create-directory
    create-fifo
    create-hard-link
    create-symlink
    rename-file
    delete-directory
    set-file-mode
    set-file-owner
    set-file-group
    set-file-timespecs
    truncate-file

    file-info
    file-info?
    file-info:device file-info:inode file-info:mode file-info:nlinks file-info:uid
    file-info:gid file-info:rdev file-info:size file-info:blksize file-info:blocks
    file-info:atime file-info:mtime file-info:ctime
    file-info-directory? file-info-fifo? file-info-regular? file-info-socket?
    file-info-special? file-info-symlink?

    open-directory read-directory close-directory
    directory-entry-inode directory-entry-offset
    directory-entry-type directory-entry-filename

    temp-file-prefix
    create-temp-file
    call-with-temporary-filename
    real-path
    ;; spawn
    ;; spawn-path
    ;; file-spawn
    ;; fork
    ;; exec
    ;; exec-path
    process-object?
    process-object:pid
    wait
    wait-process-group
    status:exit-val
    status:stop-sig
    status:term-sig
    umask
    set-umask
    working-directory
    set-working-directory
    pid
    parent-pid
    process-group
    set-process-group
    set-priority
    priority
    nice
    user-login-name
    user-uid
    user-gid
    user-supplementary-gids
    set-uid
    set-gid
    user-effective-uid
    set-user-effective-uid
    user-effective-gid
    set-user-effective-gid
    user-info
    user-info?
    user-info:name user-info:uid user-info:gid user-info:home-dir user-info:shell
    group-info
    group-info?
    group-info:name group-info:gid group-info:members
    system-name
    uname
    uname?
    uname:os-name uname:node-name uname:release-name uname:version uname:machine
    current-timezone
    current-locale
    signal-process
    signal-process-group
    posix-time
    monotonic-time
    timespec-difference
    timespec=?
    exec-path-list
    tty?
    tty-file-name
    with-raw-mode
    without-echo
    without-interrupt-chars
    open-control-tty
    become-session-leader
    tty-process-group
    set-tty-process-group
    control-tty-file-name)
  (import
    (rnrs (6))
    (srfi :98 os-environment-variables)
    (struct pack)
    (only (loko) make-parameter port-file-descriptor port-file-descriptor-set!)
    (loko match)
    (except (loko system fibers) wait)
    (loko system unsafe)
    (loko runtime utils)
    (loko arch amd64 linux-syscalls)
    (loko arch amd64 linux-numbers))

;; Convert a filename to a NUL terminated bytevector.
(define (filename->c-string who fn)
  (unless (string? fn)
    (assertion-violation who "Expected a string" fn))
  (string-for-each
   (lambda (c)
     (when (eqv? c #\nul)
       ;; The filename is not representable on Linux
       (raise (condition
               (make-who-condition who)
               (make-i/o-filename-error fn)
               (make-message-condition "Unrepresentable filename")
               (make-irritants-condition (list fn))))))
   fn)
  (string->utf8 (string-append fn "\x0;")))

;; Convert a NUL terminated bytevector to a string.
(define (utf8z->string src offset)
  (do ((len 0 (fx+ len 1)))
      ((if (fx=? (fx+ offset len) (bytevector-length src))
           (assertion-violation 'utf8z->string "No NUL terminator found" src offset)
           (eqv? (bytevector-u8-ref src (fx+ offset len)) 0))
       (let ((tmp (make-bytevector len)))
         (bytevector-copy! src offset
                           tmp 0 (bytevector-length tmp))
         (utf8->string tmp)))))

(define & bytevector-address)

(define-syntax define-optional
  (lambda (x)
    (define (opt-clauses name args* opt*)
      (syntax-case opt* ()
        [() '()]
        [((lhs rhs) (lhs* rhs*) ...)
         (with-syntax ([(args ...) args*])
           #`([(args ...) (#,name args ... rhs)]
              #,@(opt-clauses name #'(args ... lhs) #'((lhs* rhs*) ...))))]))
    (syntax-case x ()
      [(_ (name args ... [(lhs* rhs*) ...])
          . body)
       #`(define name
           (case-lambda
             #,@(opt-clauses #'name #'(args ...) #'((lhs* rhs*) ...))
             [(args ... lhs* ...) . body]))])))

(define-syntax with-restart
  (syntax-rules ()
    ((_ (proc args ... handler))
     (let retry ()
       (proc args ... (lambda (errno)
                        (cond ((eqv? errno EINTR)
                               ;; The syscall was interrupted.
                               (yield-current-task)
                               (retry))
                              (else
                               (handler errno)))))))))

;;; 3.1 Errors

;; This is an extra condition included in the errors raised by this
;; library.
(define-condition-type &errno-error &error
   make-errno-error* syscall-error?
   (errno syscall-error:errno)
   (message syscall-error:message)
   (procedure syscall-error:procedure)
   (data syscall-error:data))

;; Make a condition that is compatible with the errno-error stuff.
(define (make-errno-error errno procedure . data)
  (let ((msg (cond ((and (fx<? errno (vector-length errno-list))
                         (vector-ref errno-list errno))
                    => cdr)
                   (else #f))))
    (make-errno-error* errno msg procedure data)))

;; Raise an error. Not to be used in this library.
(define (errno-error errno procedure . data)
  (raise (apply make-errno-error errno procedure data)))

;;; 3.2 I/O

(define (fdes->binary-input-port fd)
  (define who 'fdes->binary-input-port)
  (define position (sys_lseek fd 0 SEEK_CUR (lambda _ #f)))
  (define (handle-read-error errno)
    (raise
      (condition
       (make-syscall-error 'read errno))))
  (define (read! bv start count)
    (let ((status (let retry ()
                    (sys_read fd (fx+ (& bv) start) count
                              (lambda (errno)
                                (cond ((eqv? errno EAGAIN)
                                       (wait-for-readable fd)
                                       (retry))
                                      ((eqv? errno EINTR)
                                       (yield-current-task)
                                       (retry))
                                      (else
                                       (handle-read-error errno))))))))
      (when position
        (set! position (+ status)))
      status))
  (define (get-position)
    position)
  (define (set-position! off)
    (sys_lseek fd off SEEK_SET)
    (set! position off))
  (define (close)
    (sys_close fd (lambda (errno)
                    (unless (eqv? errno EINTR)
                      (raise
                        (make-who-condition 'close-port)
                        (make-message-condition "Error while closing the fd")
                        (make-syscall-error 'close errno))))))
  (let ((p (make-custom-binary-input-port
            (string-append "fd " (number->string fd))
            read! (and position get-position) (and position set-position!) close)))
    (port-file-descriptor-set! p fd)
    p))

(define (fdes->textual-input-port fd)
  (transcoded-port (fdes->binary-input-port fd) (native-transcoder)))

(define (fdes->binary-output-port fd)
  (define who 'fdes->binary-output-port)
  (define position (sys_lseek fd 0 SEEK_CUR (lambda _ #f)))
  (define (handle-write-error errno)
    (raise
      (condition
       (make-syscall-error 'write errno))))
  (define (write! bv start count)
    (let ((status
           (let retry ()
             (sys_write fd (fx+ (bytevector-address bv) start) count
                        (lambda (errno)
                          (cond ((eqv? errno EAGAIN)
                                 (wait-for-writable fd)
                                 (retry))
                                ((eqv? errno EINTR)
                                 (retry))
                                (else
                                 (handle-write-error errno))))))))
      (when position
        (set! position (+ position status)))
      status))
  (define (get-position)
    position)
  (define (set-position! off)
    (sys_lseek fd off SEEK_SET)
    (set! position off))
  (define (close)
    (sys_close fd (lambda (errno)
                    (unless (eqv? errno EINTR)
                      (raise
                        (make-who-condition 'close-port)
                        (make-message-condition "Error while closing the fd")
                        (make-syscall-error 'close errno))))))
  (let ((p (make-custom-binary-output-port
            (string-append "fd " (number->string fd))
            write! (and position get-position) (and position set-position!) close)))
    (port-file-descriptor-set! p fd)
    p))

(define (fdes->textual-output-port fd)
  (transcoded-port (fdes->binary-output-port fd) (native-transcoder)))

(define-optional (dup->fdes port [(fd #f)])
  (define (raise-error errno)
    (let ((syswho (if fd 'dup3 'fcntl)))
      (raise
        (condition
         (make-who-condition 'dup->fdes)
         (make-message-condition "Failed to duplicate the file descriptor")
         (make-irritants-condition (list port fd))
         (make-syscall-error syswho errno)
         (make-errno-error errno dup->fdes port fd)))))
  (cond
    ((or (not (port? port)) (not (port-file-descriptor port)))
     (assertion-violation 'dup->fdes "Expected a port with a file descriptor" port fd))
    ((not fd)
     (with-restart (sys_fcntl (port-file-descriptor port) F_DUPFD 0 raise-error)))
    (else
     (with-restart (sys_dup3 (port-file-descriptor port) fd 0 raise-error)))))

(define (close-fdes fd)
  (sys_close fd
             (lambda (errno)
               (unless (eqv? errno EINTR)
                 (raise
                   (condition
                    (make-who-condition 'close-fdes)
                    (make-message-condition "Failed to close file descriptor")
                    (make-irritants-condition (list fd))
                    (make-syscall-error 'close errno)
                    (make-errno-error errno close-fdes fd))))))
  (values))

;;; 3.3 File system

;; As per R6RS, these are also used when appropriate and feasible:

;; make-i/o-filename-error
;; make-i/o-file-protection-error
;; make-i/o-file-is-read-only-error
;; make-i/o-file-already-exists-error
;; make-i/o-file-does-not-exist-error

(define (filename-condition syswho errno fname)
  (cond ((eqv? errno EACCES)
         (make-i/o-file-protection-error fname))
        ((eqv? errno EROFS)
         ;; XXX: There are actually more errors that should be covered
         ;; by this, but it requires more syscalls to find the actual
         ;; cause of the error.
         (make-i/o-file-is-read-only-error fname))
        ((eqv? errno EEXIST)
         (make-i/o-file-already-exists-error fname))
        ((eqv? errno ENOENT)
         (make-i/o-file-does-not-exist-error fname))
        (else
         (make-i/o-filename-error fname))))

;; Try to unlink the filename
(define (try-unlink fname-bv raise-error)
  (with-restart
   (sys_unlinkat AT_FDCWD (& fname-bv) 0
                 (lambda (errno)
                   (cond
                     ((eqv? errno EISDIR)
                      (with-restart
                       (sys_unlinkat AT_FDCWD (& fname-bv) AT_REMOVEDIR
                                     (lambda (errno)
                                       (unless (eqv? errno ENOENT)
                                         (raise-error 'unlinkat errno))))))
                     ((eqv? errno ENOENT) #f)
                     (else
                      (raise-error 'unlinkat errno))))))
  (values))

(define-optional (create-directory fname [(permission-bits #o775) (override? #f)])
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'create-directory)
       (make-message-condition
        (case syswho
          ((unlinkat) "Failed to remove the old file system entry")
          (else "Failed to create the directory")))
       (make-irritants-condition (list fname permission-bits override?))
       (filename-condition syswho errno fname)
       (make-syscall-error syswho errno)
       (make-errno-error errno create-directory fname permission-bits override?))))
  (unless (fixnum? permission-bits)
    (assertion-violation 'create-directory "Expected an exact integer"
                         fname permission-bits override?))
  (let ((fname-bv (filename->c-string 'create-directory fname)))
    (with-restart
     (sys_mkdirat AT_FDCWD (& fname-bv) permission-bits
                  (lambda (errno)
                    (cond ((and (eqv? errno EEXIST) override?)
                           (try-unlink fname-bv raise-error)
                           (with-restart
                            (sys_mkdirat AT_FDCWD (& fname-bv) permission-bits
                                         (lambda (errno) (raise-error 'mkdirat errno)))))
                          (else (raise-error 'mkdirat errno))))))
    (values)))

(define-optional (create-fifo fname [(permission-bits #o664) (override? #f)])
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'create-fifo)
       (make-message-condition
        (case syswho
          ((unlinkat) "Failed to remove the old file system entry")
          (else "Failed to create the FIFO")))
       (make-irritants-condition (list fname permission-bits override?))
       (filename-condition syswho errno fname)
       (make-syscall-error syswho errno)
       (make-errno-error errno create-fifo fname permission-bits override?))))
  (unless (fixnum? permission-bits)
    (assertion-violation 'create-fifo "Expected an exact integer"
                         fname permission-bits override?))
  (let ((fname-bv (filename->c-string 'create-fifo fname))
        (mode (fxior (fxand permission-bits (fxnot S_IFMT)) S_IFSOCK)))
    (with-restart
     (sys_mknodat AT_FDCWD (& fname-bv) mode 0
                  (lambda (errno)
                    (cond ((and (eqv? errno EEXIST) override?)
                           (try-unlink fname-bv raise-error)
                           (with-restart
                            (sys_mknodat AT_FDCWD (& fname-bv) mode 0
                                         (lambda (errno) (raise-error 'mknodat errno)))))
                          (else (raise-error 'mknodat errno))))))
    (values)))

(define-optional (create-hard-link oldname newname [(override? #f)])
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'create-hard-link)
       (make-message-condition
        (case syswho
          ((unlinkat) "Failed to remove the old file system entry")
          (else "Failed to create the link")))
       (make-irritants-condition (list oldname newname override?))
       ;; XXX: This error reporting could be improved
       (filename-condition syswho errno oldname)
       (filename-condition syswho errno newname)
       (make-syscall-error syswho errno)
       (make-errno-error errno create-hard-link oldname newname override?))))
  (let ((oldname-bv (filename->c-string 'create-hard-link oldname))
        (newname-bv (filename->c-string 'create-hard-link newname)))
    (with-restart
     (sys_linkat AT_FDCWD (& oldname-bv) AT_FDCWD (& newname-bv) 0
                 (lambda (errno)
                   (cond ((and (eqv? errno EEXIST) override?)
                          (try-unlink newname-bv raise-error)
                          (with-restart
                           (sys_linkat AT_FDCWD (& oldname-bv) AT_FDCWD (& newname-bv) 0
                                       (lambda (errno) (raise-error 'linkat errno)))))
                         (else (raise-error 'linkat errno))))))
    (values)))

(define-optional (create-symlink oldname newname [(override? #f)])
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'create-symlink)
       (make-message-condition
        (case syswho
          ((unlinkat) "Failed to remove the old file system entry")
          (else "Failed to create the symlink")))
       (make-irritants-condition (list oldname newname override?))
       (filename-condition syswho errno newname)
       (make-syscall-error syswho errno)
       (make-errno-error errno create-symlink oldname newname override?))))
  (let ((oldname-bv (filename->c-string 'create-symlink oldname))
        (newname-bv (filename->c-string 'create-symlink newname)))
    (with-restart
     (sys_symlinkat (& oldname-bv) AT_FDCWD (& newname-bv)
                    (lambda (errno)
                      (cond ((and (eqv? errno EEXIST) override?)
                             (try-unlink newname-bv raise-error)
                             (with-restart
                              (sys_symlinkat (& oldname-bv) AT_FDCWD (& newname-bv)
                                             (lambda (errno) (raise-error 'symlinkat errno)))))
                            (else (raise-error 'symlinkat errno))))))
    (values)))

(define-optional (rename-file oldname newname [(override? #f)])
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'rename-file)
       (make-message-condition
        (case syswho
          ((unlinkat) "Failed to remove the old file system entry")
          (else "Failed to rename the file")))
       (make-irritants-condition (list oldname newname override?))
       (filename-condition syswho errno newname)
       (make-syscall-error syswho errno)
       (make-errno-error errno rename-file oldname newname override?))))
  (let ((oldname-bv (filename->c-string 'rename-file oldname))
        (newname-bv (filename->c-string 'rename-file newname)))
    (with-restart
     (sys_renameat2 AT_FDCWD (& oldname-bv) AT_FDCWD (& newname-bv) 0
                    (lambda (errno)
                      (cond ((and (eqv? errno EEXIST) override?)
                             (with-restart
                              (sys_unlinkat AT_FDCWD (& newname-bv) AT_REMOVEDIR
                                            (lambda (errno)
                                              (unless (eqv? errno ENOENT)
                                                (raise-error 'unlinkat errno)))))
                             (with-restart
                              (sys_renameat2 AT_FDCWD (& oldname-bv) AT_FDCWD (& newname-bv) 0
                                             (lambda (errno) (raise-error 'renameat2 errno)))))
                            (else (raise-error 'renameat2 errno))))))
    (values)))

(define (delete-directory fname)
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'delete-directory)
       (make-message-condition "Failed to delete the directory")
       (make-irritants-condition (list fname))
       (filename-condition syswho errno fname)
       (make-syscall-error syswho errno)
       (make-errno-error errno delete-directory fname))))
  (let ((newname-bv (filename->c-string 'delete-directory fname)))
    (with-restart
     (sys_unlinkat AT_FDCWD (& newname-bv) AT_REMOVEDIR
                   (lambda (errno)
                     (raise-error 'unlinkat errno))))
    (values)))

(define (set-file-mode fname/port permission-bits)
  (define (raise-error errno)
    (let ((syswho (if (string? fname/port) 'fchmodat 'fchmod)))
      (raise
        (condition
         (make-who-condition 'set-file-mode)
         (make-message-condition "Could not set file mode")
         (make-irritants-condition (list fname/port))
         (if (string? fname/port) (make-i/o-filename-error fname/port) (condition))
         (make-syscall-error syswho errno)
         (make-errno-error errno set-file-mode fname/port)))))
  (if (string? fname/port)
      (let ((fname-bv (filename->c-string 'set-file-mode fname/port)))
        (with-restart
         (sys_fchmodat AT_FDCWD (& fname-bv) permission-bits raise-error)))
      (cond ((port-file-descriptor fname/port) =>
             (lambda (fd)
               (with-restart
                (sys_fchmod fd permission-bits raise-error))))
            (else
             (assertion-violation 'set-file-mode
                                  "Expected a filename or a port with a file descriptor"
                                  fname/port))))
  (values))

(define (set-file-owner fname/port uid)
  (define (raise-error errno)
    (let ((syswho (if (string? fname/port) 'fchownat 'fchown)))
      (raise
        (condition
         (make-who-condition 'set-file-mode)
         (make-message-condition "Could not set file owner")
         (make-irritants-condition (list fname/port uid))
         (if (string? fname/port) (make-i/o-filename-error fname/port) (condition))
         (make-syscall-error syswho errno)
         (make-errno-error errno set-file-mode fname/port uid)))))
  (if (string? fname/port)
      (let ((fname-bv (filename->c-string 'set-file-mode fname/port)))
        (with-restart
         (sys_fchownat AT_FDCWD (& fname-bv) uid -1 0 raise-error)))
      (cond ((port-file-descriptor fname/port) =>
             (lambda (fd)
               (with-restart
                (sys_fchown fd uid -1 raise-error))))
            (else
             (assertion-violation 'set-file-mode
                                  "Expected a filename or a port with a file descriptor"
                                  fname/port))))
  (values))

(define (set-file-group fname/port gid)
  (define (raise-error errno)
    (let ((syswho (if (string? fname/port) 'fchownat 'fchown)))
      (raise
        (condition
         (make-who-condition 'set-file-mode)
         (make-message-condition "Could not set file group")
         (make-irritants-condition (list fname/port gid))
         (if (string? fname/port) (make-i/o-filename-error fname/port) (condition))
         (make-syscall-error syswho errno)
         (make-errno-error errno set-file-mode fname/port gid)))))
  (if (string? fname/port)
      (let ((fname-bv (filename->c-string 'set-file-mode fname/port)))
        (with-restart
         (sys_fchownat AT_FDCWD (& fname-bv) -1 gid 0 raise-error)))
      (cond ((port-file-descriptor fname/port) =>
             (lambda (fd)
               (with-restart
                (sys_fchown fd -1 gid raise-error))))
            (else
             (assertion-violation 'set-file-mode
                                  "Expected a filename or a port with a file descriptor"
                                  fname/port))))
  (values))

(define-optional (set-file-timespecs fname/port [(access-timespec #f) (mod-timespec #f)])
  (define (raise-error errno)
    (let ((syswho 'utimensat))
      (raise
        (condition
         (make-who-condition 'set-file-timespec)
         (make-message-condition "Could not set file times")
         (make-irritants-condition (list fname/port access-timespec mod-timespec))
         (if (string? fname/port) (make-i/o-filename-error fname/port) (condition))
         (make-syscall-error syswho errno)
         (make-errno-error errno set-file-timespecs fname/port access-timespec mod-timespec)))))
  ;; Extension: either of the timespecs can be omitted
  (let ((access-timespec
         (or access-timespec (if mod-timespec (cons 0 UTIME_OMIT) (cons 0 UTIME_NOW))))
        (mod-timespec
         (or mod-timespec (if access-timespec (cons 0 UTIME_OMIT) (cons 0 UTIME_NOW)))))
    (let ((utimes (pack "2Q 2Q" (car access-timespec) (cdr access-timespec)
                        (car mod-timespec) (cdr mod-timespec))))
      (if (string? fname/port)
          (let ((fname-bv (filename->c-string 'set-file-timespecs fname/port)))
            (with-restart
             (sys_utimensat AT_FDCWD (& fname-bv) (& utimes) 0 raise-error)))
          (cond ((port-file-descriptor fname/port) =>
                 (lambda (fd)
                   (with-restart
                    (sys_utimensat fd 0 (& utimes) 0 raise-error))))
                (else
                 (assertion-violation 'set-file-timespecs
                                      "Expected a filename or a port with a file descriptor"
                                      fname/port access-timespec mod-timespec))))))
  (values))

(define (truncate-file fname/port len)
  (define (raise-error errno)
    (let ((syswho (if (string? fname/port) 'truncate 'ftruncate)))
      (raise
        (condition
         (make-who-condition 'truncate-file)
         (make-message-condition "Could not truncate file")
         (make-irritants-condition (list fname/port len))
         (if (string? fname/port)
             (make-i/o-filename-error fname/port)
             (condition))
         (make-syscall-error syswho errno)
         (make-errno-error errno truncate-file fname/port len)))))
  (unless (fixnum? len)
    (assertion-violation 'truncate-file "Expected a fixnum" fname/port len))
  (if (string? fname/port)
      (let ((fname-bv (filename->c-string 'truncate-file fname/port)))
        (with-restart
         (sys_truncate (& fname-bv) len raise-error)))
      (cond ((port-file-descriptor fname/port) =>
             (lambda (fd)
               (with-restart
                (sys_ftruncate fd len raise-error))))
            (else
             (assertion-violation 'truncate-file
                                  "Expected a filename or a port with a file descriptor"
                                  fname/port len))))
  (values))

(define-record-type (&file-info make-file-info file-info?)
  (fields (immutable device file-info:device)
          (immutable inode file-info:inode)
          (immutable mode file-info:mode)
          (immutable nlinks file-info:nlinks)
          (immutable uid file-info:uid)
          (immutable gid file-info:gid)
          (immutable rdev file-info:rdev)
          (immutable size file-info:size)
          (immutable blksize file-info:blksize)
          (immutable blocks file-info:blocks)
          (immutable atime file-info:atime)
          (immutable mtime file-info:mtime)
          (immutable ctime file-info:ctime)))

;; One of stat, lstat or fstat
(define-optional (file-info filename/port [(chase? #t)])
  (define (bytevector-timespec-native-ref stat offset)
    (cons (bytevector-s64-native-ref stat offset)
          (bytevector-u64-native-ref stat (fx+ offset 8))))
  (define (raise-error errno)
    (let ((syswho (if (string? filename/port) (if chase? 'stat 'lstat) 'fstat)))
      (raise (condition
              (make-who-condition 'file-info)
              (make-message-condition "Could not stat file")
              (make-irritants-condition (list filename/port))
              (if (string? filename/port)
                  (make-i/o-filename-error filename/port)
                  (condition))
              (make-syscall-error syswho errno)
              (make-errno-error errno file-info filename/port chase?)))))
  (let ((statbuf (make-bytevector sizeof-stat)))
    ;; Call the right stat syscall
    (if (string? filename/port)
        (let ((fn (filename->c-string 'file-info filename/port)))
          (if chase?
              (with-restart (sys_stat (& fn) (& statbuf) raise-error))
              (with-restart (sys_lstat (& fn) (& statbuf) raise-error))))
        (cond ((port-file-descriptor filename/port) =>
               (lambda (fd) (with-restart (sys_fstat fd (& statbuf) raise-error))))
              (else
               (assertion-violation 'file-info
                                    "Expected a filename or a port with a file descriptor"
                                    filename/port))))
    ;; Decode the buffer
    (let ((device (bytevector-u64-native-ref statbuf offsetof-stat-st_dev))
          (inode (bytevector-u64-native-ref statbuf offsetof-stat-st_ino))
          (mode (bytevector-u32-native-ref statbuf offsetof-stat-st_mode))
          (nlinks (bytevector-u32-native-ref statbuf offsetof-stat-st_nlink))
          (uid (bytevector-u32-native-ref statbuf offsetof-stat-st_uid))
          (gid (bytevector-u32-native-ref statbuf offsetof-stat-st_gid))
          (rdev (bytevector-u64-native-ref statbuf offsetof-stat-st_rdev))
          (size (bytevector-s64-native-ref statbuf offsetof-stat-st_size))
          (blksize (bytevector-s32-native-ref statbuf offsetof-stat-st_blksize))
          (blocks (bytevector-s64-native-ref statbuf offsetof-stat-st_blocks))
          (atime (bytevector-timespec-native-ref statbuf offsetof-stat-st_atime))
          (mtime (bytevector-timespec-native-ref statbuf offsetof-stat-st_mtime))
          (ctime (bytevector-timespec-native-ref statbuf offsetof-stat-st_ctime)))
      (make-file-info device inode mode nlinks uid gid rdev
                      size blksize blocks atime mtime ctime))))

(define (file-info-directory? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFDIR))

(define (file-info-fifo? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFIFO))

(define (file-info-regular? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFREG))

(define (file-info-socket? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFSOCK))

(define (file-info-special? file-info)
  (let ((type (fxand (file-info:mode file-info) S_IFMT)))
    (or (eqv? type S_IFCHR) (eqv? type S_IFBLK))))

(define (file-info-symlink? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFLNK))

(define (open-directory dirname)
  (let* ((fn (filename->c-string 'open-directory dirname))
         (fd (with-restart
              (sys_open (& fn)
                        (bitwise-ior O_CLOEXEC O_DIRECTORY O_NONBLOCK)
                        0
                        (lambda (errno)
                          (raise
                            (condition
                             (make-who-condition 'open-directory)
                             (make-message-condition "Could not open directory")
                             (make-irritants-condition (list dirname))
                             (if (eqv? errno ENOENT)
                                 (make-i/o-file-does-not-exist-error dirname)
                                 (make-i/o-filename-error dirname))
                             (make-syscall-error 'open errno)
                             (make-errno-error errno open-directory dirname))))))))
    (define (read! bv start count)
      (let retry ()
        (with-restart
         (sys_getdents64 fd (fx+ (& bv) start) count
                         (lambda (errno)
                           (cond ((eqv? errno EAGAIN)
                                  (wait-for-readable fd)
                                  (retry))
                                 (else
                                  (raise
                                    (condition
                                     (make-who-condition 'read-directory)
                                     (make-i/o-filename-error dirname)
                                     (make-syscall-error 'getdents64 errno)
                                     (make-errno-error errno read-directory dirname))))))))))
    (define get-position #f)
    (define set-position! #f)
    (define (close)
      (sys_close fd (lambda (errno)
                      (raise
                        (make-who-condition 'close-directory)
                        (make-message-condition "Error while closing the directory")
                        (make-i/o-filename-error dirname)
                        (make-syscall-error 'close errno)
                        (make-errno-error errno close-directory dirname)))))
    (let ((p (make-custom-binary-input-port
              dirname read! get-position set-position! close)))
      (port-file-descriptor-set! p fd)
      p)))

(define-record-type directory-entry
  (fields inode offset type filename))

(define read-directory
  (case-lambda
    ((dir)
     (read-directory dir #f))
    ;; The full-info? argument is an extension
    ((dir full-info?)
     ;; Read and parse a "struct linux_dirent64" (which is apparently
     ;; not part of the UAPI headers).
     (if (port-eof? dir)
         #f
         (let-values ([(d_ino d_off d_reclen d_type) (get-unpack dir "QQSC")])
           (let ((fn (utf8z->string (get-bytevector-n dir (fx- d_reclen (format-size "QQSC")))
                                    0)))
             (if full-info?
                 (make-directory-entry d_ino d_off d_type fn)
                 fn)))))))

(define (close-directory dir)
  (close-port dir))

(define-optional (directory-files [(dirname ".") (dotfiles? #f)])
  (call-with-port (open-directory dirname)
    (lambda (dir)
      (let lp ((ret '()))
        (cond ((read-directory dir) =>
               (lambda (fn)
                 (if (or (and (not dotfiles?)
                              (char=? (string-ref fn 0) #\.))
                         (member fn '("." "..")))
                     (lp ret)
                     (lp (cons fn ret)))))
              (else ret))))))

(define (read-symlink filename)
  (define (raise-error syswho errno)
    (raise
      (condition
       (make-who-condition 'read-symlink)
       (make-message-condition "Failed to read symlink")
       (make-irritants-condition (list filename))
       (filename-condition syswho errno filename)
       (make-syscall-error syswho errno)
       (make-errno-error errno read-symlink filename))))
  (let ((fn (filename->c-string 'read-symlink filename)))
    (let lp ((bufsize 1))
      (let ((buf (make-bytevector bufsize)))
        (let ((len (sys_readlinkat AT_FDCWD (& fn) (& buf) bufsize raise-error)))
          (cond ((fx=? len bufsize)
                 ;; The filename was truncated
                 (lp (fx* bufsize 2)))
                (else
                 (utf8z->string buf 0))))))))

;; FIXME: Must be a SRFI-39 parameter
(define temp-file-prefix
  (make-parameter
   (let ((tmpdir (or (get-environment-variable "TMPDIR") "/tmp")))
     (string-append tmpdir "/" (number->string (sys_getpid))))))

(define-optional (create-temp-file [(prefix #f)])
  (error 'create-temp-file "TODO" prefix))

(define-optional (call-with-temporary-filename maker [(prefix #f)])
  (error 'call-with-temporary-filename "TODO" maker prefix))

(define (real-path path^)
  (define error-msg "Failed to get the canonicalized absolute pathname")
  (define (path-is-symlink? filename enoent-ok)
    (let* ((statbuf (make-bytevector sizeof-stat))
           (fn (filename->c-string 'real-path filename)))
      (and
        (with-restart
         (sys_lstat (& fn) (& statbuf)
                    (lambda (errno)
                      (cond ((and (eqv? errno ENOENT) enoent-ok)
                             #f)
                            (else
                             (raise (condition
                                     (make-who-condition 'real-path)
                                     (make-message-condition error-msg)
                                     (make-irritants-condition (list path^))
                                     (make-i/o-file-protection-error filename)
                                     (make-syscall-error 'lstat errno)
                                     (make-errno-error errno real-path path^))))))))
        (eqv? (fxand (bytevector-u32-native-ref statbuf offsetof-stat-st_mode)
                     S_IFMT)
              S_IFLNK))))
  (define (join-components components)
    (if (null? components)
        "/"
        (call-with-string-output-port
          (lambda (p)
            (for-each
             (lambda (component)
               (put-char p #\/)
               (put-string p component))
             components)))))
  (let loop ((path path^) (seen #f) (limit 500))
    (when (eqv? limit 0)
      ;; This is mostly in case the hashtable approach isn't foolproof
      (raise
        (condition
         (make-who-condition 'real-path)
         (make-message-condition error-msg)
         (make-irritants-condition (list path^))
         (make-i/o-filename-error path^)
         (make-errno-error ELOOP real-path path^))))
    (let ((path (if (or (eqv? 0 (string-length path))
                        (not (eqv? #\/ (string-ref path 0))))
                    (string-append (working-directory) "/" path)
                    path)))
      (let lp ((x '())
               (components (string-split path #\/)))
        (match components
          [("" . c*) (lp x c*)]
          [("." . c*) (lp x c*)]
          [(".." . c*)
           (if (null? x)
               (lp x c*)
               (lp (cdr x) c*))]
          [(c . c*)
           (let ((tmppath (join-components (reverse (cons c x)))))
             (cond ((path-is-symlink? tmppath (null? c*))
                    (let ((target (read-symlink tmppath))
                          (seen (or seen (make-hashtable string-hash string=?))))
                      (hashtable-update! seen tmppath
                                         (lambda (old)
                                           (if old
                                               (raise
                                                 (condition
                                                  (make-who-condition 'real-path)
                                                  (make-message-condition error-msg)
                                                  (make-irritants-condition (list path tmppath))
                                                  (make-i/o-filename-error path^)
                                                  (make-errno-error ELOOP real-path path^)))
                                               #t))
                                         #f)
                      (if (eqv? #\/ (string-ref target 0))
                          (loop (string-append target (join-components c*)) seen (fx- limit 1))
                          (loop (string-append (join-components (reverse x)) "/" target "/"
                                               (join-components c*))
                                seen (fx- limit 1)))))
                   (else
                    (lp (cons c x) c*))))]
          [()
           (join-components (reverse x))])))))

;;; 3.4 Processes

;; (spawn mode [config] prog arg[1] ...arg[n])     →     object
;; (spawn-path mode [config] prog arg[1] ...arg[n])     →     object
;; (file-spawn file)     →     object
;; (fork [thunk or #f])     →     process-object or #f
;; (exec [config] prog arg[1] ...arg[n])     →     never returns
;; (exec-path [config] prog arg[1] ...arg[n])     →     never returns

;;; 3.4.1 Process objects

(define-record-type process-object
  (fields (immutable pid process-object:pid)))

;;; 3.4.2 Process waiting

(define-optional (wait proc/pid [(flags #f)])
  (error 'wait "TODO" proc/pid flags))

(define-optional (wait-process-group pgrp [(flags #f)])
  (error 'wait "TODO" pgrp flags))

;;; 3.4.3 Analysing process status codes

(define (status:exit-val status)
  (error 'status:exit-val "TODO"))

(define (status:stop-sig status)
  (error 'status:stop-sig "TODO"))

(define (status:term-sig status)
  (error 'status:term-sig "TODO"))

;;; 3.5 Process state

(define (umask)
  (let ((mask (sys_umask #o22)))
    (sys_umask mask)
    mask))

(define (set-umask mask)
  (sys_umask mask))

(define (working-directory)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'working-directory)
       (make-message-condition "Could not get the working directory")
       (make-irritants-condition '())
       (make-syscall-error 'getcwd errno)
       (make-errno-error errno working-directory))))
  (let lp ((size 128))
    (let* ((buf (make-bytevector size))
           (status
            (with-restart
             (sys_getcwd (& buf) (bytevector-length buf)
                         (lambda (errno)
                           (cond ((eqv? errno ERANGE)
                                  'resize)
                                 (else
                                  (raise-error errno))))))))
      (case status
        ((resize)
         (lp (fx* size 2)))
        (else
         (utf8z->string buf 0))))))

(define-optional (set-working-directory [(fname #f)])
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-working-directory)
       (make-message-condition "Could not set the working directory")
       (make-irritants-condition (list fname))
       (make-syscall-error 'chdir errno)
       (make-errno-error errno set-working-directory fname))))
  (let ((fname (or fname (get-environment-variable "HOME"))))
    (unless fname
      (assertion-violation 'set-working-directory
                           "Failed to get the home directory"))
    (let ((fname-bv (filename->c-string 'set-working-directory fname)))
      (with-restart
       (sys_chdir (& fname-bv) raise-error))
      (values))))

(define (pid)
  (sys_getpid))

(define (parent-pid)
  (sys_getppid))

(define (process-group)
  (sys_getpgid 0))

(define (->pid x)
  (if (fixnum? x) x (process-object:pid x)))

(define set-process-group
  (case-lambda
    [(pgrp)
     (set-process-group 0 pgrp)]
    [(proc/pid pgrp)
     (define (raise-error errno)
       (raise
         (condition
          (make-who-condition 'set-process-group)
          (make-message-condition "Could not set the process group")
          (make-irritants-condition (list proc/pid pgrp))
          (make-syscall-error 'setpgid errno)
          (make-errno-error errno set-process-group proc/pid pgrp))))
     (let ((pid (->pid proc/pid)))
       (with-restart (sys_setpgid pid pgrp raise-error)))]))

(define (set-priority which who niceness)
  'priority/process
  'priority/process-group
  'priority/user
  (error 'set-priority "TODO" which who niceness))

(define (priority which who)
  'priority/process
  'priority/process-group
  'priority/user
  (error 'priority "TODO" which who))

(define-optional (nice [(proc/pid #f) (delta #f)])
  ;; getpriority(PRIO_PROCESS, 0)
  ;; setpriority(PRIO_PROCESS, 0, 5)
  (error 'nice "TODO" proc/pid delta))

(define (user-login-name)
  (or (get-environment-variable "LOGNAME")
      (get-environment-variable "USER")))

(define (user-uid)
  (sys_getuid))

(define (user-gid)
  (sys_getgid))

(define (user-supplementary-gids)
  (error 'user-supplementary-gids "TODO"))

(define (set-uid uid)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-uid)
       (make-message-condition "Failed to set the uid")
       (make-irritants-condition (list uid))
       (make-syscall-error 'setuid errno)
       (make-errno-error errno set-uid uid))))
  (sys_setreuid uid -1 raise-error)
  (values))

(define (set-gid gid)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-gid)
       (make-message-condition "Failed to set the gid")
       (make-irritants-condition (list gid))
       (make-syscall-error 'setregid errno)
       (make-errno-error errno set-gid gid))))
  (sys_setregid gid -1 raise-error)
  (values))

(define (user-effective-uid)
  (sys_geteuid))

(define (set-user-effective-uid euid)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-effective-uid)
       (make-message-condition "Failed to set the effective uid")
       (make-irritants-condition (list euid))
       (make-syscall-error 'seteuid errno)
       (make-errno-error errno set-user-effective-uid euid))))
  (sys_setreuid -1 euid raise-error)
  (values))

(define (user-effective-gid)
  (sys_getegid))

(define (set-user-effective-gid egid)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-effective-gid)
       (make-message-condition "Failed to set the effective gid")
       (make-irritants-condition (list egid))
       (make-syscall-error 'setegid errno)
       (make-errno-error errno set-user-effective-gid egid))))
  (sys_setregid -1 egid raise-error)
  (values))

;;; 3.6 User and group database access

;; This chapter is a pain to implement fully without access to libc on
;; a system where everything else uses libc.

(define (user-info uid/name)
  (error 'user-info "TODO" uid/name))

(define-record-type (&user-info make-user-info user-info?)
  (fields (immutable name user-info:name)
          (immutable uid user-info:uid)
          (immutable gid user-info:gid)
          (immutable home-dir user-info:home-dir)
          (immutable shell user-info:shell)))

(define (group-info gid/name)
  (error 'group-info "TODO" gid/name))

(define-record-type (&group-info make-group-info group-info?)
  (fields (immutable name group-info:name)
          (immutable gid group-info:gid)
          (immutable members group-info:members)))

;;; 3.8 System parameters

(define-record-type (&uname make-uname uname?)
  (fields (immutable os-name uname:os-name)
          (immutable node-name uname:node-name)
          (immutable release-name uname:release-name)
          (immutable version uname:version)
          (immutable machine uname:machine)
          (immutable domain-name uname:domain-name)))

(define (system-name)
  (uname:node-name (uname)))

(define (uname)
  (let ((buf (make-bytevector sizeof-new_utsname #xff)))
    (sys_uname (& buf))
    (make-uname (utf8z->string buf offsetof-new_utsname-sysname)
                (utf8z->string buf offsetof-new_utsname-nodename)
                (utf8z->string buf offsetof-new_utsname-release)
                (utf8z->string buf offsetof-new_utsname-version)
                (utf8z->string buf offsetof-new_utsname-machine)
                (utf8z->string buf offsetof-new_utsname-domainname))))

(define (current-timezone)
  #f)

(define (current-locale)
  #f)

;;; 3.9 Signal system

(define (signal-process proc/pid sig)
  (error 'signal-process "TODO" proc/pid sig))

(define (signal-process-group prgrp/pid sig)
  (error 'signal-process-group "TODO" prgrp/pid sig))

;;; 3.10 Time

(define (posix-time)
  (error 'posix-time "TODO"))

(define (monotonic-time)
  (error 'monotonic-time "TODO"))

(define (timespec-difference timespec1 timespec2)
  (error 'timespec-difference "TODO" timespec1 timespec2))

(define (timespec=? timespec1 timespec2)
  (equal? timespec1 timespec2))

;;; 3.11 Environment variables

(define exec-path-list
  (string-split (get-environment-variable "PATH") #\:))

;;; 3.12 Terminal device control

(define (tty? port)
  (cond ((port-file-descriptor port) =>
         (lambda (fd)
           (let ((buf (make-bytevector sizeof-termios)))
             (with-restart
              (sys_ioctl fd TCGETS (& buf) (lambda _ #f)))
             #t)))
        (else #f)))

(define (tty-file-name port)
  (error 'tty-file-name "TODO"))

;; TODO: Implement
(define (with-raw-mode port min time thunk)
  (dynamic-wind
    (lambda () #f)
    thunk
    (lambda () #f)))

;; TODO: Implement
(define (without-echo port thunk)
  (dynamic-wind
    (lambda () #f)
    thunk
    (lambda () #f)))

;; TODO: Implement
(define (without-interrupt-chars port thunk)
  (dynamic-wind
    (lambda () #f)
    thunk
    (lambda () #f)))

(define (open-control-tty tty-name)
  (error 'open-control-tty "TODO" tty-name))

(define (become-session-leader)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'become-session-leader)
       (make-message-condition "Could not become session leader")
       (make-irritants-condition '())
       (make-syscall-error 'setsid errno)
       (make-errno-error errno become-session-leader))))
  (sys_setsid raise-error))

(define (tty-process-group port/fname)
  (error 'tty-process-group "TODO" port/fname))

(define (set-tty-process-group port/fname process-group)
  (error 'set-tty-process-group "TODO" port/fname process-group))

(define (control-tty-file-name)
  (error 'control-tty-file-name "TODO")))
