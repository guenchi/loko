;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright © 2019 Göran Weinholt
#!r6rs

;;; SRFI-170 (POSIX) support library for Linux

#|

Known non-conformance and extensions:

 * temp-file-prefix is a Chez-style parameter, but should be a
   SRFI-39/R7RS parameter

 * read-directory has an optional flag that causes it to return
   directory-entry objects instead of just filenames

 * make-directory-files-generator is not implemented here, but can
   be implemented in a (srfi :170 posix) library without incurring
   extra overhead from syscalls

 * uname is kept from SRFI 170 draft #6 as an extension

 * The user-info and group-info procedures do not try to access the
   databases via NSS and therefore will not see some users and groups.
   If this is a problem for you then please open an issue.

 * The with-/without- procedures are not based in dynamic-wind because
   then they would not compose well with fibers.

This library is meant to be used in (srfi :170 posix), in a file like
srfi/%3a170/posix.loko.sls, perhaps in the chez-srfi package. With
some simple care in these things you can make your code portable
between implementations.

Both posix-time and monotonic-time assume that (srfi :174) will
use (loko system time).

|#

(library (loko posix)
  (export
    fdes->textual-input-port
    fdes->binary-input-port
    fdes->textual-output-port
    fdes->binary-output-port
    (rename (port-file-descriptor port-fdes))
    close-fdes

    create-directory
    create-fifo
    create-hard-link
    create-symlink
    read-symlink
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
    file-info-directory? file-info-fifo? file-info-regular? file-info-symlink?

    directory-files
    ;; make-directory-files-generator
    open-directory read-directory close-directory
    directory-entry-inode directory-entry-offset
    directory-entry-type directory-entry-filename

    real-path
    temp-file-prefix
    create-temp-file
    call-with-temporary-filename

    umask
    set-umask
    working-directory
    set-working-directory
    pid
    parent-pid
    process-group
    nice
    user-uid
    user-gid
    user-effective-uid
    user-effective-gid
    user-supplementary-gids

    user-info
    user-info?
    user-info:name user-info:uid user-info:gid user-info:home-dir user-info:shell
    user-info:full-name user-info:parsed-full-name
    group-info
    group-info?
    group-info:name group-info:gid

    uname
    uname?
    uname:os-name uname:node-name uname:release-name uname:version uname:machine

    posix-time monotonic-time

    terminal? terminal-file-name
    with-raw-mode with-rare-mode without-echo)
  (import
    (rnrs (6))
    (rnrs mutable-strings (6))
    (srfi :98 os-environment-variables)
    (struct pack)
    (only (loko) make-parameter port-file-descriptor port-file-descriptor-set!)
    (loko match)
    (loko system fibers)
    (loko system time)
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

;;; 3.2 I/O

(define (fdes->binary-input-port fd)
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
    (sys_close fd))
  (let ((p (make-custom-binary-input-port
            (string-append "fd " (number->string fd))
            read! (and position get-position) (and position set-position!) close)))
    (port-file-descriptor-set! p fd)
    p))

(define (fdes->textual-input-port fd)
  (transcoded-port (fdes->binary-input-port fd) (native-transcoder)))

(define (fdes->binary-output-port fd)
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
    (sys_close fd))
  (let ((p (make-custom-binary-output-port
            (string-append "fd " (number->string fd))
            write! (and position get-position) (and position set-position!) close)))
    (port-file-descriptor-set! p fd)
    p))

(define (fdes->textual-output-port fd)
  (transcoded-port (fdes->binary-output-port fd) (native-transcoder)))

(define (close-fdes fd)
  (sys_close fd)
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
       (make-syscall-error syswho errno))))
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
       (make-syscall-error syswho errno))))
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
       (make-syscall-error syswho errno))))
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
       (make-syscall-error syswho errno))))
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
       (make-syscall-error syswho errno))))
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
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'delete-directory)
       (make-message-condition "Failed to delete the directory")
       (make-irritants-condition (list fname))
       (filename-condition 'unlinkat errno fname)
       (make-syscall-error 'unlinkat errno))))
  (let ((newname-bv (filename->c-string 'delete-directory fname)))
    (with-restart
     (sys_unlinkat AT_FDCWD (& newname-bv) AT_REMOVEDIR raise-error))
    (values)))

(define (set-file-mode fname permission-bits)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-file-mode)
       (make-message-condition "Could not set file mode")
       (make-irritants-condition (list fname))
       (make-i/o-filename-error fname)
       (make-syscall-error 'fchmodat errno))))
  (let ((fname-bv (filename->c-string 'set-file-mode fname)))
    (with-restart
     (sys_fchmodat AT_FDCWD (& fname-bv) permission-bits raise-error)))
  (values))

(define (set-file-owner fname uid)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-file-mode)
       (make-message-condition "Could not set file owner")
       (make-irritants-condition (list fname uid))
       (make-i/o-filename-error fname)
       (make-syscall-error 'fchownat errno))))
  (let ((fname-bv (filename->c-string 'set-file-mode fname)))
    (with-restart
     (sys_fchownat AT_FDCWD (& fname-bv) uid -1 0 raise-error)))
  (values))

(define (set-file-group fname gid)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-file-mode)
       (make-message-condition "Could not set file group")
       (make-irritants-condition (list fname gid))
       (make-i/o-filename-error fname)
       (make-syscall-error 'fchownat  errno))))
  (let ((fname-bv (filename->c-string 'set-file-mode fname)))
    (with-restart
     (sys_fchownat AT_FDCWD (& fname-bv) -1 gid 0 raise-error)))
  (values))

(define-optional (set-file-timespecs fname [(access-timespec #f) (mod-timespec #f)])
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'set-file-timespec)
       (make-message-condition "Could not set file times")
       (make-irritants-condition (list fname access-timespec mod-timespec))
       (make-i/o-filename-error fname)
       (make-syscall-error 'utimensat errno))))
  ;; Extension: either of the timespecs can be omitted
  (let ((access-timespec
         (or access-timespec (if mod-timespec (cons 0 UTIME_OMIT) (cons 0 UTIME_NOW))))
        (mod-timespec
         (or mod-timespec (if access-timespec (cons 0 UTIME_OMIT) (cons 0 UTIME_NOW)))))
    (let ((utimes (pack "2Q 2Q" (car access-timespec) (cdr access-timespec)
                        (car mod-timespec) (cdr mod-timespec))))
      (let ((fname-bv (filename->c-string 'set-file-timespecs fname)))
        (with-restart
         (sys_utimensat AT_FDCWD (& fname-bv) (& utimes) 0 raise-error)))))
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
         (make-syscall-error syswho errno)))))
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
(define-optional (file-info filename/port [(follow? #t)])
  (define (bytevector-timespec-native-ref stat offset)
    (cons (bytevector-s64-native-ref stat offset)
          (bytevector-u64-native-ref stat (fx+ offset 8))))
  (define (raise-error errno)
    (let ((syswho (if (string? filename/port) (if follow? 'stat 'lstat) 'fstat)))
      (raise (condition
              (make-who-condition 'file-info)
              (make-message-condition "Could not stat file")
              (make-irritants-condition (list filename/port))
              (if (string? filename/port)
                  (make-i/o-filename-error filename/port)
                  (condition))
              (make-syscall-error syswho errno)))))
  (let ((statbuf (make-bytevector sizeof-stat)))
    ;; Call the right stat syscall
    (if (string? filename/port)
        (let ((fn (filename->c-string 'file-info filename/port)))
          (if follow?
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

(define (file-info-symlink? file-info)
  (eqv? (fxand (file-info:mode file-info) S_IFMT) S_IFLNK))

(define-optional (open-directory dirname [(dot-files? #f)])
  (let* ((fn (filename->c-string 'open-directory dirname))
         (fd (with-restart
              (sys_openat AT_FDCWD (& fn)
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
                               (make-syscall-error 'open errno))))))))
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
                                     (make-syscall-error 'getdents64 errno))))))))))
    (define get-position #f)
    (define set-position! #f)
    (define (close)
      (sys_close fd (lambda (errno)
                      (raise
                        (make-who-condition 'close-directory)
                        (make-message-condition "Error while closing the directory")
                        (make-i/o-filename-error dirname)
                        (make-syscall-error 'close errno)))))
    (let ((p (make-custom-binary-input-port
              dirname read! get-position set-position! close)))
      (port-file-descriptor-set! p fd)
      (make-dir p dot-files?))))

(define-record-type dir
  (fields port dot-files?))

(define-record-type directory-entry
  (fields inode offset type filename))

(define-optional (read-directory dir [(full-info? #f)])
  ;; Read and parse a "struct linux_dirent64" (which is apparently not
  ;; part of the UAPI headers). The full-info? argument is an
  ;; extension.
  (let ((port (dir-port dir)))
    (let lp ()
      (if (port-eof? port)
          #f
          (let-values ([(d_ino d_off d_reclen d_type) (get-unpack port "QQSC")])
            (let ((fn (utf8z->string (get-bytevector-n port (fx- d_reclen (format-size "QQSC")))
                                     0)))
              (if (or (and (not (dir-dot-files? dir)) (char=? (string-ref fn 0) #\.))
                      (member fn '("." "..")))
                  (lp)
                  (if full-info?
                      (make-directory-entry d_ino d_off d_type fn)
                      fn))))))))

(define (close-directory dir)
  (close-port (dir-port dir)))

(define-optional (directory-files [(dirname ".") (dotfiles? #f) (full-info? #f)])
  (let ((dir (open-directory dirname dotfiles?)))
    (unwind-protectish
     (lambda ()
       (let lp ((ret '()))
         (cond ((read-directory dir full-info?) =>
                (lambda (fn) (lp (cons fn ret))))
               (else ret))))
     (lambda ()
       (close-directory dir)))))

(define (read-symlink filename)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition 'read-symlink)
       (make-message-condition "Failed to read symlink")
       (make-irritants-condition (list filename))
       (filename-condition 'readlinkat errno filename)
       (make-syscall-error 'readlinkat errno))))
  (let ((fn (filename->c-string 'read-symlink filename)))
    (let lp ((bufsize 128))
      (let ((buf (make-bytevector bufsize)))
        (let ((len (sys_readlinkat AT_FDCWD (& fn) (& buf) bufsize raise-error)))
          (cond ((fx=? len bufsize)
                 ;; The filename was truncated
                 (lp (fx* bufsize 2)))
                (else
                 (utf8z->string buf 0))))))))

;; FIXME: Must be a SRFI-39/R7RS parameter
(define temp-file-prefix
  (make-parameter
   (let ((tmpdir (or (get-environment-variable "TMPDIR") "/tmp")))
     (string-append tmpdir "/" (number->string (sys_getpid))))))

(define (string-index-right str c)
  (do ((i (fx- (string-length str) 1) (fx- i 1)))
      ((or (fx=? i -1) (eqv? (string-ref str i) c))
       i)))

(define (make-xorshift32 seed)
  ;; http://www.jstatsoft.org/v08/i14/paper
  (let ((state seed))
    (lambda ()
      (let* ((y state)
             (y (fxxor y (fxarithmetic-shift y 13)))
             (y (fxxor y (fxarithmetic-shift y -17)))
             (y (fxxor y (fxarithmetic-shift y 5)))
             (y (fxand y #xffffffff)))
        (set! state y)
        y))))

(define make-temp-name
  (let ((rng #f))
    (lambda ()
      (unless rng
        (let ((seed
               (or (let ((buf (make-bytevector (format-size "=L"))))
                     (with-restart
                      (sys_getrandom (& buf) (bytevector-length buf) GRND_NONBLOCK
                                     (lambda _ #f)))
                     (unpack "=L" buf))
                   ;; This only happens if this code runs extremely early in
                   ;; the boot process for some happy reason, when nobody is
                   ;; around to attack the filename creation anyway. So we
                   ;; just do a funny dance.
                   (bitwise-bit-field (* (time-nanosecond (posix-time))
                                         (time-nanosecond (posix-time)))
                                      0 32))))
          (set! rng (make-xorshift32
                     (if (eqv? seed 0) (sys_getpid) seed)))))
      (let* ((len (fx+ 8 (fxand (rng) 7)))
             (ret (make-string len)))
        (do ((i 0 (fx+ i 1)))
            ((fx=? i len) ret)
          (string-set! ret i (integer->char
                              (fx+ (char->integer #\A)
                                   (fxmod (rng) 26)))))))))

(define-optional (create-temp-file [(prefix (temp-file-prefix))])
  (define who 'create-temp-file)
  (define (raise-error errno)
    (raise
      (condition
       (make-who-condition who)
       (make-message-condition "Failed to create temporary file")
       (make-irritants-condition (list prefix))
       (make-syscall-error 'linkat errno))))
  (let* ((slash (string-index-right prefix #\/))
         (dir (if (eqv? slash -1)
                  "."
                  (substring prefix 0 slash)))
         (dir-bv (filename->c-string who dir)))
    ;; Create an anonymous file
    (let* ((fd (sys_openat AT_FDCWD (& dir-bv) (fxior O_TMPFILE O_WRONLY) #o600))
           (procname (string-append "/proc/self/fd/" (number->string fd)))
           (procname-bv (filename->c-string who procname)))
      (let retry ((tries 100))
        (let* ((tempname (string-append prefix (make-temp-name)))
               (tempname-bv (filename->c-string who tempname)))
          ;; Try to link the file to a new name
          (case (with-restart
                 (sys_linkat AT_FDCWD (& procname-bv) AT_FDCWD (& tempname-bv) AT_SYMLINK_FOLLOW
                             (lambda (errno)
                               (cond ((and (eqv? errno EEXIST) (not (eqv? tries 0)))
                                      'exists)
                                     (else
                                      (sys_close fd)
                                      (raise-error errno))))))
            ((exists)
             (retry (fx- tries 1)))
            (else
             (sys_close fd)
             tempname)))))))

;; XXX: Please don't use this to create temporary files; use the
;; procedure above instead. It is hopefully secure against symlink
;; attacks.
(define-optional (call-with-temporary-filename maker [(prefix (temp-file-prefix))])
  (let retry ((tries 100))
    (let ((tempname (string-append prefix (make-temp-name))))
      (let-values ([(x . rest)
                    (guard (exn
                            ((and (syscall-errno-condition? exn)
                                  (not (eqv? 0 tries)))
                             #f))
                      (maker tempname))])
        (if (not x)
            (retry (fx- tries 1))
            (apply values x rest))))))

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
                                     (make-syscall-error 'lstat errno))))))))
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
         (make-i/o-filename-error path^))))
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
                                                  (make-i/o-filename-error path^)))
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
       (make-syscall-error 'getcwd errno))))
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
       (make-syscall-error 'chdir errno))))
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

(define-optional (nice [(delta 1)])
  (let ((current (fx- 20 (sys_getpriority PRIO_PROCESS 0))))
    (sys_setpriority PRIO_PROCESS 0 (fx+ current delta))))

(define (user-uid)
  (sys_getuid))

(define (user-gid)
  (sys_getgid))

(define (user-effective-uid)
  (sys_geteuid))

(define (user-effective-gid)
  (sys_getegid))

(define (user-supplementary-gids)
  (let* ((groups (sys_getgroups 0 0))
         (buf (make-bytevector (fx* groups sizeof-gid_t))))
    (sys_getgroups (bytevector-length buf) (& buf))
    (bytevector->uint-list buf (native-endianness) sizeof-gid_t)))

;;; 3.6 User and group database access

;; This chapter is a pain to implement fully without access to libc on
;; a system where everything else uses libc. It would be nice, in a
;; way, if /etc/passwd was a virtual file. Perhaps a file that you
;; could send queries to. One can dream...

;; If you're reading this because you found out about the limitation
;; the hard way, then please open an issue. There's a way to solve
;; this with a daemon that relays NSS info.

(define (user-info uid/name)
  (define who (if (fixnum? uid/name) "getpwuid" "getpwnam"))
  (when (not (file-exists? "/etc/passwd"))
    (error 'user-info "No /etc/passwd file" uid/name))
  (call-with-input-file "/etc/passwd"
    (lambda (p)
      (let lp ()
        (let ((line (get-line p)))
          (if (eof-object? line)
              (error 'user-info "User not found in /etc/passwd" uid/name)
              (let ((parts (string-split line #\:)))
                (if (not (fx=? (length parts) 7))
                    (lp)
                    (let ((name (car parts))
                          (uid (string->number (list-ref parts 2) 10)))
                      (if (or (equal? name uid/name) (equal? uid uid/name))
                          (apply make-user-info parts)
                          (lp)))))))))))

(define-record-type (&user-info make-user-info user-info?)
  (fields (immutable name user-info:name)
          passwd
          (immutable uid user-info:uid)
          (immutable gid user-info:gid)
          (immutable full-name user-info:full-name) ;gecos
          (immutable home-dir user-info:home-dir)
          (immutable shell user-info:shell)))

(define (user-info:parsed-full-name user-info)
  (let ((parts (string-split (user-info:full-name user-info) #\,)))
    (if (null? parts)
        '()
        (let ((part0
               (call-with-string-output-port
                 (lambda (p)
                   (string-for-each
                    (lambda (c)
                      (if (eqv? c #\&)
                          (let ((name (user-info:name user-info)))
                            (let ((c0 (string-ref name 0)))
                              (put-char p
                                        (if (char<? c0 #\delete)
                                            (char-upcase c0)
                                            c0)))
                            (put-string p (substring name 1 (string-length name))))
                          (put-char p c)))
                    (car parts))))))
          (cons part0 (cdr parts))))))

(define (group-info gid/name)
  (define who (if (fixnum? gid/name) "getgrgid" "getgrnam"))
  (when (not (file-exists? "/etc/group"))
    (error 'group-info "No /etc/group file" gid/name))
  (call-with-input-file "/etc/group"
    (lambda (p)
      (let lp ()
        (let ((line (get-line p)))
          (if (eof-object? line)
              (error 'group-info "Group not found in /etc/group" gid/name)
              (let ((parts (string-split line #\:)))
                (if (not (fx=? (length parts) 4))
                    (lp)
                    (let ((name (car parts))
                          (uid (string->number (list-ref parts 2) 10)))
                      (if (or (equal? name gid/name) (equal? uid gid/name))
                          (apply make-group-info parts)
                          (lp)))))))))))

(define-record-type (&group-info make-group-info group-info?)
  (fields (immutable name group-info:name)
          passwd
          (immutable gid group-info:gid)
          members))

;;; 3.8 System parameters

(define-record-type (&uname make-uname uname?)
  (fields (immutable os-name uname:os-name)
          (immutable node-name uname:node-name)
          (immutable release-name uname:release-name)
          (immutable version uname:version)
          (immutable machine uname:machine)
          (immutable domain-name uname:domain-name)))

(define (uname)
  (let ((buf (make-bytevector sizeof-new_utsname #xff)))
    (sys_uname (& buf))
    (make-uname (utf8z->string buf offsetof-new_utsname-sysname)
                (utf8z->string buf offsetof-new_utsname-nodename)
                (utf8z->string buf offsetof-new_utsname-release)
                (utf8z->string buf offsetof-new_utsname-version)
                (utf8z->string buf offsetof-new_utsname-machine)
                (utf8z->string buf offsetof-new_utsname-domainname))))

;;; 3.10 Time

(define (gettime clock)
  (let* ((x (make-bytevector sizeof-timespec))
         (_ (sys_clock_gettime clock (& x)))
         (seconds (bytevector-u64-native-ref x offsetof-timespec-tv_sec))
         (nanoseconds (bytevector-u64-native-ref x offsetof-timespec-tv_nsec)))
    (make-time seconds nanoseconds 1)))

(define (posix-time)
  (gettime CLOCK_REALTIME))

(define (monotonic-time)
  (gettime CLOCK_MONOTONIC))

;;; 3.12 Terminal device control

(define (terminal? port)
  (cond ((port-file-descriptor port) =>
         (lambda (fd)
           (let ((buf (make-bytevector sizeof-termios)))
             (with-restart
              (sys_ioctl fd TCGETS (& buf) (lambda _ #f)))
             #t)))
        (else #f)))

(define (terminal-file-name port)
  (unless (terminal? port)
    (assertion-violation 'terminal-file-name
                         "Expected a port connected to a terminal" port))
  (let ((proc (string-append "/proc/self/fd/"
                             (number->string (port-file-descriptor port)))))
    (let ((devname (read-symlink proc)))
      (let ((portinfo (file-info port))
            (devinfo (file-info devname)))
        (unless (and (eqv? (file-info:device portinfo) (file-info:device devinfo))
                     (eqv? (file-info:inode portinfo) (file-info:inode devinfo)))
          (error 'terminal-file-name
                 "Symlink points to wrong device" port proc devname)))
      devname)))

(define (tcgets port)
  (let ((buf (make-bytevector sizeof-termios)))
    (sys_ioctl (port-file-descriptor port) TCGETS (& buf))
    buf))

(define (tcsetsw port buf)
  (assert (fx=? (bytevector-length buf) sizeof-termios))
  (sys_ioctl (port-file-descriptor port) TCSETSW (& buf)))

(define (set-port-modes in out update-termios!)
  (let ((in-info (file-info in))
        (out-info (file-info out)))
    (cond ((and (eqv? (file-info:device in-info) (file-info:device out-info))
                (eqv? (file-info:inode in-info) (file-info:inode out-info)))
           (let ((x (tcgets in)))
             (update-termios! x)
             (tcsetsw in x)))
          (else
           (let ((x (tcgets in)))
             (update-termios! x)
             (tcsetsw in x))
           (let ((x (tcgets out)))
             (update-termios! x)
             (tcsetsw out x))))))

(define (unwind-protectish thunk on-exit)
  (with-exception-handler
    (lambda (exn)
      (on-exit)
      (raise exn))
    (lambda ()
      (let-values ([x (thunk)])
        (on-exit)
        (apply values x)))))

(define (make-restorer input-port output-port)
  (let ((saved-in (tcgets input-port))
        (saved-out (tcgets output-port)))
    (lambda ()
      (tcsetsw output-port saved-out)
      (tcsetsw input-port saved-in))))

(define (with-raw-mode input-port output-port min time proc)
  (define (update-termios! buf)
    (let-values ([(iflag oflag cflag lflag) (unpack "=4L" buf)])
      (let ((iflag (fxand iflag (fxnot (fxior BRKINT ICRNL INPCK ISTRIP IXON))))
            (oflag (fxand oflag (fxnot OPOST)))
            (cflag (fxior CS8 (fxand cflag (fxnot (fxior CSIZE PARENB)))))
            (lflag (fxand lflag (fxnot (fxior ECHO ICANON IEXTEN ISIG)))))
        (pack! "=4L" buf 0 iflag oflag cflag lflag)
        (bytevector-u8-set! buf (+ offsetof-termios-c_cc VMIN) min)
        (bytevector-u8-set! buf (+ offsetof-termios-c_cc VTIME) time))))
  (define restore (make-restorer input-port output-port))
  (unwind-protectish
   (lambda ()
     (set-port-modes input-port output-port update-termios!)
     (proc input-port output-port))
   restore))

(define (with-rare-mode input-port output-port proc)
  (define (update-termios! buf)
    (let-values ([(iflag oflag cflag lflag) (unpack "=4L" buf)])
      (let ((lflag (fxand lflag (fxnot (fxior ECHO ICANON)))))
        (pack! "=4L" buf 0 iflag oflag cflag lflag)
        (bytevector-u8-set! buf (+ offsetof-termios-c_cc VMIN) 1)
        (bytevector-u8-set! buf (+ offsetof-termios-c_cc VTIME) 0))))
  (define restore (make-restorer input-port output-port))
  (unwind-protectish
   (lambda ()
     (set-port-modes input-port output-port update-termios!)
     (proc input-port output-port))
   restore))

(define (without-echo input-port output-port proc)
  (define (update-termios! buf)
    (let-values ([(iflag oflag cflag lflag) (unpack "=4L" buf)])
      (let ((lflag (fxand lflag (fxnot (fxior ECHO ECHOE ECHOK ECHONL)))))
        (pack! "=4L" buf 0 iflag oflag cflag lflag))))
  (define restore (make-restorer input-port output-port))
  (set-port-modes input-port output-port update-termios!)
  (unwind-protectish
   (lambda ()
     (set-port-modes input-port output-port update-termios!)
     (proc input-port output-port))
   restore)))
