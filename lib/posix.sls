;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright © 2019 Göran Weinholt
#!r6rs

;;; POSIX API for Loko Scheme

(library (loko posix)
  (export
    file-info
    file-info?
    file-info:device file-info:inode file-info:mode file-info:nlinks file-info:uid
    file-info:gid file-info:rdev file-info:size file-info:blksize file-info:blocks
    file-info:atime file-info:mtime file-info:ctime
    file-info-directory? file-info-fifo? file-info-regular? file-info-socket?
    file-info-special? file-info-symlink?

    open-directory read-directory close-directory

    read-symlink
    )
  (import
    (rnrs (6))
    (struct pack)
    (rename (loko)
            (port-file-descriptor port-fdes))
    (loko system unsafe)
    (loko arch amd64 linux-syscalls)
    (loko arch amd64 linux-numbers))

;; Convert a filename to a NUL terminated bytevector.
(define (filename->c-string who fn)
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
(define (utf8z->string src)
  (do ((len 0 (fx+ len 1)))
      ((eqv? (bytevector-u8-ref src len) 0)
       (let ((tmp (make-bytevector len)))
         (bytevector-copy! src 0
                           tmp 0 (bytevector-length tmp))
         (utf8->string tmp)))))

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

;; One of stat, lstat and fstat
(define file-info
  (cond
    ((equal? (machine-type) '#(amd64 linux))
     (case-lambda
       ((filename/port)
        (file-info filename/port #t))
       ((filename/port chase?)
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
                    (make-syscall-error syswho errno)))))
        (let ((statbuf (make-bytevector sizeof-stat)))
          ;; Call the right stat syscall
          (if (string? filename/port)
              (let ((fn (filename->c-string 'file-info filename/port)))
                (if chase?
                    (sys_stat (bytevector-address fn) (bytevector-address statbuf) raise-error)
                    (sys_lstat (bytevector-address fn) (bytevector-address statbuf) raise-error)))
              (cond ((port-fdes filename/port) =>
                     (lambda (fd)
                       (sys_fstat fd (bytevector-address statbuf) raise-error)))
                    (else
                     (assertion-violation 'file-info
                                          "Expected a port with an associated file descriptor"
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
                            size blksize blocks atime mtime ctime))))))
    (else
     (lambda (filename)
       (error 'file-info "Not implemented for this platform" filename)))))

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

(define open-directory
  (cond
    ((equal? (machine-type) '#(amd64 linux))
     (lambda (dirname)
       (let* ((fn (filename->c-string 'open-directory dirname))
              (fd (sys_open (bytevector-address fn)
                            (bitwise-ior O_CLOEXEC O_DIRECTORY)
                            0
                            (lambda (errno)
                              (raise (condition
                                      (make-who-condition 'open-directory)
                                      (make-message-condition "Could not open directory")
                                      (make-irritants-condition (list dirname))
                                      (if (eqv? errno ENOENT)
                                          (make-i/o-file-does-not-exist-error dirname)
                                          (make-i/o-filename-error dirname))
                                      (make-syscall-error 'open errno)))))))
         (define (handle-read-error errno)
           (if (eqv? errno EINTR)
               'retry
               (raise
                 (condition
                  (make-i/o-filename-error dirname)
                  (make-syscall-error 'getdents64 errno)))))
         (define (read! bv start count)
           ;; Reading should be done in another thread...
           (let ((status (sys_getdents64 fd (fx+ (bytevector-address bv) start) count
                                         handle-read-error)))
             (if (eqv? status 'retry)
                 (read! bv start count)
                 status)))
         (define get-position #f)
         (define set-position! #f)
         (define (close)
           (sys_close fd (lambda (errno)
                           (unless (eqv? errno (- EINTR))
                             (raise
                               (make-who-condition 'close-directory)
                               (make-message-condition "Error while closing the directory")
                               (make-i/o-filename-error dirname)
                               (make-syscall-error 'close errno))))))
         (let ((p (make-custom-binary-input-port
                   dirname read! get-position set-position! close)))
           (port-file-descriptor-set! p fd)
           p))))
    (else
     (lambda (dirname)
       (error 'file-info "Not implemented for this platform" dirname)))))

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
           (let ((fn (utf8z->string (get-bytevector-n dir (fx- d_reclen (format-size "QQSC"))))))
             (if full-info?
                 (make-directory-entry d_ino d_off d_type fn)
                 fn)))))))

(define (close-directory dir)
  (close-port dir))

(define directory-files
  (case-lambda
    (()
     (directory-files "."))
    ((dirname)
     (directory-files dirname #f))
    ((dirname dotfiles?)
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
                 (else ret))))))))

(define read-symlink
  (cond
    ((equal? (machine-type) '#(amd64 linux))
     (lambda (filename)
       (let ((fn (filename->c-string 'read-symlink filename)))
         (let lp ((bufsize 1))
           (let ((buf (make-bytevector bufsize)))
             (let ((len (sys_readlink (bytevector-address fn)
                                      (bytevector-address buf)
                                      bufsize)))
               (cond ((fx=? len bufsize)
                      ;; The filename was truncated
                      (lp (fx* bufsize 2)))
                     (else
                      (utf8->string buf)))))))))))

)
