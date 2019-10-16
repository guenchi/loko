;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: MIT
;; Copyright © 2019 Göran Weinholt
#!r6rs

;;; SRFI-170 (POSIX)

(library (srfi :170)
  (export
    fdes->textual-input-port
    fdes->binary-input-port
    fdes->textual-output-port
    fdes->binary-output-port
    port-fdes close-fdes

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
    make-directory-files-generator
    open-directory read-directory close-directory

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

    posix-time monotonic-time

    terminal? terminal-file-name
    with-raw-mode with-rare-mode without-echo)

  (import
    (srfi :170 posix)))
