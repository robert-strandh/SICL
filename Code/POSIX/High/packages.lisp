(cl:in-package #:common-lisp-user)

(defpackage #:sicl-posix-high
  (:use #:common-lisp)
  (:local-nicknames (#:low #:sicl-posix-low))
  (:shadow #:read
           #:write)
  (:export #:read
           #:write))
