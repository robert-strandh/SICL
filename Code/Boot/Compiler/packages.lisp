(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-compile-and-tie
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:compile-and-tie))
