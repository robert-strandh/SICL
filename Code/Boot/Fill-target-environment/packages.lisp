(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-fill-target-environment
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:boot))
