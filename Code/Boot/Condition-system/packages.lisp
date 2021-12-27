(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-condition-system
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:boot))
