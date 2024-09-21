(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot-backtrace-inspector
  (:use #:common-lisp)
  (:local-nicknames (#:cb #:common-boot))
  (:shadow #:inspect #:inspector)
  (:export #:inspect))
