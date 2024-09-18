(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot-backtrace-inspector
  (:use #:common-lisp)
  (:local-nicknames (#:cbae #:common-boot-ast-interpreter))
  (:shadow #:inspect #:inspector)
  (:export #:inspect))
