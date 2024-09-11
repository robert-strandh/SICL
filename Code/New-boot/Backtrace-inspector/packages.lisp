(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot-backtrace-inspector
  (:use #:common-lisp)
  (:local-nicknames (#:cbae #:common-boot-fast-ast-evaluator))
  (:shadow #:inspect #:inspector)
  (:export #:inspect))
