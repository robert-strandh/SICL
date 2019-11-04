(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-backtrace-inspector
  (:use #:common-lisp)
  (:shadow #:inspect #:inspector)
  (:export #:inspect))
