(cl:in-package #:common-lisp-user)

(defpackage #:maclina-backtrace-inspector
  (:use #:common-lisp)
  (:shadow #:inspect #:inspector)
  (:export #:inspect))
