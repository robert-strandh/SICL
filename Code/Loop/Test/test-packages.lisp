(cl:in-package #:common-lisp-user)

(defpackage #:sicl-loop-test
  (:use #:common-lisp)
  (:shadow #:loop #:loop-finish))
