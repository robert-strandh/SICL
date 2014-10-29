(cl:in-package #:common-lisp-user)

(defpackage #:sicl-direct-extrinsic-compiler
  (:use #:common-lisp)
  (:shadow #:function)
  (:export #:compile))
