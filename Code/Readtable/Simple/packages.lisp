(cl:in-package #:common-lisp-user)

(defpackage #:sicl-simple-readtable
  (:use #:common-lisp)
  (:shadow #:readtable)
  (:export #:readtable))
