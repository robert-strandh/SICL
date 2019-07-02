(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-inspector
  (:use #:common-lisp)
  (:shadow #:inspect)
  (:export #:inspect))
