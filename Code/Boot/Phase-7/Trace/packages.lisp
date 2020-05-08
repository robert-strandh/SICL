(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-trace
  (:use #:common-lisp)
  (:shadow #:trace
           #:untrace)
  (:export #:trace
           #:untrace
           #:trace-all))
