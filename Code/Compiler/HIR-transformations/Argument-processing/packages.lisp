(cl:in-package #:common-lisp-user)

(defpackage #:sicl-argument-processing
  (:use #:common-lisp)
  (:export #:check-minimum-argument-count
           #:check-maximum-argument-count
           #:initialize-required-parameters))
