(cl:in-package #:common-lisp)

(defpackage #:sicl-iteration
  (:use #:common-lisp)
  (:shadow #:dolist #:dotimes #:do #:do*)
  (:export #:dolist #:dotimes #:do #:do*))
