(cl:in-package #:common-lisp-user)

(defpackage #:sicl-system-construction
  (:use #:common-lisp)
  (:shadow #:*features*
           #:*modules*)
  (:export #:*features*
           #:*modules*))
