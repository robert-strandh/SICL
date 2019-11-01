(cl:in-package #:common-lisp-user)

(defpackage #:sicl-system-construction
  (:use #:common-lisp)
  (:shadow #:*features*
           #:*modules*
           #:*compile-file-pathname*
           #:*compile-file-truename*)
  (:export #:*features*
           #:*modules*
           #:*compile-file-pathname*
           #:*compile-file-truename*))
