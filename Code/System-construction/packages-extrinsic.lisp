(cl:in-package #:common-lisp-user)

(defpackage #:sicl-system-construction
  (:use #:common-lisp)
  (:shadow #:*features*
           #:*modules*
           #:*compile-file-pathname*)
  (:export #:*features*
           #:*modules*
           #:*compile-file-pathname*))
