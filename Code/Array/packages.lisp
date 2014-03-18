(cl:in-package #:common-lisp-user)

(defpackage #:sicl-array
  (:use #:common-lisp)
  (:export
   #:array
   #:arrayp
   #:make-array
   #:array-rank
   #:array-dimensions
   #:array-dimension
   ))
