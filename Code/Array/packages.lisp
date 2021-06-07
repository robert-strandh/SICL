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
   #:array-bit
   #:array-t
   #:array-character
   #:array-single-float
   #:array-double-float
   #:array-signed-byte-32
   #:array-signed-byte-64
   #:array-unsigned-byte-32
   #:array-unsigned-byte-64
   #:array-unsigned-byte-8
   #:array-complex-single-float
   #:array-complex-double-float
   #:vector-single-float
   #:vector-double-float
   #:vector-signed-byte-32
   #:vector-signed-byte-64
   #:vector-unsigned-byte-32
   #:vector-unsigned-byte-64
   #:vector-unsigned-byte-8
   #:vector-complex-single-float
   #:vector-complex-double-float))
