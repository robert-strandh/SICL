(cl:in-package #:common-lisp-user)

(defpackage #:buoy
  (:use #:cl)
  (:export #:single-copysign
           #:double-copysign
           #:single-next-before #:single-next-after
           #:double-next-before #:double-next-after 
           #:decode-single-float #:integer-decode-single-float
           #:decode-double-float #:integer-decode-double-float
           #:single-float-precision #:scale-single-float
           #:double-float-precision #:scale-double-float))
