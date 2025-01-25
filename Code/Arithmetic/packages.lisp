(cl:in-package #:common-lisp-user)

(defpackage #:sicl-arithmetic
  (:use #:common-lisp)
  (:local-nicknames (#:po #:sicl-primop))
  (:export
   #:binary-add
   #:binary-subtract
   #:binary-multiply
   #:binary-divide
   #:binary-less
   #:binary-not-greater
   #:binary-greater
   #:binary-not-less
   #:binary-equal
   #:binary-gcd
   #:binary-lcm
   #:binary-logand
   #:binary-logior
   #:binary-logxor
   #:generic-ceiling
   #:generic-floor
   #:generic-round
   #:generic-truncate
   #:sign-and-limb-count
   #:single-float-p
   #:double-float-p
   #:complex-rational
   #:complex-single-float
   #:complex-double-float
   #:bignum
   #:positive-bignum
   #:negative-bignum
   #:convert-fixnum-to-bignum
   #:most-positive-single-float #:least-positive-single-float #:least-positive-normalized-single-float #:most-negative-single-float #:least-negative-single-float #:least-negative-normalized-single-float #:most-positive-double-float #:least-positive-double-float #:least-positive-normalized-double-float #:most-negative-double-float #:least-negative-double-float #:least-negative-normalized-double-float #:most-positive-short-float #:least-positive-short-float #:least-positive-normalized-short-float #:most-negative-short-float #:least-negative-short-float #:least-negative-normalized-short-float #:most-positive-long-float #:least-positive-long-float #:least-positive-normalized-long-float #:most-negative-long-float #:least-negative-long-float #:least-negative-normalized-long-float
   #:decode-float #:scale-float #:float-radix #:float-sign #:float-digits #:float-precision #:integer-decode-float))
