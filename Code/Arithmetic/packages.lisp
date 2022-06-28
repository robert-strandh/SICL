(cl:in-package #:common-lisp-user)

(defpackage #:sicl-arithmetic
  (:use #:common-lisp)
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
   #:convert-fixnum-to-bignum))
