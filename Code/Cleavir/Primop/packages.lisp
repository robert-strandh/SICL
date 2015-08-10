(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-primop
  (:use)
  (:export
   #:eq
   #:typeq
   #:consp
   #:car #:cdr #:rplaca #:rplacd
   #:fixnum-arithmetic #:fixnum-+ #:fixnum--
   #:fixnum-< #:fixnum-<= #:fixnum-> #:fixnum->= #:fixnum-=
   #:short-float-add
   #:short-float-sub
   #:short-float-mul
   #:short-float-div
   #:short-float-less
   #:short-float-not-greater
   #:short-float-greater
   #:short-float-not-less
   #:short-float-equal
   #:short-float-sin
   #:short-float-cos
   #:short-float-sqrt
   #:single-float-add
   #:single-float-sub
   #:single-float-mul
   #:single-float-div
   #:single-float-less
   #:single-float-not-greater
   #:single-float-greater
   #:single-float-not-less
   #:single-float-equal
   #:single-float-sin
   #:single-float-cos
   #:single-float-sqrt
   #:slot-read #:slot-write
   #:aref #:aset
   #:call-with-variable-bound
   #:let-uninitialized))
