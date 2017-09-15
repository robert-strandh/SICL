(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-primop
  (:use)
  (:export
   #:eq
   #:typeq
   #:car #:cdr #:rplaca #:rplacd
   #:fixnum-arithmetic
   #:fixnum-add
   #:fixnum-sub
   #:fixnum-less
   #:fixnum-not-greater
   #:fixnum-greater
   #:fixnum-not-less
   #:fixnum-equal
   ;; Each of these operations takes a type argument in addition to
   ;; the normal argument(s) of the corresponding Common Lisp
   ;; function.  That type argument is the first one, and it is not
   ;; evaluated.
   #:float-add
   #:float-sub
   #:float-mul
   #:float-div
   #:float-less
   #:float-not-greater
   #:float-greater
   #:float-not-less
   #:float-equal
   #:float-sin
   #:float-cos
   #:float-sqrt
   ;; The following floating-point operations are deprecated in favor
   ;; of the ones above.
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
   #:double-float-add
   #:double-float-sub
   #:double-float-mul
   #:double-float-div
   #:double-float-less
   #:double-float-not-greater
   #:double-float-greater
   #:double-float-not-less
   #:double-float-equal
   #:double-float-sin
   #:double-float-cos
   #:double-float-sqrt
   #:long-float-add
   #:long-float-sub
   #:long-float-mul
   #:long-float-div
   #:long-float-less
   #:long-float-not-greater
   #:long-float-greater
   #:long-float-not-less
   #:long-float-equal
   #:long-float-sin
   #:long-float-cos
   #:long-float-sqrt
   #:coerce
   #:slot-read #:slot-write
   #:aref #:aset
   #:call-with-variable-bound
   #:let-uninitialized
   #:funcall
   #:multiple-value-call
   #:values
   #:unreachable
   #:ast))
