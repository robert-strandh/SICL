(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-primop
  (:use)
  (:export
   #:eq
   #:typeq
   #:car #:cdr #:rplaca #:rplacd
   #:fixnum-arithmetic #:fixnum-+ #:fixnum--
   #:fixnum-< #:fixnum-<= #:fixnum-> #:fixnum->= #:fixnum-=
   #:fixnum-add
   #:fixnum-sub
   #:fixnum-less
   #:fixnum-not-greater
   #:fixnum-greater
   #:fixnum-not-less
   #:fixnum-equal
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
   #:slot-read #:slot-write
   #:simple-t-aref #:simple-t-aset
   #:non-simple-t-aref #:non-simple-t-aset
   #:simple-bit-aref #:simple-bit-aset
   #:non-simple-bit-aref #:non-simple-bit-aset
   #:simple-short-float-aref #:simple-short-float-aset
   #:non-simple-short-float-aref #:non-simple-short-float-aset
   #:simple-single-float-aref #:simple-single-float-aset
   #:non-simple-single-float-aref #:non-simple-single-float-aset
   #:simple-double-float-aref #:simple-double-float-aset
   #:non-simple-double-float-aref #:non-simple-double-float-aset
   #:simple-long-float-aref #:simple-long-float-aset
   #:non-simple-long-float-aref #:non-simple-long-float-aset
   #:simple-unsigned-byte-8-aref-ast
   #:simple-unsigned-byte-8-aset-ast
   #:non-simple-unsigned-byte-8-aref-ast
   #:non-simple-unsigned-byte-8-aset-ast
   #:simple-unsigned-byte-16-aref-ast
   #:simple-unsigned-byte-16-aset-ast
   #:non-simple-unsigned-byte-16-aref-ast
   #:non-simple-unsigned-byte-16-aset-ast
   #:call-with-variable-bound
   #:let-uninitialized
   #:funcall
   #:values))
