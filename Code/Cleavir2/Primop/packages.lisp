(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-primop
  (:use)
  (:export
   #:fixnump
   #:consp
   #:characterp
   #:single-float-p
   #:standard-object-p
   #:eq
   #:typeq
   #:car #:cdr #:rplaca #:rplacd
   #:fixnum-arithmetic
   #:fixnum-add
   #:fixnum-sub
   #:fixnum-multiply
   #:fixnum-divide
   #:fixnum-less
   #:fixnum-not-greater
   #:fixnum-greater
   #:fixnum-not-less
   #:fixnum-equal
   #:fixnum-logand
   #:fixnum-logior
   #:fixnum-logxor
   #:fixnum-lognot
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
   #:coerce
   #:nook-read #:nook-write
   #:aref #:aset
   #:call-with-variable-bound
   #:let-uninitialized
   #:funcall
   #:multiple-value-call
   #:multiple-value-setq
   #:values
   #:unreachable
   #:ast
   #:cst-to-ast
   #:char-code
   #:code-char))
