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
   #:coerce
   ;; SLOT-READ and SLOT-WRITE are obsolete and should be replaced by
   ;; NOOK-READ and NOOK-WRITE.  The difference between them is that
   ;; SLOT-READ/WRITE take a slot number starting from 0, whereas
   ;; NOOK-READ/WRITE take an index into the contents vector of a
   ;; standard object.  A standard object may contain information
   ;; other than slots, so they are not quite the same thing.  For
   ;; example, if the contents vector of a standard object contains
   ;; two addtional words preceding the first slot, then a call to
   ;; SLOT-READ with a slot number 3 would be equivelent to a call to
   ;; NOOK-READ with an index of 5.
   #:slot-write
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
   #:cst-to-ast))
