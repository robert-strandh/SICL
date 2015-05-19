(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-primop
  (:use)
  (:export
   #:typeq
   #:consp
   #:car #:cdr #:rplaca #:rplacd
   #:fixnum-arithmetic #:fixnum-+ #:fixnum--
   #:fixnum-< #:fixnum-<= #:fixnum-> #:fixnum->= #:fixnum-=
   #:slot-read #:slot-write
   #:aref #:aset
   #:call-with-variable-bound))
