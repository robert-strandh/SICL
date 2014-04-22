(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-primop
  (:export
   #:car #:cdr #:rplaca #:rplacd
   #:fixnum-+ #:fixnum--
   #:fixnum-< #:fixnum-<= #:fixnum-> #:fixnum->= #:fixnum-=
   #:slot-read #:slot-write
   #:aref #:aset))
