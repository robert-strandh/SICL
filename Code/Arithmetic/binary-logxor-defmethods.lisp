(cl:in-package #:sicl-arithmetic)

(defmethod binary-logxor ((x fixnum) (y fixnum))
  (cleavir-primop:fixnum-logxor x y))
