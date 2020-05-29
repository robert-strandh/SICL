(cl:in-package #:sicl-arithmetic)

(defmethod binary-equal ((x fixnum) (y fixnum))
  (cleavir-primop:fixnum-equal x y))
