(cl:in-package #:sicl-arithmetic)

(defmethod binary-not-greater ((x fixnum) (y fixnum))
  (cleavir-primop:fixnum-not-greater x y))
