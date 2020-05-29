(cl:in-package #:sicl-arithmetic)

(defmethod binary-not-greater ((x fixnum) (y fixnum))
  (if (cleavir-primop:fixnum-not-greater x y) t nil))
