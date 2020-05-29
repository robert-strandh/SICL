(cl:in-package #:sicl-arithmetic)

(defmethod binary-equal ((x fixnum) (y fixnum))
  (if (cleavir-primop:fixnum-equal x y) t nil))
