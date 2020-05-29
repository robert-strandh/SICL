(cl:in-package #:sicl-arithmetic)

(defmethod binary-less ((x fixnum) (y fixnum))
  (if (cleavir-primop:fixnum-less x y) t nil))
