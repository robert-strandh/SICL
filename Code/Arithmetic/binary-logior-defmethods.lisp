(cl:in-package #:sicl-arithmetic)

(defmethod binary-logior ((x fixnum) (y fixnum))
  (cleavir-primop:fixnum-logior x y))
