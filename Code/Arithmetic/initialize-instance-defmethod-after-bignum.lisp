(cl:in-package #:sicl-arithmetic)

(defmethod initialize-instance :after ((object bignum) &key limb-count)
  (loop for i from 3
        repeat limb-count
        do (cleavir-primop:nook-write object i 0)))
