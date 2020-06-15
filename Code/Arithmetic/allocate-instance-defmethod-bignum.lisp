(cl:in-package #:sicl-arithmetic)

(defmethod allocate-instance ((class (eql (find-class 'bignum))) &key limb-count)
  (sicl-clos:allocate-instance-common class (+ 2 limb-count)))
