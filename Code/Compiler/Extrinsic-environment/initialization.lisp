(cl:in-package #:sicl-extrinsic-environment)

(defmethod initialize-instance :after
    ((object environment) &key &allow-other-keys)
  (fill-environment object))
