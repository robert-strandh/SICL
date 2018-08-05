(cl:in-package #:sicl-minimal-extrinsic-environment)

(defmethod initialize-instance :after
    ((object environment) &key system &allow-other-keys)
  (fill-environment object system))

