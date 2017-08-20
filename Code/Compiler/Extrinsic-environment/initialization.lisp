(cl:in-package #:sicl-extrinsic-environment)

(defmethod initialize-instance :after
    ((object environment) &key &allow-other-keys)
  (fill-environment object))

(defmethod initialize-instance :around
    ((object environment) &key cache-p &allow-other-keys)
  (let ((*cache-p* cache-p))
    (call-next-method)))
