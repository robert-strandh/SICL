(cl:in-package #:sicl-extrinsic-environment)

(defmethod initialize-instance :after
    ((object environment) &key system &allow-other-keys)
  (fill-environment object system))

(defmethod initialize-instance :around
    ((object environment) &key (cache-p nil cache-p-p) &allow-other-keys)
  (if cache-p-p
      (let ((*cache-p* cache-p))
        (call-next-method))
      (call-next-method)))
