(cl:in-package #:sicl-extrinsic-environment)

(defmethod initialize-instance :after
    ((object environment) &key &allow-other-keys)
  (fill-environment object))

(defmethod initialize-instance :around
    ((object environment) &key (cache-p nil cache-p-p) &allow-other-keys)
  (if cache-p-p
      (let ((*cache-p* cache-p))
        (call-next-method))
      (call-next-method)))
