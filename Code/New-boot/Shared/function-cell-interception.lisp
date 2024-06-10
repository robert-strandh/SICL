(cl:in-package #:sicl-new-boot)

(defmethod clostrum-sys:ensure-operator-cell :around
    ((client client) environment operator-name)
  (call-next-method))
