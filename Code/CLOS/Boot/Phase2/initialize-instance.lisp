(cl:in-package #:sicl-clos)

(defmethod initialize-instance (object &rest keys &key)
  (declare (ignore object keys))
  nil)

(cl:defmethod cl:initialize-instance :after
  ((metaobject metaobject) &rest keys &key &allow-other-keys)
  (apply #'initialize-instance metaobject keys))
