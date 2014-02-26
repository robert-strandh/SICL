(cl:in-package #:sicl-clos)

(defmethod add-direct-subclass ((superclass class) (subclass class))
  (add-direct-subclass-default superclass subclass))

(defmethod remove-direct-subclass ((superclass class) (subclass class))
  (remove-direct-subclass-default superclass subclass))
