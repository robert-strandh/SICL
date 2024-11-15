(cl:in-package #:clostrophilia)

(defmethod initialize-instance :after
    ((class standard-class)
     &rest initargs
     &key &allow-other-keys)
  (finalize-inheritance class))

(defmethod initialize-instance :after
    ((class funcallable-standard-class)
     &rest initargs
     &key &allow-other-keys)
  (finalize-inheritance class))
