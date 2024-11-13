(cl:in-package #:clostrophilia)

(defmethod initialize-instance :after
    ((class standard-class)
     &rest initargs
     &key &allow-other-keys)
  (finalize-inheritance class))
