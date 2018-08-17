(cl:in-package #:sicl-clos)

(defmethod shared-initialize :around
    ((generic-function generic-function)
     slot-names
     &rest initargs
     &key &allow-other-keys)
  (apply #'shared-initialize-around-generic-function-default
         #'call-next-method
         generic-function
         slot-names
         initargs))
