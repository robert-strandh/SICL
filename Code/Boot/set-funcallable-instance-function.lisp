(cl:in-package #:sicl-clos)

(defun set-funcallable-instance-function (generic-function function)
  #+sbcl(sb-pcl:set-funcallable-instance-function generic-function function)
  #-sbcl(error "Can't set funcallable instance function")
  )
