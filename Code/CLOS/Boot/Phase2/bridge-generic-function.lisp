(cl:in-package #:sicl-clos)

(cl:defclass bridge-generic-function
    (#+sbcl sb-pcl:funcallable-standard-object
     standard-generic-function)
  ()
  (:metaclass #+sbcl sb-pcl:funcallable-standard-class))

  
