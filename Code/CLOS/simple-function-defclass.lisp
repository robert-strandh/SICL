(cl:in-package #:sicl-clos)

(defclass simple-function (funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))
