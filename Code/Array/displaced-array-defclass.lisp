(cl:in-package #:sicl-array)

(defclass displaced-array (array)
  ((%target :initarg :target :reader target)
   (%ofset :initarg :ofset :reader ofset)))
