(cl:in-package #:sicl-clos)

(defclass effective-slot-definition (slot-definition)
  ((%location 
    :initform nil
    :initarg :location 
    :accessor slot-definition-location)))
