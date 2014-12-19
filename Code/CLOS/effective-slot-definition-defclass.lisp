(cl:in-package #:sicl-clos)

(defclass effective-slot-definition (slot-definition)
  ((%location 
    :initform nil
    :initarg :location 
    :reader slot-definition-location
    :writer (setf location))))
