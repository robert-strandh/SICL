(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-ACCESSOR-METHOD.

(defgeneric slot-location (standard-accessor-method))

(defclass standard-accessor-method (standard-method)
  ((%slot-definition 
    :initarg :slot-definition
    :type direct-slot-definition
    :accessor accessor-method-slot-definition)
   (%slot-location
    :initarg :slot-location
    :initform nil
    :reader slot-location)))
