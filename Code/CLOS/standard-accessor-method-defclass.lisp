(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-ACCESSOR-METHOD.

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/accessor-method-slot-definition.html
(defgeneric accessor-method-slot-definition (accessor-method))

(defgeneric (setf accessor-method-slot-definition)
    (slot-definition accessor-method))

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
