(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  DIRECT-SLOT-DEFINITION-CLASS and EFFECTIVE-SLOT-DEFINITION-CLASS.

(defgeneric direct-slot-definition-class (class &rest initargs))

(defmethod direct-slot-definition-class
    ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  'standard-direct-slot-definition)
;;  (find-class 'standard-direct-slot-definition))

(defmethod direct-slot-definition-class
    ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  'standard-direct-slot-definition)  
;;  (find-class 'standard-direct-slot-definition))

(defgeneric effective-slot-definition-class (class &rest initargs))

(defmethod effective-slot-definition-class
    ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  'standard-effective-slot-definition)
;;  (find-class 'standard-effective-slot-definition))

(defmethod effective-slot-definition-class
    ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  'standard-effective-slot-definition)
;;  (find-class 'standard-effective-slot-definition))

