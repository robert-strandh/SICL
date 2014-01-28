(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  DIRECT-SLOT-DEFINITION-CLASS and EFFECTIVE-SLOT-DEFINITION-CLASS.

(defmethod direct-slot-definition-class
    ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  *standard-direct-slot-definition*)

(defmethod direct-slot-definition-class
    ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  *standard-direct-slot-definition*)

(defmethod effective-slot-definition-class
    ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  *standard-effective-slot-definition*)

(defmethod effective-slot-definition-class
    ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  *standard-effective-slot-definition*)
