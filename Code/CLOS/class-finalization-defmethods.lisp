(cl:in-package #:sicl-clos)

(defmethod finalize-inheritance ((class standard-class))
  (finalize-inheritance-default class))

(defmethod finalize-inheritance ((class funcallable-standard-class))
  (finalize-inheritance-default class))

(defmethod compute-default-initargs ((class standard-class))
  (compute-default-initargs-default class))

(defmethod compute-default-initargs ((class funcallable-standard-class))
  (compute-default-initargs-default class))

(defmethod compute-slots :around ((class standard-class))
  (let ((slots (call-next-method)))
    (compute-slots-around-default slots)))

(defmethod compute-slots :around ((class funcallable-standard-class))
  (let ((slots (call-next-method)))
    (compute-slots-around-default slots)))

(defmethod compute-slots ((class standard-class))
  (compute-slots-default class))

(defmethod compute-slots ((class funcallable-standard-class))
  (compute-slots-default class))

(defmethod compute-effective-slot-definition ((class standard-class)
					      name
					      direct-slot-definitions)
  (compute-effective-slot-definition-default
   name
   direct-slot-definitions
   (effective-slot-definition-class class)))

(defmethod compute-effective-slot-definition ((class funcallable-standard-class)
					      name
					      direct-slot-definitions)
  (compute-effective-slot-definition-default
   name
   direct-slot-definitions
   (effective-slot-definition-class class)))
  
(defmethod compute-class-precedence-list ((class class))
  (compute-class-precedence-list-default class))
