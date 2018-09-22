(cl:in-package #:sicl-clos)

(defmethod finalize-inheritance ((class real-class))
  (finalize-inheritance-default class))

;;; Use REAL-CLASS for now.
(defmethod compute-default-initargs ((class real-class))
  (compute-default-initargs-default class))

;;; Use REAL-CLASS for now.
(defmethod compute-slots :around ((class real-class))
  (declare (ignorable class))
  (let ((slots (call-next-method)))
    (compute-slots-around-default slots)))

;;; Use REAL-CLASS for now.
(defmethod compute-slots ((class real-class))
  (compute-slots-default class))

;;; Use REAL-CLASS for now.
(defmethod compute-effective-slot-definition ((class real-class)
                                              name
                                              direct-slot-definitions)
  (compute-effective-slot-definition-default
   name
   direct-slot-definitions
   (effective-slot-definition-class class)))

(defmethod compute-class-precedence-list ((class class))
  (compute-class-precedence-list-default class))
