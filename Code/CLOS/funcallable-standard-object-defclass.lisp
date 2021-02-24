(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCALLABLE-STANDARD-OBJECT.

(defgeneric entry-point (funcallable-standard-object))

(defgeneric (setf entry-point) (entry-point funcallable-standard-object))

(defclass funcallable-standard-object (standard-object function)
  ((%entry-point :initarg :entry-point :accessor entry-point)
   (%environment :initform nil :initarg :environment :accessor environment))
  (:metaclass funcallable-standard-class))
