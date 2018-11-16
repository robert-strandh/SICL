(cl:in-package #:sicl-boot-phase-2)

(defclass header (closer-mop:funcallable-standard-object)
  ((%class :initarg :class)
   (%rack :initarg :rack))
  (:metaclass closer-mop:funcallable-standard-class))
