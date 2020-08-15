(cl:in-package #:sicl-structure)

;;; The only difference between this extrinsic definition and the intrinsic
;;; definition is that this definition inherits from standard-class, not
;;; sicl-clos:regular-class.
(defclass structure-class (standard-class)
  ((%has-standard-constructor :initarg :has-standard-constructor
                              :reader has-standard-constructor))
  (:default-initargs :has-standard-constructor nil))
