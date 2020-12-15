(cl:in-package #:sicl-structure)

(defgeneric has-standard-constructor (structure-class))

(defclass structure-class (sicl-clos:regular-class)
  ((%has-standard-constructor :initarg :has-standard-constructor
                              :reader has-standard-constructor))
  (:default-initargs :has-standard-constructor nil))
