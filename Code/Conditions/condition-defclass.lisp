(cl:in-package #:sicl-conditions)

(defclass condition (standard-object)
  ((%report
    :initform nil
    :initarg :report
    :allocation :class
    :reader report))
  (:metaclass condition-class))
