(cl:in-package #:sicl-conditions)

;;; This class is included as a superclass of a condition class if the
;;; DEFINE-CONDITION form contains the :REPORT option.
(defclass report-mixin ()
  ((%report :initarg :report :allocation :class :reader report)))
