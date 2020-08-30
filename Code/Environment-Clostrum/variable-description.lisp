(cl:in-package #:sicl-environment)

;;; Instances of this class are used to put compile-time information
;;; about variables in the compilation environment.

(defclass variable-description ()
  ((%type :initarg :type :initform t :reader type)))

(defclass special-variable-description (variable-description)
  ())

(defclass constant-variable-description (variable-description)
  ())
