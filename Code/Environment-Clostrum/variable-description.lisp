(cl:in-package #:sicl-environment)

;;; Instances of this class are used to put compile-time information
;;; about variables in the compilation environment.

(defclass variable-description ()
  ((%type :initarg :type :initform t :reader type)))

(defclass special-variable-description (variable-description)
  ())

(defun make-special-variable-description ()
  (make-instance 'special-variable-description))

(defclass constant-variable-description (variable-description)
  ((%value :initarg :value :reader value)))

(defun make-constant-variable-description (value)
  (make-instance 'constant-variable-description
    :value value))

(defgeneric variable-description
    (environment variable-name))

(defgeneric (setf variable-description)
    (description environment variable-name))
