(cl:in-package #:sicl-environment)

;;; Instances of this class are used to put compile-time information
;;; about functions in the compilation environment.  At the moment, we
;;; do not supply very much information.  This situation should be
;;; improved.

(defclass function-description ()
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%class-name :initarg :class-name :reader class-name)))

(defclass simple-function-description (function-description)
  ()
  ;; FIXME: this class name is not quite right, since SICL functions
  ;; are instances of SIMPLE-FUNCTION.
  (:default-initargs :class-name 'function))

(defun make-simple-function-description (lambda-list)
  (make-instance 'simple-function-description
    :lambda-list lambda-list))

(defclass generic-function-description (function-description)
  ((%method-class-name
    :initarg :method-class-name :reader method-class-name)
   ;; This slot contains a list starting with the name of the method
   ;; combination and followed by the method-combination arguments.
   ;; If no :METHOD-COMBINATION option was given to DEFGENERIC, then
   ;; this slot contains (STANDARD).
   (%method-combination-info
    :initarg :method-combination-info
    :initform '(standard)
    :reader method-combination-info)))

(defun make-generic-function-description
    (lambda-list class-name method-class-name &optional method-combination-info)
  (make-instance 'generic-function-description
    :lambda-list lambda-list
    :class-name class-name
    :method-class-name method-class-name
    :method-combination-info
    (if (null method-combination-info) '(standard) method-combination-info)))

(defgeneric function-description
    (environment function-name))

(defmethod function-description
    (environment function-name)
  (let ((client (client environment)))
    (clostrum:function-description client environment function-name)))

(defgeneric (setf function-description)
    (description environment function-name))

(defmethod (setf function-description)
    (description environment function-name)
  (let ((client (client environment)))
    (setf (clostrum:function-description client environment function-name)
          description)))
