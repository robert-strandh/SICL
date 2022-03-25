(cl:in-package #:sicl-environment)

;;; Instances of this class are used to put compile-time information
;;; about classes in the compilation environment.

(defclass class-description ()
  ((%name :initarg :name :reader name)
   (%superclass-names :initarg :superclass-names :reader superclass-names)
   (%metaclass-name :initarg :metaclass-name :reader metaclass-name)))

(defun make-class-description (name superclass-names metaclass-name)
  (make-instance 'class-description
    :name name
    :superclass-names superclass-names
    :metaclass-name metaclass-name))

(defgeneric class-description (environment class-name))

(defmethod class-description (environment class-name)
  (let ((client (client environment)))
    (clostrum:class-description client environment class-name)))

(defgeneric (setf class-description) (description environment class-name))

(defmethod (setf class-description) (description environment class-name)
  (let ((client (client environment)))
    (setf (clostrum:class-description client environment class-name)
          description)))
