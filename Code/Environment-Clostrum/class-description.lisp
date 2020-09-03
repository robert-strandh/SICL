(cl:in-package #:sicl-environment)

;;; Instances of this class are used to put compile-time information
;;; about classes in the compilation environment.

(defclass class-description ()
  ((%name :initarg :name :reader name)
   (%superclass-names :initarg :superclass-names :reader superclass-names)
   (%metaclass-name :initarg :metaclass-name :reader metaclass-name)))
