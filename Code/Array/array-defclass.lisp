(cl:in-package #:sicl-array)

;;; ARRAY is a standard class, so STANDARD-OBJECT will automatically
;;; be included as a superclass.
(defclass array ()
  ((%dimensions :initarg :dimensions :reader array-dimensions)))
