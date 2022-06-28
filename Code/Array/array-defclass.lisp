(cl:in-package #:sicl-array)

;;; This generic function is a reader for a slot that stores the list
;;; of dimensions in the rack.
(defgeneric array-dimensions (array))

;;; ARRAY is a standard class, so STANDARD-OBJECT will automatically
;;; be included as a superclass.
(defclass array ()
  ((%dimensions :initarg :dimensions :reader array-dimensions)))
