(cl:in-package #:sicl-array)

;;; All our vectors have a slot for the fill pointer.  When a vector
;;; is created without a fill pointer, we set the slot to NIL.

(defclass vector (array sequence)
  ((%fill-pointer
    :initform nil
    :initarg :fill-pointer
    :accessor vector-fill-pointer)))
