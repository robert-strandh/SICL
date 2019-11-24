(cl:in-package #:sicl-array)

;;; This generic function is a reader for a slot that stores the list
;;; of dimensions in the rack.
(defgeneric array-dimensions (array))
