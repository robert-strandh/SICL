(cl:in-package #:sicl-array)

(defgeneric row-major-aref (array index))

(defgeneric (setf row-major-aref) (new-element array index))
