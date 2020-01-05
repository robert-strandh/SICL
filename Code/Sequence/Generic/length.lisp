(cl:in-package #:sicl-sequence)

(defmethod length ((datum t))
  (error 'must-be-sequence
         :datum datum))

(defmethod length ((list list))
  (cl:length list))

(defmethod length ((vector vector))
  (cl:length vector))
