(cl:in-package #:sicl-sequence)

(defmethod length ((list list))
  (list-length list))

(defmethod length ((sequence sequence))
  (non-list-length sequence))
