(cl:in-package #:sicl-character)

(defmethod equalp ((x character) (y character))
  (char-equal x y))
