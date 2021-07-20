(cl:in-package #:sicl-character)

(defmethod equal ((x character) (y character))
  (eql x y))
