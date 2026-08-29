(cl:in-package #:sicl-arithmetic)

(defgeneric abs (number))

(defmethod abs ((number real))
  (if (minusp number)
      (- number)
      number))

(defmethod abs ((number complex))
  (let ((r (realpart number))
        (i (imagpart number)))
    (sqrt (* r r) (* i i))))
