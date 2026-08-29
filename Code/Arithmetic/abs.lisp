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

(setf (documentation 'abs 'function)
      (format nil
              "Lambda list: (NUMBER).~@
               Return the absolute value of the number NUMBER.~@
               If given a real number, the result type is the same~@
               as that of the number given, so that if for instance~@
               a double float is given, then the result is a double
               float as well.~@
               If given a complex number, the result is a real number.~@
               In that case, the return value might be a floating point number,~@
               even if the result could be expressed as an exact rational number.~@
               The consequences are undefine if NUMBER is not a number."))
