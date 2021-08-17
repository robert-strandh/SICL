(cl:in-package #:sicl-arithmetic)

(defmethod expt ((base integer) (power integer))
  (cond ((zerop power) 1)
        ((minusp power) (/ (expt base (- power))))
        ((= power 1) base)
        ((= base 2) (ash 1 power))
        (t ; binary exponentiation
         (do ((base base (* base base))
              (power power (ash power -1))
              (result 1 (if (oddp power) (* result base) result)))
             ((zerop power result))))))
