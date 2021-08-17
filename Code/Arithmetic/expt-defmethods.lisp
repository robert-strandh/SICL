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

(defmethod expt ((base ratio) (power integer))
  (cond ((zerop power) 1)
        ((= power 1) base)
        (t
         (flet (;; In the below, we know num and den are coprime, so we
                ;; build a ratio directly rather than divide.
                (%ratio (num den)
                  (make-instance 'ratio :numerator num :denominator den)))
           (let ((num (numerator base)) (den (denominator base)))
             (if (minusp power)
                 (let ((npower (- power)))
                   (cond ((= num 1) (expt den npower))
                         ((= num -1) (expt (- den) npower))
                         (t (%ratio (expt den npower) (expt num npower)))))
                 (%ratio (expt num npower) (expt den npower))))))))
