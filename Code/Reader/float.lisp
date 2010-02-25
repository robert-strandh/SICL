;;; This is an implementation of the simple algorithm shown by Clinger
;;; in his paper "How to Read Floating point Numbers Accurately". 
;;; The precision parameter is 24 for single-precision IEEE floats
;;; and 53 bits for IEEE floats, i.e., the hidden bit is counted.

(defun nextfloat (significand exponent precision)
  (if (= significand (1- (ash 1 precision)))
      (values (ash 1 (1- precision)) (1+ exponent))
      (values (1+ significand) exponent)))

(defun ratio-to-float-components (numerator denominator exponent precision)
  (multiple-value-bind (quotient remainder) (floor numerator denominator)
    (let ((v-r (- denominator remainder)))
      (cond ((< remainder v-r) (values quotient exponent))
	    ((> remainder v-r) (nextfloat quotient exponent))
	    ((evenp quotient) (values quotient exponent))
	    (t (nextfloat quotient exponent precision))))))

(defun clinger1 (f e precision)
  (flet ((aux (numerator denominator exponent)
	   (loop while (< (floor numerator denominator) (ash 1 (1- precision)))
		 do (setf numerator (ash numerator 1))
		 do (decf exponent))
	   (loop while (>= (floor numerator denominator) (ash 1 precision))
		 do (setf denominator (ash denominator 1))
		 do (incf exponent))
	   (ratio-to-float-components numerator denominator exponent precision)))
    (if (minusp e)
	(aux f (expt 10 (- e)) 0)
	(aux (* f (expt 10 e)) 1 0))))

(defun normalize (rational)
  (let ((exponent 0))
    (loop while (< rational 1/2)
	  do (setf rational (* rational 2))
	  do (decf exponent))
    (loop while (>= rational 1)
	  do (setf rational (/ rational 2))
	  do (incf exponent))
    (values rational exponent)))

