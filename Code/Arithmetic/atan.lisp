(cl:in-package #:sicl-arithmetic)

(defgeneric atan (argument))

(defmethod atan ((argument single-float))
  (sicl-primop:primop :single-float-atan argument))

(defmethod atan ((argument double-float))
  (sicl-primop:primop :double-float-atan argument))

(defmethod atan ((argument rational))
  (atan (float argument)))

(defmethod atan ((argument number))
  (/ (log (/ (- (complex 0 1) argument)
             (+ (complex 0 1) argument)))
     (complex 0 2)))

(setf (documentation 'atan 'function)
      (format nil
              "Lambda list: (NUMBER1 &optional NUMBER2).~@
               If NUMBER2 is not supplied, return the arc tangent~@
               of the number NUMBER1.  In that case, NUMBER1 can be~@
               any number.~@
               If NUMBER2 is supplied, return the arc tangent of~@
               NUMBER1/NUMBER2.  In that case, NUMBER1 and NUMBER2 must~@
               both be real numbers.~@
               If NUMBER2 is not given, the result is in the interval~@
               ]-pi/2,pi/2[~@
               If number2 is given, the result is in the interval~@
               [-pi,pi[ when minus zero is NOT supported, and in the interval~@
               [-pi,pi] when minus zero IS supported.~@
               If NUMBER2 is not supplied and NUMBER1 is not a number,~@
               then an error of type TYPE-ERROR is signaled.~@
               If NUMBER2 is supplied, and at least one of the two~@
               numbers is not real, an error of type TYPE-ERROR is signaled.~@
               Might signal an error of type ARITHMETIC-ERROR if~@
               unable to fulfill its contract."))
