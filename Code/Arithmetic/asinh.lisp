(cl:in-package #:sicl-arithmetic)

(setf (documentation 'asinh 'function)
      (format nil
              "Lambda list: (NUMBER).~@
               Return the hyperbolic arc sine of the number NUMBER.~@
               If NUMBER is not a number, then an error of type~@
               TYPE-ERROR is signaled.~@
               Might signal an error of type ARITHMETIC-ERROR if~@
               unable to fulfill its contract."))
