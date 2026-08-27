(cl:in-package #:sicl-arithmetic)

(defun <= (argument &rest arguments)
  (loop for x = argument then y
        for y in arguments
        always (binary-not-greater x y)))

(setf (documentation '<= 'function)
      (format nil
              "Lambda list: (&rest NUMBERS).~@
              At least one argument is required.~@
              Return true if the numbers in NUMBERS are in~@
              monotonically non-decreasing order.~@
              The consequences are undefined if some of the objects~@
              in numbers are not real numbers, but if that is the case~@
              and a condition is signaled, that condition is an error~@
              of type TYPE-ERROR.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))
