(cl:in-package #:sicl-arithmetic)

(defun /= (argument &rest arguments)
  (if (null arguments)
      t
      (and (loop for y in arguments
                 never (binary-equal argument y))
           (apply #'/= arguments))))

(setf (documentation '/= 'function)
      (format nil
              "Lambda list: (&rest NUMBERS).~@
              At least one argument is required.~@
              Return true no two numbers in NUMBERS have the same value.~@
              The consequences are undefined if some of the objects~@
              in numbers are not real numbers, but if that is the case~@
              and a condition is signaled, that condition is an error~@
              of type TYPE-ERROR.~@
              Might signal an error of type ARITHMETIC-ERROR if~@
              unable to fulfill its contract."))
