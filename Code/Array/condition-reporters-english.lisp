(cl:in-package #:sicl-array)

(defmethod acclimation:report-condition
    ((condition row-major-index-must-be-non-negative-and-less-than-total-size)
     stream
     (language acclimation:english))
  (format stream
          "The row-major index given in order to access the array:~@
           ~s~@
           has a value of ~s~@
           but the total size of the array is ~d,~@
           so the index must be a non-negative integer~@
           that is strictly less than that size."
          (given-array condition)
          (type-error-datum condition)
          (array-total-size (given-array condition))))

(defmethod acclimation:report-condition
    ((condition incompatible-type-for-new-element)
     stream
     (language acclimation:english))
  (format stream
          "The new element to be stored in the array is:~@
           ~s of type ~s,~@
           but the element type of the array to store it in:
           ~s~@
           is ~s,~@
           which is incompatible with the type of the new element."
          (type-error-datum condition)
          (type-of (type-error-datum condition))
          (given-array condition)
          (type-error-expected-type condition)))
