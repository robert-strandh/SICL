(cl:in-package #:sicl-array)

(defmethod acclimation:report-condition
    ((condition object-must-be-an-array)
     stream
     (language acclimation:english))
  (format stream
          "An object of type ~s was expected,~@
           but the following object was found instead~@
           ~s"
          (type-error-expected-type condition)
          (type-error-datum condition)))

(defmethod acclimation:report-condition
  ((condition number-of-indices-must-equal-array-rank)
   stream
     (language acclimation:english))
  (format stream
          "The following indices:~@
           ~s~@
           were given in order to access the array:~@
           ~s~.@
           However, the number of indices given is ~d~@
           whereas the randk of the array is ~d."
          (indices condition)
          (given-array condition)
          (length (indices condition))
          (array-rank (given-array condition))))

(defmethod acclimation:report-condition
    ((condition index-must-be-non-negative-and-less-than-dimension)
     stream
     (language acclimation:english))
  (format stream
          "The ~:R index given in order to access the array:~@
           ~s~@
           has a value of ~s~@
           but the ~:R dimension of the array is ~d,~@
           so the index must be a non-negative integer~@
           that is strictly less than that dimension."
          (1+ (index-number condition))
          (given-array condition)
          (type-error-datum condition)
          (1+ (index-number condition))
          (array-dimension (given-array condition) (index-number condition))))

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
