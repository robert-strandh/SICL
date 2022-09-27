(cl:in-package #:sicl-array)

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
