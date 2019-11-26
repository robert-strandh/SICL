(cl:in-package #:sicl-array)

(defmethod acclimation:report-condition
    ((condition argument-to-array-element-type-must-be-an-array)
     stream
     (language acclimation:english))
  (format stream
	  "An object of type ~s was expected,~@
           but the following object was found instead~@
           ~s"
          (type-error-expected-type condition)
          (type-error-datum condition)))

(defmethod acclimation:report-condition
    ((condition argument-to-array-displacement-must-be-an-array)
     stream
     (language acclimation:english))
  (format stream
	  "An object of type ~s was expected,~@
           but the following object was found instead~@
           ~s"
          (type-error-expected-type condition)
          (type-error-datum condition)))
