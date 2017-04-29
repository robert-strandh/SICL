(cl:in-package #:sicl-clos)

(defmethod acclimation:report-condition
    ((c class-name-must-be-non-nil-symbol)
     stream
     (language acclimation:english))
  (format stream
	  "A class name must be a non-nil symbol, but~@
           ~s was found."
	  (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c attempt-to-add-existing-subclass)
     stream
     (language acclimation:english))
  (format stream
          "Attempt to add existing subclass ~s as a subclass of ~s."
	   (subclass c)
           (superclass c)))
