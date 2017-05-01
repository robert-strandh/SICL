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

(defmethod acclimation:report-condition
    ((c readers-must-be-proper-list)
     stream
     (language acclimation:english))
  (format stream
          "The keyword argument :READERS when supplied as~@
           initialization of a slot definition, must be~@
           a proper list, but the following was found instead:~@
           ~s."
          (readers c)))

(defmethod acclimation:report-condition ((c malformed-documentation-option)
			     stream
			     (language acclimation:english))
  (format stream
	  "A documentation option must have the form~@
           (:documentation <name>), but~@
           ~s was found."
	  (type-error-datum c)))
