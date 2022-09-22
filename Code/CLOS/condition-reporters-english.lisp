(cl:in-package #:sicl-clos)

(defmethod acclimation:report-condition
    ((c slot-definition-argument-must-be-supplied)
     stream
     (language acclimation:english))
  (format stream "The slot-definition argument must be supplied."))

(defmethod acclimation:report-condition
    ((c unable-to-compute-class-precedence-list)
     stream
     (language acclimation:english))
  (format stream
          "Unable to compute the class precedence list of the class ~s"
          (offending-class c)))

(defmethod acclimation:report-condition
    ((c option-or-method-must-be-non-empty-list)
     stream
     (language acclimation:english))
  (format stream
          "Option or method must be a non-empty list,~@
           but the following expression was found instead~@
           ~s"
          (expression c)))
