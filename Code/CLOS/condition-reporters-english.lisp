(cl:in-package #:sicl-clos)

(defmethod acclimation:report-condition
    ((c option-or-method-must-be-non-empty-list)
     stream
     (language acclimation:english))
  (format stream
          "Option or method must be a non-empty list,~@
           but the following expression was found instead~@
           ~s"
          (expression c)))
