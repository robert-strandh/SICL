(cl:in-package #:cleavir-load-time-value-hoisting)

(defmethod acclimation:report-condition
    ((condition circular-dependencies-in-creation-form)
     stream
     (language acclimation:english))
  (with-accessors ((object object)
                   (creation-form creation-form))
      condition
    (format stream
            "The creation form ~A ~
             of the object ~A ~
             contains a circular dependency."
            creation-form object)))

(defmethod acclimation:report-condition
    ((condition circular-dependencies-in-load-time-value-form)
     stream
     (language acclimation:english))
  (with-accessors ((form form))
      condition
    (format stream
            "The load time value form ~A ~
             contains circular references to itself."
            form)))
