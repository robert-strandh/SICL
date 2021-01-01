(cl:in-package #:cleavir-value-hoisting)

(defmethod acclimation:report-condition
    ((condition circular-dependencies-in-creation-form)
     stream
     (language acclimation:english))
  (with-accessors ((object object)
                   (creation-form creation-form))
      condition
    (format stream
            "The creation form ~S ~
             of the object ~S ~
             contains a circular dependency."
            creation-form object)))

(defmethod acclimation:report-condition
    ((condition object-not-externalizable)
     stream
     (language acclimation:english))
  (with-accessors ((object object)) condition
    (format stream
            "The object ~S is not externalizable."
            object)))
