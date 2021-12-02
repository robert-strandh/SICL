(cl:in-package #:cleavir-literals)

(defmethod acclimation:report-condition
    ((condition object-not-externalizable)
     stream
     (language acclimation:english))
  (with-accessors ((object object)) condition
    (format stream
            "The object ~S is not externalizable."
            object)))
