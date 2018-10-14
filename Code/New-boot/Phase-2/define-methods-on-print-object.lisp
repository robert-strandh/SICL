(cl:in-package #:sicl-clos)

(defmethod print-object ((object standard-generic-function) stream)
  (format stream "<standard generic function named ~s>"
          (generic-function-name object)))
