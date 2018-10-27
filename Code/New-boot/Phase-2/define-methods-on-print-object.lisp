(cl:in-package #:sicl-clos)

(defmethod print-object ((object standard-generic-function) stream)
  (format stream "<E3 Bridge generic function ~s>"
          (generic-function-name object)))

(defmethod print-object ((object standard-class) stream)
  (format stream "<E2 Bridge standard-class ~s>"
          (class-name object)))

(defmethod print-object ((object funcallable-standard-class) stream)
  (format stream "<E2 Bridge funcallable-standard-class ~s>"
          (class-name object)))
