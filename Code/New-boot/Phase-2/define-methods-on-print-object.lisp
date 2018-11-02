(cl:in-package #:sicl-clos)

(defmethod print-object ((object standard-generic-function) stream)
  (format stream "<E3 Bridge standard-generic-function ~s>"
          (generic-function-name object)))

(defmethod print-object ((object standard-method) stream)
  (format stream "<E3 Bridge standard-method>"))

(defmethod print-object ((object standard-class) stream)
  (format stream "<E2 Bridge standard-class ~s>"
          (class-name object)))

(defmethod print-object ((object funcallable-standard-class) stream)
  (format stream "<E2 Bridge funcallable-standard-class ~s>"
          (class-name object)))

(defmethod print-object ((object built-in-class) stream)
  (format stream "<E2 Bridge built-in-class ~s>"
          (class-name object)))

(defmethod print-object ((object standard-direct-slot-definition) stream)
  (format stream "<E2 Bridge standard-direct-slot-definition ~s>"
          (slot-definition-name object)))

(defmethod print-object ((object standard-effective-slot-definition) stream)
  (format stream "<E2 Bridge standard-effective-slot-definition ~s>"
          (slot-definition-name object)))

(defmethod print-object ((object method-combination) stream)
  (format stream "<E3 Bridge method-combination>"))
