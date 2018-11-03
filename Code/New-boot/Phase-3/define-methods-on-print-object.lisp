(cl:in-package #:sicl-clos)

(defgeneric print-object (object stream))

(defmethod print-object (object stream)
  (format stream "<Unknown Ersatz object>"))

(defmethod print-object ((object standard-class) stream)
  (format stream "<E3 Ersatz standard class ~s>"
          (class-name object)))

(defmethod print-object ((object funcallable-standard-class) stream)
  (format stream "<E3 Ersatz funcallable standard class ~s>"
          (class-name object)))

(defmethod print-object ((object built-in-class) stream)
  (format stream "<E3 Ersatz funcallable standard class ~s>"
          (class-name object)))

(defmethod print-object ((object standard-generic-function) stream)
  (format stream "<E4 Ersatz standard generic function ~s>"
          (generic-function-name object)))

(defmethod print-object ((object standard-method) stream)
  (format stream "<E4 Ersatz standard method>"))

(defmethod print-object ((object standard-reader-method) stream)
  (format stream "<E4 Ersatz standard reader method>"))

(defmethod print-object ((object standard-writer-method) stream)
  (format stream "<E4 Ersatz standard writer method>"))

(defmethod print-object ((object method-combination) stream)
  (format stream "<E4 Ersatz method combination>"))

(defmethod print-object
    ((object sicl-method-combination:method-combination-template) stream)
  (format stream "<E4 Ersatz method combination template>"))

