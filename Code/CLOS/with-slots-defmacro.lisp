(cl:in-package #:sicl-clos)

(defmacro with-slots ((&rest slot-entries) instance-form &rest body)
  (with-slots-expander slot-entries instance-form body))
