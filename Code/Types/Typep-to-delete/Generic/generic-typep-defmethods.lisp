(cl:in-package #:sicl-type)

(defmethod generic-typep (object (type-specifier cons))
  (typep-compound object (first type-specifier) (rest type-specifier)))

(defmethod generic-typep (object type-specifier)
  (typep-atomic object type-specifier))
