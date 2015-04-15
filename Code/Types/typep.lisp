(cl:in-package #:sicl-type)

(defgeneric typep (object type-specifier environment))

(defmethod typep (object type-specifier environment)
  (if (symbolp type-specifier)
      (typep-atomic object type-specifier environment)
      (typep-compound object type-specifier environment)))
