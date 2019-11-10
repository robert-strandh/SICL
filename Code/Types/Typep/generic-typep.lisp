(cl:in-package #:sicl-type)

;;; We try not to allocate any memory in TYPEP.  To avoid it as much
;;; as possible, we do not start by expanding the type desriptor.

(defmethod generic-typep (object type-specifier environment)
  (if (symbolp type-specifier)
      (typep-atomic object type-specifier environment)
      (typep-compound object type-specifier environment)))
