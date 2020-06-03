(cl:in-package #:sicl-type)

(defun typep (object type-specifier)
  (if (consp type-specifier)
      (typep-compound object type-specifier)
      (typep-atomic object type-specifier)))
