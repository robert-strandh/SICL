(cl:in-package #:sicl-types)

(defmethod typep (object (type-specifier cons) &optional environment)
  (typep-compound
   object
   (first type-specifier)
   (rest type-specifier)
   (if (null environment)
       (sicl-genv:global-environment)
       environment)))

(defmethod typep (object type-specifier &optional environment)
  (typep-atomic
   object
   type-specifier
   (if (null environment)
       (sicl-genv:global-environment)
       environment)))
