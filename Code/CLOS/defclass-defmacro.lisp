(cl:in-package #:sicl-clos)

(defmacro defclass (name
                    superclass-names
                    slot-specifiers
                    &rest options)
  (defclass-expander name superclass-names slot-specifiers options))
