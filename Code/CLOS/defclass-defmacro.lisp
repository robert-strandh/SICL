(cl:in-package #:sicl-clos)

(defmacro defclass (&environment environment
                      name
                      superclass-names
                      slot-specifiers
                    &rest options)
  (defclass-expander name superclass-names slot-specifiers options environment))
