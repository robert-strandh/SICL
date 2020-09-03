(cl:in-package #:sicl-clos)

(defmacro defgeneric (&environment environment
                        name lambda-list
                      &rest options-and-methods)
  (defgeneric-expander name lambda-list options-and-methods environment))
