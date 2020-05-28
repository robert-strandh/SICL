(cl:in-package #:sicl-clos)

(defmacro defgeneric (name lambda-list
                      &rest options-and-methods)
  (defgeneric-expander name lambda-list options-and-methods))
