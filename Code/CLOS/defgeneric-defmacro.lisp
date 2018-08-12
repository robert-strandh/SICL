(cl:in-package #:sicl-clos)

(defmacro defgeneric (&environment env
                        name lambda-list
                      &rest options-and-methods)
  (defgeneric-expander env name lambda-list options-and-methods))
