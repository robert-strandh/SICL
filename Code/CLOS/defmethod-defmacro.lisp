(cl:in-package #:sicl-clos)

(defmacro defmethod (&environment ct-env function-name &rest rest)
  (defmethod-expander ct-env function-name rest))
