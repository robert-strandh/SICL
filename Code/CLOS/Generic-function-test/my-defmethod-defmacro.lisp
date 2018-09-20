(cl:in-package #:sicl-clos)

(defmacro my-defmethod (function-name &rest rest)
  (defmethod-expander function-name rest))
