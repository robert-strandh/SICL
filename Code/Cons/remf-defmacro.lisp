(cl:in-package #:sicl-cons)

(defmacro remf (&environment env place indicator)
  (remf-expander env place indicator))
