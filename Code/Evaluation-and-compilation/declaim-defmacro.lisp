(cl:in-package #:sicl-evaluation-and-compilation)

(defmacro declaim (&rest declaration-specifiers)
  (declaim-expander declaration-specifiers))
