(cl:in-package #:sicl-structure)

(defun expand-defstruct (description environment)
  (if (defstruct-type description)
      (expand-typed-defstruct description environment)
      (expand-object-defstruct description environment)))
