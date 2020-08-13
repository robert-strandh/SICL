(cl:in-package #:sicl-structure)

(defun expand-defstruct (description environment)
  (if (defstruct-type description)
      (expand-sequence-defstruct description environment)
      (expand-object-defstruct description environment)))
