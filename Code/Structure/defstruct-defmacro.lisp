(cl:in-package #:sicl-structure)

(defmacro defstruct (&environment environment name-and-options &rest slots)
  (let ((description (parse-defstruct name-and-options slots)))
    (if (defstruct-type description)
        (expand-sequence-defstruct description environment)
        (expand-object-defstruct description environment))))
