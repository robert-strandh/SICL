(cl:in-package #:sicl-structure)

(defmacro defstruct (&environment environment name-and-options &rest slots)
  (expand-defstruct (parse-defstruct name-and-options slots) environment))
