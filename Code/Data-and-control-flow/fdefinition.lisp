(cl:in-package #:sicl-data-and-control-flow)

(defun fdefinition (function-name)
  (sicl-global-environment:fdefinition
   function-name
   sicl-global-environment:*global-environment*))
