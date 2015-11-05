(cl:in-package #:sicl-data-and-control-flow)

(defun fdefinition (function-name)
  (sicl-global-environment:fdefinition
   function-name
   (load-time-value
    (sicl-global-environment:global-environment))))
