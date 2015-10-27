(cl:in-package #:sicl-data-and-control-flow)

(defun fboundp (function-name)
  (sicl-global-environment:fboundp
   function-name
   (sicl-global-environment:global-environment)))
