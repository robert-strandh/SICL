(cl:in-package #:sicl-data-and-control-flow)

(defun fmakunbound (function-name)
  (sicl-global-environment:fmakunbound
   function-name
   (sicl-global-environment:global-environment)))
