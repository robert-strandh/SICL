(cl:in-package #:sicl-clos)

(defmethod find-method-combination
    ((generic-function standard-generic-function)
     method-combination-type-name
     method-combination-options)
  (sicl-method-combination:find-method-combination
   method-combination-type-name
   method-combination-options
   (sicl-environment:global-environment)))
