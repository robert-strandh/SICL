(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun error (&rest arguments)
  (cl:error "ERROR was called with the following arguments: ~s" arguments))
 
