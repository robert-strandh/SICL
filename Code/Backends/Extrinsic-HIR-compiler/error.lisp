(cl:in-package #:sicl-extrinsic-hir-compiler)

;;; This function will be entered under the name CL:ERROR in the
;;; global environment.  It will be entered early on, because many
;;; functions that will subsequently be entered into the environment
;;; will refer to it.  Later, when the environment is more complete,
;;; we replace this function with one that invokes the condition
;;; system, calls MAKE-INSTANCE, FORMAT and many other things.
(defun error (&rest arguments)
  (cl:error "ERROR was called with the following arguments: ~s" arguments))
