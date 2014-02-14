(cl:in-package #:sicl-clos)

;;; In this version of COMPUTE-DISCRIMINATING-FUNCTION-DEFAULT, we DO
;;; use the compiler.  For a version that does not use the compiler,
;;; see the file compute-discriminating-function-support-b.lisp.

;;; This function takes a generic function an returns a discriminating
;;; function for it that has the GENERIC-FUNCTION argument compiled in
;;; as a constant, so that the discriminating function can pass the
;;; generic function to the default discriminating function.
(defun make-default-discriminating-function (generic-function)
  (compile nil
	   `(lambda (&rest arguments)
	      (default-discriminating-function ,generic-function arguments))))

(defun compute-discriminating-function-default (generic-function)
  (make-default-discriminating-function generic-function))
