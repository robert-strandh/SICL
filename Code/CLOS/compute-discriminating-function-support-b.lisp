(cl:in-package #:sicl-clos)

;;; In this version of COMPUTE-DISCRIMINATING-FUNCTION-DEFAULT, we do
;;; not use the compiler.  For a version that DOES use the compiler,
;;; see the file compute-discriminating-function-support-a.lisp.

;;; This function takes a generic function an returns a discriminating
;;; function for it that closes over the GENERIC-FUNCTION argument, so
;;; that the discriminating function can pass the generic function to
;;; the default discriminating function.
(defun make-default-discriminating-function (generic-function)
  (lambda (&rest arguments)
    (default-discriminating-function generic-function
                                     arguments
                                     (specializer-profile generic-function))))

(defun compute-discriminating-function-default (generic-function)
  (make-default-discriminating-function generic-function))
