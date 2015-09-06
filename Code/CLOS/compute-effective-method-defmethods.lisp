(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-EFFECTIVE-METHOD.
;;;
;;; There is a potential metastability problem in this generic
;;; function namely when the first argument is the generic function
;;; named COMPUTE-EFFECTIVE-METHOD, which is a standard generic
;;; function using the standard method combination.  We avoid the
;;; metastability problem by making the discriminating function of
;;; this generic function recognize the special case when the class of
;;; the first argument is STANDARD-GENERIC-FUNCTION and the class of
;;; the second argument is METHOD-COMBINATION-STANDARD and by having
;;; it call a default function in that case.  Thus, this generic
;;; function will never be called in that case.
;;;
;;; There is no trace in this file of the special case mentioned
;;; above, because it is handled by having the generic function
;;; COMPUTE-DISCRIMINATING-FUNCTION recognize it and compute a
;;; discriminating function that tests for the special case and calls
;;; COMPUTE-EFFECTIVE-METHOD-DEFAULT to handle it. 

(defmethod compute-effective-method
    ((generic-function standard-generic-function) method-combination methods)
  (method-combination-compute-effective-method method-combination
					       methods
					       generic-function))

