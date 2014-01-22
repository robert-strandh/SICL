(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function METHOD-COMBINATION-COMPUTE-EFFECTIVE-METHOD.
;;;
;;; This function is not specified in the AMOP.  The AMOP stipulates
;;; that there is a single specified method on the generic function
;;; COMPUTE-EFFECTIVE-METHOD specialized only for a generic function
;;; of STANDARD-GENERIC-FUNCTION.  There is no specialization on the
;;; second argument, which is METHOD-COMBINATION.  For that reason, we
;;; can not add methods on COMPUTE-EFFECTIVE-METHOD for each method
;;; combination in the Common Lisp HyperSpec.  Instead, the only
;;; specified method on COMPUTE-EFFECTIVE-METHOD calls this generic
;;; function, which will dispatch on the class of the method
;;; combination.
;;;
;;; The macro DEFINE-METHOD-COMBINATION will use some unspecified
;;; mechanism to add methods to this generic function. 
;;;
;;; Furthermore, there is a potential metastability problem in
;;; COMPUTE-EFFECTIVE-METHOD, namely when the first argument is the
;;; generic function named COMPUTE-EFFECTIVE-METHOD, which is a
;;; standard generic function using the standard method combination.
;;; We avoid the metastability problem by making the discriminating
;;; function of COMPUTE-EFFECTIVE-METHOD recognize the special case
;;; when the class of the first argument is STANDARD-GENERIC-FUNCTION
;;; and the class of the second argument is
;;; METHOD-COMBINATION-STANDARD and by having it call a default
;;; function in that case.  Thus, this generic function will never be
;;; called in that case.

(defgeneric method-combination-compute-effective-method
    (method-combination methods))
