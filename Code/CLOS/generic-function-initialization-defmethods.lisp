(cl:in-package #:sicl-clos)

;;; This :AROUND method on SHARED-INITIALIZE is meant to implement the
;;; generic-function initialization protocol as described in the AMOP.
;;; We define a trivial version that trampolines to an ordinary
;;; function.  That way, during bootstrapping we can have the ordinary
;;; function executed by the host and we can define special-purpose
;;; versions of this :AROUND method.  Notice that the function
;;; CALL-NEXT-METHOD is passed as an additional argument to the
;;; ordinary function.  This is also the case of the function
;;; INVALIDATE-DISCRIMINATING-FUNCTION.  This way, the ordinary
;;; function does not have to refer to a globally defined function.

(defmethod shared-initialize :around
    ((generic-function generic-function)
     slot-names
     &rest initargs
     &key &allow-other-keys)
  (apply #'shared-initialize-around-generic-function-default
         #'call-next-method
         #'invalidate-discriminating-function
         generic-function
         slot-names
         initargs))

(defmethod initialize-instance :before
    ((object generic-function)
     &key
       (method-combination nil method-combination-p)
     &allow-other-keys)
  (unless method-combination-p
    (error 'method-combination-keyword-argument-must-be-given)))
