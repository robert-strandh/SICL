(cl:in-package #:sicl-clos)

;;; This :AROUND method on SHARED-INITIALIZE is meant to implement the
;;; generic-function initialization protocol as describe in the AMOP.
;;; We define a trivial version that trampolines to an ordinary
;;; function.  That way, during bootstrapping we can have the ordinary
;;; function evaluated by the host and we can define special-purpose
;;; versions of this :AROUND method.  Notice that the function
;;; CALL-NEXT-METHOD is passed as an additional argument to the
;;; ordinary function.

(defmethod shared-initialize :around
    ((generic-function generic-function)
     slot-names
     &rest initargs
     &key &allow-other-keys)
  (apply #'shared-initialize-around-generic-function-default
         #'call-next-method
         generic-function
         slot-names
         initargs))
