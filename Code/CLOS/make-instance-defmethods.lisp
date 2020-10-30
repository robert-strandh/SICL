(cl:in-package #:sicl-clos)

(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

;;; The HyperSpec only recognizes methods on MAKE-INSTANCE specialized
;;; to SYMBOL and STANDARD-CLASS.  However, the AMOP clearly says
;;; that ENSURE-GENERIC-FUNCTION calls MAKE-INSTANCE, and the default
;;; class that is instantiated is STANDARD-GENERIC-FUNCTION.  But
;;; STANDARD-GENERIC-FUNCTION is not an instance of STANDARD-CLASS,
;;; and instead of FUNCALLABLE-STANDARD-CLASS.
;;;
;;; We have two possible ways of solving this conflict.  Way number 1
;;; is to specialize the method on MAKE-INSTANCE to some superclass
;;; common to STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.  Way
;;; number 2 would be for ENSURE-GENERIC-FUNCTION to call some other
;;; function to instantiate the generic function class, in violation
;;; of the AMOP.
;;;
;;; We choose way number 1 and specialize the method to REGULAR-CLASS,
;;; which is the most special superclass of STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.  SBCL uses the same solution, except
;;; that the method is specialized to CLASS.  So for instance, it is
;;; possible to call (MAKE-INSTANCE 'SYMBOL) in SBCL.  It does not
;;; complain in MAKE-INSTANCE, but in ALLOCATE-INSTANCE.
(defmethod make-instance ((class regular-class) &rest initargs)
  (apply #'make-instance-default class initargs))
