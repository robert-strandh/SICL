(cl:in-package #:sicl-clos)

;;; This :AROUND method on SHARED-INITIALIZE is meant to implement the
;;; class-initialization protocol as described in the AMOP.  We define
;;; a trivial version that trampolines to an ordinary function.  That
;;; way, during bootstrapping we can have the ordinary function
;;; executed by the host and we can define special-purpose versions
;;; of this :AROUND method.  Notice that the function CALL-NEXT-METHOD
;;; is passed as an additional argument to the ordinary function.

(defmethod shared-initialize :around
    ((class real-class)
     slot-names
     &rest initargs
     &key
     &allow-other-keys)
  (apply #'shared-initialize-around-real-class-default
         #'call-next-method
         class
         slot-names
         initargs))

(defmethod shared-initialize :after
    ((class built-in-class)
     slot-names
     &rest initargs
     &key
     &allow-other-keys)
  (shared-initialize-after-built-in-class-default class))

(defmethod initialize-instance :around
    ((class real-class) &rest initargs &key &allow-other-keys)
  (apply #'initialize-instance-around-real-class-default
         #'call-next-method
         class
         initargs))
