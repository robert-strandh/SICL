(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We need a function to determine whether some object is a class.
;;; Since classes can be unnamed, we cannot consult our database of
;;; classes.  Instead, we decide that something is a class if it is 
;;; a subclass of the class CLASS.

(defmethod classp (object)
  nil)

(defmethod classp ((object class))
  t)
