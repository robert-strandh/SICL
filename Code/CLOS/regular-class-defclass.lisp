(cl:in-package #:sicl-clos)
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class REGULAR-CLASS.
;;;
;;; A common superclass of STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.
;;;
;;; This class is not specified by the AMOP, but we are allowed to
;;; define it.  See:
;;; http://metamodular.com/CLOS-MOP/restrictions-on-implementations.html

;;; This function is used by the class finalization protocol to set
;;; the flag in the class that indicates that it is finalized.
(defgeneric (setf class-finalized-p) (new-value class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-prototype.html
(defgeneric class-prototype (class))

;;; This function sets the class prototype of the class.
(defgeneric (setf class-prototype) (prototype class))

(defclass regular-class (real-class)
  ((%direct-slots
    :initarg :direct-slots
    :reader class-direct-slots)
   (%finalized-p
    :initform nil
    :accessor class-finalized-p)
   (%default-initargs
    :accessor class-default-initargs)
   (%effective-slots
    :initform '()
    :accessor class-slots)
   (%prototype
    :accessor class-prototype)
   (%dependents
    :initform '()
    :accessor dependents)))
