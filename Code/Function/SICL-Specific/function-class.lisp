(cl:in-package #:sicl-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class STANDARD-FUNCTION.

;;; We need for funcallable standard objects and standard functions to
;;; be called the same way.  There is a slight difficulty in order for
;;; that to happen, though.  When a funcallable standard object is
;;; allocated, since it is a standard object, two additional cells are
;;; allocated in the rack, namely for the object class
;;; unique number and for the class slots of the class.  When a
;;; standard function is allocated, however, the cell containing the
;;; class slots of the class is not present because standard functions
;;; are not standard objects, so they can not become obsolete, and
;;; therefore do not need this information in order to be updated.
;;; But we still want the slots of the FUNCTION class to have the same
;;; location in instances of STANDARD-FUNCTION.  To accomplish that,
;;; we add a dummy slot to standard functions that is in the same
;;; location as the class slots of the class in standard objects.  To
;;; add this dummy slot, we define a class DUMMY-SLOT-PROVIDER,
;;; containing such a slot.

(defclass dummy-slot-supplier (t)
  ((%dummy :initform nil))
  (:metaclass built-in-class))

;;; It is important that the list of superclasses appear in the order
;;; that it does in this definition, because slots are allocated with
;;; locations that take this order into account.  In this case, the
;;; slot supplied by DUMMY-SLOT-SUPPLIER will occupy the first
;;; location in instances of STANDARD-FUNCTION.
(defclass standard-function (function dummy-slot-supplier)
  ()
  (:metaclass built-in-class))
