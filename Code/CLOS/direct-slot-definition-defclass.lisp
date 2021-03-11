(cl:in-package #:sicl-clos)

;;; The READERS and WRITERS slots exist only in direct slot
;;; definitions, because they are not combined the way other slot
;;; properties are when an effective slot definition is computed.
(defclass direct-slot-definition (slot-definition)
  ((%readers 
    :initform '()
    :initarg :readers 
    :reader slot-definition-readers)
   (%writers 
    :initform '()
    :initarg :writers 
    :reader slot-definition-writers)
   ;; The CAR of the CONS cell stored in this slot is used to hold
   ;; values when :ALLOCATION is :CLASS.  In this case, the CONS cell
   ;; becomes the location of the effective slot definition.
   (%storage
    :initform (list nil)
    :reader slot-definition-storage)))
