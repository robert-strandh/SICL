(in-package #:sicl-clos)

(defclass slot-definition (metaobject)
  ((%name 
    :initarg :name 
    :reader slot-definition-name)
   (%allocation 
    :initarg :allocation
    :initform :instance
    :reader slot-definition-allocation)
   (%type 
    :initarg :type
    :initform t
    :reader slot-definition-type)
   (%initargs 
    :initform '()
    :initarg :initargs 
    :reader slot-definition-initargs)
   (%initform 
    :initarg :initform 
    :reader slot-definition-initform)
   (%initfunction 
    :initform nil
    :initarg :initfunction
    :accessor slot-definition-initfunction)))

;;; The READERS and WRITERS slots only exist in direct slot
;;; definitions, because they are not combined the way other slot
;;; properties are when an effective slot definition is computer.
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

(defclass effective-slot-definition (slot-definition)
  ((%location 
    :initform nil
    :initarg :location 
    :accessor slot-definition-location)))

(defclass standard-slot-definition (slot-definition)
  ())

(defclass standard-direct-slot-definition
    (standard-slot-definition direct-slot-definition)
  ())

(defclass standard-effective-slot-definition
    (standard-slot-definition effective-slot-definition)
  ())

;; (defun make-direct-slot-definition (&rest args)
;;   (apply #'make-instance 'standard-direct-slot-definition args))

;; (defun make-effective-slot-definition (&rest args)
;;   (apply #'make-instance 'standard-effective-slot-definition args))

