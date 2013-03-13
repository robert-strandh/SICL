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

(defun make-direct-slot-definition (&rest args)
  (apply #'make-instance 'standard-direct-slot-definition args))

(defun make-effective-slot-definition (&rest args)
  (apply #'make-instance 'standard-effective-slot-definition args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  DIRECT-SLOT-DEFINITION-CLASS and EFFECTIVE-SLOT-DEFINITION-CLASS.

(defgeneric direct-slot-definition-class (class &rest initargs))

(defmethod direct-slot-definition-class
    ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition))

(defmethod direct-slot-definition-class
    ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition))

(defgeneric effective-slot-definition-class (class &rest initargs))

(defmethod effective-slot-definition-class
    ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition))

(defmethod effective-slot-definition-class
    ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  COMPUTE-EFFECTIVE-SLOT-DEFINITION.

(defgeneric compute-effective-slot-definition
    (class name direct-slot-definitions))

;;; Implement the behavior of compute-effective-slot-definition
;;; for standard-class and funcallable-standard-class.
(defun compute-effective-slot-definition-aux (class name direct-slot-definitions)
  (let (allocation initargs initform initfunction type)
    (setf allocation
	  (slot-definition-allocation (first direct-slot-definitions)))
    (setf initargs
	  (reduce #'union
		  (mapcar #'slot-definition-initargs direct-slot-definitions)))
    (let ((first-init (find-if-not #'null direct-slot-definitions
				   :key #'slot-definition-initfunction)))
      (unless (null first-init)
	(setf initform (slot-definition-initform first-init)
	      initfunction (slot-definition-initfunction first-init))))
    (setf type
	  `(and ,@(mapcar #'slot-definition-type direct-slot-definitions)))
    (let ((slot-definition-class (effective-slot-definition-class class)))
      (if (null initfunction)
	  (make-instance slot-definition-class
			 :name name
			 :allocation allocation
			 :initargs initargs
			 :type type)
	  (make-instance slot-definition-class
			 :name name
			 :allocation allocation
			 :initargs initargs
			 :initform initform
			 :initfunction initfunction
			 :type type)))))

(defmethod compute-effective-slot-definition
    ((class standard-class) name direct-slot-definitions)
  (compute-effective-slot-definition-aux
   (effective-slot-definition-class class) name direct-slot-definitions))

(defmethod compute-effective-slot-definition
    ((class funcallable-standard-class) name direct-slot-definitions)
  (compute-effective-slot-definition-aux
   (effective-slot-definition-class class) name direct-slot-definitions))

