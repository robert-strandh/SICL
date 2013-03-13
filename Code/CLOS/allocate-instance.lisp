(in-package #'sicl-clos-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We follow the AMOP and define functions to allocate
;;; class instances and slot storage.  For now, these
;;; functions create other Common Lisp objects such as 
;;; structures and arrays.  In a real implementation, 
;;; these functions might use lower-level mechanisms
;;; to allocate these entities.

(defparameter *secret-unbound-value* (list "unbound"))

(defstruct standard-instance class slots)

(defun allocate-standard-instance (class slot-storage)
  (make-standard-instance :class class
			  :slots slot-storage))

(defun allocate-slot-storage (size initial-value)
  (make-array size :initial-element initial-value))

(defun slot-contents (slot-storage location)
  (aref slot-storage location))

(defun (setf slot-contents) (new-value slot-storage location)
  (setf (aref slot-storage location) new-value))

(defgeneric allocate-instance class &rest initargs)

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((number-of-slots (count :instance 
				(class-slots class)
				:test #'eq :key #'slot-definition-allocation)))
    (allocate-standard-instance
     class
     (make-array number-of-slots :initial-element *secret-unbound-value*))))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((number-of-slots (count :instance 
				(class-slots class)
				:test #'eq :key #'slot-definition-allocation)))
    (allocate-standard-instance
     class
     (make-array number-of-slots :initial-element *secret-unbound-value*))))

(defun standard-instance-access (instance location)
  (slot-contents (slots instance) location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (slot-contents (slots instance) location) new-value))

