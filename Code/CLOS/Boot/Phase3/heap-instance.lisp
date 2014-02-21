(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For now, these functions create other Common Lisp objects such as
;;; structures and arrays.  In a real implementation, these functions
;;; might use lower-level mechanisms to allocate these entities.

(defparameter *unbound-value* (list 999999))

(defstruct heap-instance class slots)

(defun allocate-heap-instance (class slot-storage)
  (make-heap-instance :class class :slots slot-storage))

(defun allocate-slot-storage (size &optional (initial-value *unbound-value*))
  (make-array size :initial-element initial-value))

(defun slot-contents (slot-storage location)
  (aref slot-storage location))

(defun (setf slot-contents) (new-value slot-storage location)
  (setf (aref slot-storage location) new-value))

