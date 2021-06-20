(cl:in-package #:fast-generic-functions)

(defun sealable-metaobjects:domain-specializers (sealed-domain)
  (declare (ignore sealed-domain))
  '())

(setf (find-class 'fast-generic-function)
      (find-class 'standard-generic-function))

(defgeneric no-primary-method (sequence-function &rest arguments))

(defun sealed-domains (sequence-function)
  (declare (ignore sequence-function))
  '())

(defun seal-domain (sequence-function stuff)
  (declare (ignore sequence-function stuff))
  nil)
