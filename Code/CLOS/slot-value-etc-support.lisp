(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MISSING.

(defun slot-missing-default
    (class object slot-name operation &optional new-value)
  (declare (ignore class operation new-value))
  (error "the slot named ~s is missing from the object ~s"
         slot-name object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-UNBOUND.

(defun slot-unbound-default (class object slot-name)
  (declare (ignore class))
  (error "the slot named ~s is unbound in the object ~s"
         slot-name object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE, (SETF SLOT-VALUE),
;;; SLOT-VALUE-USING-CLASS (SETF SLOT-VALUE-USING-CLASS)

(defun slot-value-using-class-default (class object slot)
  (let* ((location (slot-definition-location slot))
         (value
           (if (consp location)
               (car location)
               (standard-instance-access object location))))
    (if (eq value +unbound-slot-value+)
        (slot-unbound class object (slot-definition-name slot))
        value)))

(defun (setf slot-value-using-class-default) (new-value class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (setf (car location) new-value)
        (progn (cleavir-primop:nook-write object location new-value)
               new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-BOUNDP-USING-CLASS

(defun slot-boundp-using-class-default (class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (not (eq (car location) +unbound-slot-value+))
        (slot-boundp-using-index object location))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MAKUNBOUND, SLOT-MAKUNBOUND-USING-CLASS.

(defun slot-makunbound-using-class-default (class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (setf (car location) +unbound-slot-value+)
        (slot-makunbound-using-index object location)))
  nil)
