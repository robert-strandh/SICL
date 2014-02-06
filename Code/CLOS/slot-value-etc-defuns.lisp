(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MISSING.

(defun slot-missing
    (class object slot-name operation &optional new-value)
  (slot-missing-default class object slot-name operation new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-UNBOUND.

(defun slot-unbound (class object slot-name)
  (slot-unbound-default class object slot-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE, (SETF SLOT-VALUE), 
;;; SLOT-VALUE-USING-CLASS (SETF SLOT-VALUE-USING-CLASS)

(defun slot-value-using-class (class object slot)
  (slot-value-using-class-default class object slot))

(defun (setf slot-value-using-class) (new-value class object slot)
  (setf (slot-value-using-class-default class object slot) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-BOUNDP SLOT-BOUNDP-USING-CLASS

(defun slot-boundp-using-class (class object slot)
  (slot-boundp-using-class-default class object slot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MAKUNBOUND, SLOT-MAKUNBOUND-USING-CLASS.

(defun slot-makunbound-using-class (class object slot)
  (slot-makunbound-using-class-default class object slot))
