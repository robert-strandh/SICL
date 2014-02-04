(cl:in-package #:sicl-clos)

(defun slot-value (object slot-name)
  (declare (ignore object slot-name))
  (error "SLOT-VALUE has not been defined yet"))

(defun (setf slot-value) (new-value object slot-name)
  (declare (ignore new-value object slot-name))
  (error "(SETF SLOT-VALUE) has not been defined yet"))

