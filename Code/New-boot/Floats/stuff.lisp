(cl:in-package #:sicl-arithmetic)

(defclass simulated-float ()
  ((%sign :initarg :sign :reader sign)
   (%floatr :initarg :floatr :reader floatr)))

(setf (find-class 'single-float) nil)

(defclass single-float (simulated-float)
  ())

(defun make-single-float (sign floatr)
  (make-instance 'single-float
    :sign sign :floatr floatr))

(setf (find-class 'double-float) nil)

(defclass double-float (simulated-float)
  ())

(defun make-double-float (sign floatr)
  (make-instance 'double-float
    :sign sign :floatr floatr))

(defun coerce (value type)
  (assert (zerop value))
  (ecase type
    (single-float (bits-to-single-float value))
    (double-float (bits-to-double-float value))))

(defgeneric float-disgits (float))

(defmethod float-digits ((float single-float))
  (declare (ignore float))
  24)

(defmethod float-digits ((float double-float))
  (declare (ignore float))
  53)
