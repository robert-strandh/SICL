(cl:in-package #:sicl-arithmetic)

(defclass simulated-float ()
  ((%pfloat :initarg :pfloat :reader pfloat)))

(setf (find-class 'single-float) nil)

(defclass single-float (simulated-float)
  ())

(defun make-single-float (pfloat)
  (make-instance 'single-float
    :pfloat pfloat))

(defmethod binary-equal ((x single-float) (y single-float))
  (sicl-primop:primop :single-float-equal x y))

(setf (find-class 'double-float) nil)

(defclass double-float (simulated-float)
  ())

(defun make-double-float (pfloat)
  (make-instance 'double-float
    :pfloat pfloat))

(defmethod binary-equal ((x double-float) (y double-float))
  (sicl-primop:primop :double-float-equal x y))

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
