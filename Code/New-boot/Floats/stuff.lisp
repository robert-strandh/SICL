(cl:in-package #:sicl-arithmetic)

(setf (find-class 'single-float) nil)

(defclass single-float ()
  ((%bits :initarg :bits :reader bits)))

(setf (find-class 'double-float) nil)

(defclass double-float ()
  ((%bits :initarg :bits :reader bits)))

(defun bits-to-single-float (bits)
  (make-instance 'single-float :bits bits))

(defun bits-to-double-float (bits)
  (make-instance 'double-float :bits bits))

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
