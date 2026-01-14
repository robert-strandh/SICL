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
