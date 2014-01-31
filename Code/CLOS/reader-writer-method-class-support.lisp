(cl:in-package #:sicl-clos)

(defun reader-method-class-default (class direct-slot &rest initargs)
  (declare (ignore initargs))
  *standard-reader-method*)

(defun writer-method-class-default (class direct-slot &rest initargs)
  (declare (ignore initargs))
  *standard-writer-method*)

