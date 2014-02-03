(cl:in-package #:sicl-clos)

(defun reader-method-class-default (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  *standard-reader-method*)

(defun writer-method-class-default (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  *standard-writer-method*)

