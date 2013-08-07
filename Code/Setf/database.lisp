(in-package #:sicl-setf)

(defparameter *setf-expanders* '())

(defun find-setf-expander (name)
  (cdr (assoc name *setf-expanders*)))

(defun (setf find-setf-expander) (new-expander name)
  (let ((existing-entry (assoc name *setf-expanders*)))
    (if (null existing-entry)
	(push (cons name new-expander) *setf-expanders*)
	(setf (cdr existing-entry) new-expander))))
