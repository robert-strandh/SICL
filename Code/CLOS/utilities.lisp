(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility functions

;;; FIXME: check for circular lists
(defun proper-list-p (object)
  (or (null object)
      (and (consp object)
	   (null (cdr (last object))))))


