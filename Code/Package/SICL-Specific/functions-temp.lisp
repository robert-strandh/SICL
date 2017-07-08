(cl:in-package #:sicl-package)
;;; FIXME: signal correctable errors.
;;; FIXME: check that nicknames is a proper list of string designators
;;; FIXME: check that use is a proper list of package designators.
;;(defun make-package (name &key nicknames use)
;;  (declare (type string-designator name))
;;  (assert (null (find-package name)))
;;  (loop for rest = nicknames then (cdr rest)
;;	while (consp rest)
;;	do (assert (null (find-package (car rest)))))
;;  (let ((package (make-instance 'package)))
;;    (setf (name package) (string name))
;;    (setf (nicknames package (copy-list nicknames)))
;;    (use-package use package)
;;    package))
