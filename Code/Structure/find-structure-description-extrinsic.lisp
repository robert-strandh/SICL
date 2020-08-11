(cl:in-package #:sicl-structure)

(defvar *structure-descriptions* (make-hash-table :test #'eq))

(defun find-structure-description (name &optional (errorp t) environment)
  (declare (ignore environment))
  (let ((description (gethash name *structure-descriptions*)))
    (if (and (null description) errorp)
	(error "no such structure description ~S" name)
	description)))

(defun (setf find-structure-description) (new-value name &optional (errorp t) environment)
  (declare (ignore errorp environment))
  (setf (gethash name *structure-descriptions*) new-value))
