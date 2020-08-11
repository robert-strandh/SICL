(cl:in-package #:sicl-structure)

(defun find-structure-description (name &optional (errorp t) environment)
  (let* ((global-environment (sicl-genv:global-environment environment))
         (description (sicl-genv:structure-description name global-environment)))
    (if (and (null description) errorp)
	(error "no such structure description ~S" name)
	description)))

(defun (setf find-structure-description) (new-value name &optional (errorp t) environment)
  (declare (ignore errorp))
  (let ((global-environment (sicl-genv:global-environment environment)))
    (setf (sicl-genv:structure-description name global-environment) new-value)))
