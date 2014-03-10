(cl:in-package #:sicl-compiler-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CONSTANTP.

(defun constantp (form &optional environment)
  (declare (ignore environment))
  (or (and (not (symbolp form))
	   (not (consp form)))
      (keywordp form)
      (not (null (find form (constant-variables *global-environment*)
		       :key #'name :test #'eq)))
      (and (consp form)
	   (eq (car form) 'quote))))

