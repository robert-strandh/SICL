(cl:in-package #:sicl-minimal-extrinsic-environment)

;;;; This definition of IN-PACKAGE looks a bit strange.  The reason is
;;;; that when files are read by Eclector in order to be loaded, the
;;;; reader is executing in the host environment.  When an IN-PACKAGE
;;;; form is encountered, the host *PACKAGE* variable must be updated
;;;; so that subsequent forms are read in the right package.  But we
;;;; also want the variable *PACKAGE* in the extrinsic environment to
;;;; be updated.

(defun define-in-package (environment)
  (setf (sicl-genv:macro-function 'in-package environment)
	(lambda (form environment)
	  (declare (ignore environment))
	  (setq *package* (find-package (cadr form)))
	  `(setq *package* (find-package ',(cadr form))))))
