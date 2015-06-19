(cl:in-package #:sicl-boot)

(defun define-defgeneric-c1 (boot)
  (setf (sicl-genv:macro-function 'defgeneric (c1 boot))
	(lambda (form environment)
	  (declare (ignore environment))
	  `(progn (sicl-genv:fmakunbound ',(car form) ,(r1 boot))
		  (ensure-generic-function
		   ',(car form)
		   :name ',(car form)
		   :lambda-list ',(cadr form))
		  (setf (sicl-genv:fdefinition ',(car form) ,(c1 boot))
			(sicl-genv:fdefinition ',(car form) ,(r1 boot)))))))

(defun customize-c1 (boot)
  (message "Customizing compilation environment C1~%")
  (define-defgeneric-c1 boot)
  (message "Finished customizing compilation environment C1~%"))
