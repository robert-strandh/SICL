(cl:in-package #:sicl-boot)

(defun define-defgeneric-c1 (boot)
  (setf (sicl-genv:macro-function 'defgeneric (c1 boot))
	(lambda (form environment)
	  (declare (ignore environment))
	  `(progn (sicl-genv:fmakunbound ',(second form) ,(r1 boot))
		  (ensure-generic-function
		   ',(second form)
		   :name ',(second form)
		   :lambda-list ',(third form))
		  (setf (sicl-genv:fdefinition ',(second form) ,(c1 boot))
			(sicl-genv:fdefinition ',(second form) ,(r1 boot)))))))

(defun customize-c1 (boot)
  (message "Customizing compilation environment C1~%")
  (define-defgeneric-c1 boot)
  (message "Finished customizing compilation environment C1~%"))
