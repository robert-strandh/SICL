(cl:in-package #:sicl-boot)

(defun customize-r1 (boot)
  (let ((c (c1 boot))
	(r (r1 boot)))
    (message "Customizing run-time environment R2~%")
    (message "Finished customizing run-time environment R2~%")))
