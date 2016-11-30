(defun car (object)
  (cond ((consp object)
	 (memref (u- object (word 1))))
	((null object)
	 nil)
	(t
	 (error ...))))
