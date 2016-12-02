(defmacro and (&rest arguments)
  (cond ((null arguments)
	 t)
	((null (cdr arguments))
	 (car arguments))
	(t
	 `(when ,(car arguments)
	    (and ,@(cdr arguments))))))
