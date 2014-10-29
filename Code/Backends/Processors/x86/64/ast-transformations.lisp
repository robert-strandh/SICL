(cl:in-package #:sicl-x86-64)

(defmethod sicl-ast-transformations:introduce-immediates
  (ast (backend backend-x86-64))
  (labels ((aux (ast)
	     (when (typep ast 'sicl-ast:constant-ast)
	       (let ((value (sicl-ast:value ast)))
		 (cond ((and (typep value 'integer)
			     (<= (- (expt 2 29))
				 value
				 (1- (expt 2 29))))
			(change-class ast 'sicl-ast:immediate-ast
				      :value (* 4 value)))
		       ((and (typep value 'character)
			     (<= (- (expt 2 29))
				 (char-code value)
				 (1- (expt 2 29))))
			(change-class ast 'sicl-ast:immediate-ast
				      :value (+ 2 (* 4 (char-code value)))))
		       (t
			nil))))
	     (mapc #'aux (sicl-ast:children ast))))
    (aux ast)))

