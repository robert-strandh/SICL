(defgeneric build-constant (constant))

(defmethod build-constant ((constant string))
  `(let ((string (funcall make-string ,(length constant))))
     ,@(loop for i from 0
	     for char across constant
	     collect `(funcall setf-schar ,char string ,i))
     string))

(defmethod build-constant ((constant symbol))
  (let* ((package-name (package-name (symbol-package constant)))
	 (symbol-name (symbol-name constant)))
    `(funcall intern
	      ,(build-constant symbol-name)
	      ,(build-constant package-name))))

(defmethod build-constant ((constant cons))
  `(let* ((cons-symbol ,(build-constant 'cons))
	  (cons-function (funcall fdefinition cons-symbol)))
     (funcall cons-function
	      ,(build-constant (car constant))
	      ,(build-constant (cdr constant)))))
	      
