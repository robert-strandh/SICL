(in-package #:sicl-setf)

(defmacro define-setf-expander (name lambda-list &body body)
  `(let ((expander ,(cleavir-code-utilities:parse-macro name lambda-list body)))
     (setf (find-setf-expander ',name) expander)))

(defun get-setf-expansion (place &optional environment)
  (cond ((symbolp place)
	 (multiple-value-bind (expansion expanded-p)
	     (macroexpand-1 place environment)
	   (if expanded-p
	       (get-setf-expansion expansion environment)
	       (let ((temp (gensym)))
		 (values nil nil `(,temp) `(setq ,place ,temp) `,place)))))
	((consp place)
	 (let ((expander (find-setf-expander (car place))))
	   (if (null expander)
	       (multiple-value-bind (expansion expanded-p)
		   (macroexpand-1 place environment)
		 (if expanded-p
		     (get-setf-expansion expansion environment)
		     (let* ((vals (cdr place))
			    (vars (loop for val in vals collect (gensym)))
			    (store-var (gensym))
			    (writer-form `(funcall #'(setf ,(car place))
						   ,store-var
						   ,@vars))
			    (reader-form `(,(car place) ,@vars)))
		       (values vars
			       vals
			       (list store-var)
			       writer-form
			       reader-form))))
	       (funcall expander place environment))))
	(t
	 (error "a place must be a symbol or a compound form"))))

;;; FIXME: handle the long form
(defmacro defsetf (access-fun update-fun-or-lambda-list &rest rest)
  (cond ((symbolp update-fun-or-lambda-list)
	 (assert (or (null rest) (stringp (car rest))))
	 `(define-setf-expander ,access-fun (&rest args)
	    (let* ((vars (loop for arg in args collect (gensym)))
		   (store-var (gensym))
		   (writer-form `(,',update-fun-or-lambda-list
				 ,@vars ,store-var))
		   (reader-form `(,',access-fun ,@vars)))
	      (values vars
		      args
		      (list store-var)
		      writer-form
		      reader-form))))
	(t (error "can't handle the long form of defsetf yet"))))

	  
  
