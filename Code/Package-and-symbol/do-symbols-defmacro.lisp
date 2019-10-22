(cl:in-package #:sicl-package)

;;; FIXME: check syntax better
(defmacro do-symbols ((symbol-variable
		       &optional
			 (package-form '*package*)
			 (result-form 'NIL))
		      &body body)
  (let ((function-name (gensym))
	(package-var (gensym))
	(remaining-body body)
	(declarations '()))
    (loop while (eq (caar remaining-body) 'declare)
	  do (push (pop remaining-body) declarations))
    `(block nil
       (let ((,function-name (lambda (,symbol-variable)
			       (locally ,@(reverse declarations)
				 (tagbody
				    ,@remaining-body))))
	     (,package-var ,package-form))
	 (mapc ,function-name
	       (external-symbols ,package-var))
	 (mapc ,function-name
	       (internal-symbols ,package-var))
	 (mapc (lambda (used-package)
		 (mapc
		  (lambda (symbol)
		    (unless (symbol-member
			     symbol
			     (shadowing-symbols ,package-var))
		      (mapc ,function-name symbol)))
		  (external-symbols used-package)))
	       (use-list ,package-var)))
       ,result-form)))
