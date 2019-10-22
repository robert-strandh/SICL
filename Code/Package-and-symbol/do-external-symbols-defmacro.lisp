(cl:in-package #:sicl-package)

;;; FIXME: check syntax better
(defmacro do-external-symbols ((symbol-variable
				&optional
				  (package-form '*package*)
				  (result-form 'nil))
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
	     (,package-var (package-designator-to-package ,package-form)))
	 (mapc ,function-name
	       (external-symbols ,package-var)))
       ,result-form)))
