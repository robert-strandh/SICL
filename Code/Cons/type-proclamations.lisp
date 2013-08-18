(cl:in-package #:sicl-cons)

(proclaim '(ftype (or
		   (function (null) null)
		   (function (cons) t))
	    car cdr))

(proclaim '(ftype (or
		   (function ((or null (cons * null))) null)
		   (function ((cons * cons)) t))
	    cadr cddr))

(proclaim '(ftype (or
		   (function ((or null (cons null *))) null)
		   (function ((cons cons *)) t))
	    caar cdar))

(proclaim '(ftype (or
		   (function ((or null (cons * (or null (cons * null)))) null))
		   (function ((cons * (cons * cons))) t))
	    caddr cdddr))

		   
		   