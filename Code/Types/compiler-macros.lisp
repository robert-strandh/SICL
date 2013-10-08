(cl:in-package #:sicl-type)

(define-compiler-macro typep
    (&whole form object type-specifier &optional environment)
  (if (and (null environment)
	   (consp type-specifier)
	   (eq (car type-specifier) 'quote)
	   (not (null (cdr type-specifier)))
	   (null (cddr type-specifier)))
      `(typeq ,object ,(cadr type-specifier))
      form))


  
