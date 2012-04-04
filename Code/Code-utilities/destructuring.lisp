(in-package #:sicl-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Destructuring a tree according to a lambda list.
;;;
;;; We assume that the lambda-list/pattern are syntactically correct.

(defun destructure-required (required var)
  (if (or (eq required :none) (null required))
      (values '() var)
      (let ((temp1 (gensym))
	    (temp2 (gensym)))
	(multiple-value-bind (bindings rest-var)
	    (destructure-required (cdr required) temp1)
	  (values (append `((,temp1 (if (consp ,var)
					(cdr ,var)
					(error "required end early")))
			    (,temp2 (car ,var)))
			  (destructure-pattern (car required) temp2)
			  bindings)
		  rest-var)))))

(defun destructure-optionals (optionals var)
  (if (or (eq optionals :none) (null optionals))
      (values '() var)
      (let ((optional (car optionals))
	    (temp1 (gensym))
	    (temp2 (gensym)))
	(multiple-value-bind (bindings rest-var)
	    (destructure-optionals (cdr optionals) temp1)
	  (values (append `((,temp1 (if (consp ,var)
					(cdr ,var)
					'()))
			    (,temp2 (if (consp ,var)
					(car ,var)
					,(cadr optional)))
			    ,@(if (consp (cddr optional))
				  `((,(caddr optional) (consp ,var)))))
			  (destructure-pattern (car optional) temp2)
			  bindings)
		  rest-var)))))
		
(defun destructure-keys (keys var)
  (if (or (eq keys :none) (null keys))
      '()
      (let ((key (car keys)))
	(append `((,(cadar key)
		   ,(if (consp (cddr key))
			`(loop for rem = ,var then (cddr rem)
			       while (consp rem)
			       when (eq (car rem) ',(caar key))
				 return (list t (cadr rem))
			       finally (return (list nil ,(cadr key))))
			`(loop for rem = ,var then (cddr rem)
			       while (consp rem)
			       when (eq (car rem) ',(caar key))
				 return (cadr rem)
			       finally (return ,(cadr key))))))
		(if (consp (cddr key))
		    `((,(caddr key)
		       (prog1 (car ,(cadar key))
			 (setf ,(cadar key) (cadr ,(cadar key))))))
		    '())
		(destructure-keys (cdr keys) var)))))

;;; FIXME: check for even keyword/argument pairs.
;;; FIXME: check for &allow-other-keys and :allow-other-keys.
(defun destructure-lambda-list (lambda-list var)
  (multiple-value-bind (required-bindings var1)
      (destructure-required (required lambda-list) var)
    (multiple-value-bind (optional-bindings var2)
	(destructure-optionals (optionals lambda-list) var1)
      (let ((rest-bindings
	      (if (eq (rest-body lambda-list) :none)
		  '()
		  (destructure-pattern (rest-body lambda-list) var2)))
	    (key-bindings
	      (destructure-keys (keys lambda-list) var2)))
	(append required-bindings
		optional-bindings
		rest-bindings
		key-bindings)))))

(defun destructure-pattern (pattern var)
  (cond ((null pattern)
	 `((,(gensym) (unless (null ,var)
			(error "tree should be NIL")))))
	((symbolp pattern)
	 `((,pattern ,var)))
	((consp pattern)
	 (let ((temp1 (gensym))
	       (temp2 (gensym)))
	   (append `((,temp1 (if (consp ,var)
				 (car ,var)
				 (error "no match"))))
		   (destructure-pattern (car pattern) temp1)
		   `((,temp2 (cdr ,var)))
		   (destructure-pattern (cdr pattern) temp2))))
	(t
	 (destructure-lambda-list pattern var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO
;;;
;;; According to CLtL2.

(defun parse-macro (name lambda-list body &optional environment)
  (declare (ignore name environment)) ; For now.
  (let* ((parsed-lambda-list (parse-macro-lambda-list lambda-list))
	 (env-var (environment parsed-lambda-list))
	 (final-env-var (if (eq env-var :none) (gensym) env-var))
	 (form-var (gensym)))
    `(lambda (,form-var ,final-env-var)
       ;; If the lambda list does not contain &environment, then
       ;; we IGNORE the GENSYMed parameter to avoid warnings.
       ;; If the lambda list does contain &envionrment, we do
       ;; not want to make it IGNORABLE because we would want a
       ;; warning if it is not used then. 
       ,@(if (eq env-var :none)
	     `((declare (ignore ,final-env-var)))
	     `())
       (let* ,(destructure-lambda-list parsed-lambda-list form-var)
	 ;; FIXME: insert ignorable variables introduced by
	 ;; destrucuring here.
	 ,@body))))

	 