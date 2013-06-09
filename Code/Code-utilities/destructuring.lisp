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
;;; Matching arguments according to a lambda list.

(defun match-required (variables arg-op)
  (loop for variable in variables
	for i from 0
	collect `(,variable (,arg-op ,i))))

;;; Recall that an optional-entry is a list of one of two forms:
;;;
;;;  * (variable init-arg)
;;;  * (variable init-arg supplied-p-parameter)
;;;
(defun match-optional (optional-entries first-count arg-count-op arg-op)
  (loop for (variable init-arg . rest) in optional-entries
	for i from first-count
	collect `(,variable (if (> (,arg-count-op) ,i)
				(,arg-op ,i)
				,init-arg))
	unless (null rest)
	  collect `(,(car rest) (> (,arg-count-op) ,i))))

(defun match-rest/body (variable first-count arg-count-op arg-op)
  `(,variable (let ((temp '())
		    (n (,arg-count-op)))
		(tagbody
		 again
		   (when (= n ,first-count)
		     (go out))
		   (decf n)
		   (push (,arg-op n) temp)
		   (go again)
		 out)
		temp)))

;;; Recall that a key-entry is a list of one of two forms:
;;;
;;;  * ((keyword variable) init-form)
;;;  * ((keyword variable) init-form supplied-p-parameter)
;;;
(defun match-key (key-entries first-count arg-count-op arg-op)
  (loop for ((keyword variable) init-form . rest) in key-entries
	for counter = (gensym)
	collect `(,variable (let ((,counter ,first-count))
			      (block nil
				(tagbody
				 again
				   (when (= ,counter (,arg-count-op))
				     (go out))
				   (when (eq (,arg-op n) ,keyword)
				     (return (,arg-op (1+ ,counter))))
				   (go again)
				 out)
				,init-form)))
	unless (null rest)
	  collect `(,(car rest) (let ((,counter ,first-count))
				  (block nil
				    (tagbody
				     again
				       (when (= ,counter (,arg-count-op))
					 (go out))
				       (when (eq (,arg-op n) ,keyword)
					 (return t))
				       (go again)
				     out)
				    nil)))))

;;; Generate code to check that the number of arguments supplied is
;;; acceptable.
;;; 
;;; MIN is a number.  If it is 0, then we don't check the minimum
;;; number of arguments supplied.  MAX is a number, or NIL (meaning
;;; "no upper bound").  
(defun check-arg-count (min max arg-count-op)
  `(,@(if (= min 0)
	  `()
	  `((when (< (,arg-count-op) ,min)
	      (error "too few arguments"))))
    ,@(if (null max)
	  `()
	  `((when (> (,arg-count-op) ,max)
	      (error "too many arguments"))))))

;;; Generate code to check that there is an even number of keyword
;;; arguments.
(defun check-even-number-of-keyword-arguments (first-count arg-count-op)
  `(unless (,(if (evenp first-count) 'evenp 'oddp) (,arg-count-op))
     (error "odd number of keyword arguments")))

;;; Generate code to check that either :ALLOW-OTHER KEYS <true> is a
;;; keyword argument or that all the keyword arguments are valid.  The
;;; HyperSpec says that :ALLOW-OTHER-KEYS <something> is always valid,
;;; so even if we have :ALLOW-OTHER-KEYS <false>, it is valid.
;;; Furthermore, since there can be multiple instances of keyword
;;; arguments, and the first one is used to determine the ultimate
;;; value of the corresponding variable, we must determine whether
;;; :ALLOW-OTHER-KEYS is true or not from the first occurrence.  This
;;; function is only called when &allow-other-keys is not given in the
;;; lambda list.
(defun check-valid-keyword-arguments
    (keywords first-count arg-count-op arg-op)
  `(loop
     for i from ,first-count below (,arg-count-op) by 2
     do (if (eq (,arg-op i) :allow-other-keys)
	    (if (null (,arg-op (1+ i)))
		(loop-finish)
		(return)))
     finally
	(loop
	  for i from ,first-count below (,arg-count-op) by 2
	  do (unless (loop with arg = (,arg-op i)
			   for keyword in ,(cons :allow-other-keys keywords)
			   when (eq arg keyword)
			     return t)
	       (error "unknown keyword argument")))))

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
	 (form-var (gensym))
	 (args-var (gensym)))
    `(lambda (,form-var ,final-env-var)
       ;; If the lambda list does not contain &environment, then
       ;; we IGNORE the GENSYMed parameter to avoid warnings.
       ;; If the lambda list does contain &envionrment, we do
       ;; not want to make it IGNORABLE because we would want a
       ;; warning if it is not used then. 
       ,@(if (eq env-var :none)
	     `((declare (ignore ,final-env-var)))
	     `())
       (let ((,args-var (cdr ,form-var)))
	 (let* ,(destructure-lambda-list parsed-lambda-list args-var)
	   ;; FIXME: insert ignorable variables introduced by
	   ;; destrucuring here.
	   ,@body)))))

	 
