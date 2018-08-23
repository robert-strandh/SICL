(cl:in-package #:sicl-minimal-extrinsic-environment)

;;;; Generate code for parsing a list of arguments according to some
;;;; lambda list.
;;;;
;;;; The lambda list used here has the same lambda-list keywords as an
;;;; ordinary lambda list and in the same order.  Parameters have the
;;;; following form:
;;;;
;;;;   * Each required argument is just a symbol.
;;;;
;;;;   * Each &OPTIONAL argument is a list of two symbols.  The first
;;;;     element of the list is the name of the variable to hold the
;;;;     supplied, argument or NIL if no argument was supplied.  The
;;;;     second element of the list is the name of the associated
;;;;     SUPPLIED-P parameter.
;;;;
;;;;  * Each &KEY argument is a list of three symbols.  The first
;;;;    element of the list is the keyword to be used to pass the
;;;;    argument.  The second element of the list is the name of the
;;;;    variable to hold the supplied argument, or NIL if no argument
;;;;    was supplied.  The third element of the list is the name of the
;;;;    associated SUPPLIED-P parameter.

(defvar *linkage-environment*)

;;; Generate a form that parses the arguments to a function.
;;;
;;; The parameter VAR contains the name a variable (a symbol) that at
;;; runtime will hold the remaining arguments to be parsed.  The value
;;; of that variable is changed at runtime as more arguments are
;;; parsed.
(defun parse (lambda-list arguments-var var)
  (let ((result '())
	(rest lambda-list)
	(default (list nil)))
    (tagbody
     required
       (if (null rest)
	   (progn (push `(unless (null ,var)
			   (traced-funcall
			    ,*linkage-environment*
                            nil
			    (car (load-time-value
				  (sicl-genv:function-cell
				   'error
				   ,*linkage-environment*)
				  nil))
			    'sicl-additional-conditions:too-many-arguments
			    :lambda-list ',lambda-list
			    :arguments ,arguments-var))
			result)
		  (go out))
	   (let ((first (pop rest)))
	     (cond ((eq first '&optional)
		    (go optional))
		   ((or (eq first '&rest) (eq first '&body))
		    (go rest))
		   ((eq first '&key)
		    (go key))
		   (t
		    (push `(if (null ,var)
			       (traced-funcall
				,*linkage-environment*
                                nil
				(car (load-time-value
				      (sicl-genv:function-cell
				       'error
				       ,*linkage-environment*)
				      nil))
				'sicl-additional-conditions:too-few-arguments
				:lambda-list ',lambda-list
				:arguments ,arguments-var)
			       (setq ,first (pop ,var)))
			  result)
		    (go required)))))
     optional
       (if (null rest)
	   (progn (push `(unless (null ,var)
			   (traced-funcall
			    ,*linkage-environment*
                            nil
			    (car (load-time-value
				  (sicl-genv:function-cell
				   'error
				   ,*linkage-environment*)
				  nil))
			    'sicl-additional-conditions:too-many-arguments
			    :lambda-list ',lambda-list
			    :arguments ,arguments-var))
			result)
		  (go out))
	   (let ((first (pop rest)))
	     (cond ((or (eq first '&rest) (eq first '&body))
		    (go rest))
		   ((eq first '&key)
		    (go key))
		   (t
		    (push `(if (null ,var)
			       (setq ,(first first) nil
				     ,(second first) nil)
			       (setq ,(first first) (pop ,var)
				     ,(second first) t))
			  result)
		    (go optional)))))
     rest
       (push `(setq ,(pop rest) (copy-list ,var))
	     result)
       (if (null rest)
	   ;; We are done.
	   (go out)
	   ;; The first element of REST must be &key.
	   (progn (pop rest)
		  (go key)))
     key
       (if (null rest)
	   (progn (push `(unless (or (null ,var)
				     (getf ,var :allow-other-keys))
			   (traced-funcall
			    ,*linkage-environment*
                            nil
			    (car (load-time-value
				  (sicl-genv:function-cell
				   'error
				   ,*linkage-environment*)
				  nil))
			    'sicl-additional-conditions:unrecognized-keyword-argument
			    :keyword (first ,var)
			    :lambda-list ',lambda-list
			    :arguments ,arguments-var))
			result)
		  (go out))
	   (let ((first (pop rest)))
	     (if (eq first '&allow-other-keys)
		 (go out)
		 (destructuring-bind (keyword variable supplied-p) first
		   (push `(if (eq (setq ,variable
					(getf ,var ,keyword ',default))
				  ',default)
			      (setq ,supplied-p nil
				    ,variable nil)
			      (progn (setq ,supplied-p t)
				     (loop while (remf ,var ,keyword))))
			 result)
		   (go key)))))
     out)
    (reverse result)))

;;; Generate a form that parses the arguments to a function.
;;;
;;; The function for which argument parsing code is to be generated
;;; takes a single &REST argument.  The name of that argument (a
;;; symbol) is the value of the parameter named ARGUMENT-VARIABLE.
;;;
;;; ERROR is the name of a variable (a symbol) that holds the function
;;; ERROR to be called in case an error is detected.
;;;
;;; In order to avoid modifying the value of the &REST parameter of
;;; the resulting code, we generate a temporary variable that will be
;;; modified as argument parsing progresses.
(defun build-argument-parsing-code (lambda-list argument-variable)
  (let ((remaining-argument-variable (gensym)))
    `(let ((,remaining-argument-variable ,argument-variable))
       ,@(parse lambda-list argument-variable remaining-argument-variable))))
