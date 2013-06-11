(in-package #:sicl-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Destructuring a tree according to a lambda list.
;;;
;;; We assume that the lambda-list/pattern are syntactically correct.
;;;
;;; The function DESTRUCTURE-REQUIRED and DESTRUCTURE-OPTIONALS return
;;; two values:
;;;
;;;  * A list of binding forms to be used in a LET*.
;;;
;;;  * A variable to be used to destructure the remaining pattern.

;;; Recall that the required parameters of a parsed lambda list is
;;; either the keyword :NONE, or a list of patterns.  
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

;;; Recall that the optional parameters of a parsed lambda list is
;;; either the keyword :none, or it is a list of &optional entries.
;;; Each optional entry has one of the two forms:
;;;
;;;  * (pattern init-arg)
;;;
;;;  * (pattern init-arg supplied-p-parameter)
;;;
;;; If the original lambda-list did not have an init-arg, the parsing
;;; process supplied NIL so that the parsed lambda list always has an
;;; init-arg.
;;;
;;; The HyperSpec says that if there is an &optional parameter and the
;;; object to be destructured "ends early", then the init-arg is
;;; evaluated and destructured instead.  We interpret this phrase to
;;; mean that either:
;;;
;;;   * the object to be destructured is NIL, in which case it "ends
;;;     early", and we destructure the init-arg instead.
;;;
;;;   * the object is a CONS, in which case the CAR of the object is
;;;     matched against the pattern.  If it doesn't match, then an
;;;     error is signaled and no attemp is made to match agains the
;;;     init-arg.
;;;
;;;   * the object is an atom other than NIL.  Then an error is
;;;     signaled.
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
					(if (null ,var)
					    '()
					    (error "optional expected"))))
			    ;; If the object is not a CONS, then it is
			    ;; either NIL in which case we destructure
			    ;; the init-arg instead, or else it is an
			    ;; atom other than NIL and we have already
			    ;; signaled an error before, so we don't
			    ;; need to handle that case again.
			    (,temp2 (if (consp ,var)
					(car ,var)
					,(cadr optional)))
			    ;; If a supplied-p-parameter exists, then
			    ;; we give it the value TRUE whenever the
			    ;; object is a CONS, even though later
			    ;; an error might be signaled because there
			    ;; is no match. 
			    ,@(if (consp (cddr optional))
				  `((,(caddr optional) (consp ,var)))))
			  (destructure-pattern (car optional) temp2)
			  bindings)
		  rest-var)))))

;;; Recall that the keys part of a compiled lambda list is either
;;; :none, which means that no &key was given at all, or a list if
;;; &key entries.  If the list is empty, it means that &key was given,
;;; but no &key parameters followed.
;;;
;;; A &key entry is either:
;;;
;;;  * ((keyword pattern) init-form)
;;; 
;;;  * ((keyword pattern) init-form supplied-p-parameter)
;;;
;;; The HyperSpec is pretty skimpy about what happens with keyword
;;; arguments ("the rest of the list ... is taken apart
;;; appropriately").  What we do is the following:
;;;
;;;  * If there is a keyword argument with the right keyword, then
;;;    its value is matched against the pattern.
;;;
;;;  * Otherwise, the value of the init-form is matched agains the
;;;    pattern.
(defun destructure-keys (keys var)
  (if (or (eq keys :none) (null keys))
      '()
      (let ((key (car keys))
	    (temp (gensym)))
	(append `(;; What we do in step 1 depends on whether there is
		  ;; a supplied-p-parameter or not.  If there is, then
		  ;; in step 1, we return a list of two things:
		  ;; 
		  ;;  * a boolean indicating whether we found the
		  ;;    keyword.
		  ;;
		  ;;  * either the argument found, or the value of the
		  ;;    init-form if no argument was found.
		  ;;
		  ;; If there is no supplied-p-parameter, then we just
		  ;; return the argument found or the value of the
		  ;; init-form if no argument was found.
		  (,temp
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
			       finally (return ,(cadr key)))))
		  ;; If there is no supplied-p-parameter, then we are
		  ;; done.  If there is, we must get it from the first
		  ;; element of the list computed in step 1, and we must
		  ;; replace that list with its second element.
		  ,@(if (consp (cddr key))
			`((,(caddr key)
			   (prog1 (car ,temp)
			     (setf ,temp (cadr ,temp)))))
			'()))
		(destructure-pattern (cadar key) temp)
		(destructure-keys (cdr keys) var)))))

;; We return two values.  The first value is a list of bindings to be
;; used with a LET* and the purpose of which is to destructure the
;; arguments in VAR according to the LAMBDA-LIST.  The second value is
;; a list of variables that ar bound in the bindings, but that should
;; be declared IGNORE because they are introduced for technical
;; reasons and not used anywhere.
(defun destructure-lambda-list (lambda-list var)
  (multiple-value-bind (required-bindings var1)
      (destructure-required (required lambda-list) var)
    (multiple-value-bind (optional-bindings var2)
	(destructure-optionals (optionals lambda-list) var1)
      (let ((error-check-bindings '())
	    (variables-to-ignore '()))
	;; Generate bindings that check some conditions. 
	(cond ((and (eq (rest-body lambda-list) :none)
		    (eq (keys lambda-list) :none))
	       ;; If there is neither a &rest/&body nor any keyword
	       ;; parameters, then the remaining list must be NIL, or
	       ;; else we signal an error.
	       (let ((temp (gensym)))
		 (push temp variables-to-ignore)
		 (push `(,temp (when (not (null ,var2))
				 (error "too many arguments supplied")))
		       error-check-bindings)))
	      ((not (eq (keys lambda-list) :none))
	       ;; If there are keyword parameters, then we must check
	       ;; several things.  First, we must check that the
	       ;; remaining list is a proper list and that it has an
	       ;; even number of elements.
	       (let ((temp (gensym)))
		 (push temp variables-to-ignore)
		 (push `(,temp (multiple-value-bind (length structure)
				   (list-structure ,var2)
				 ;; First, the remaining list must be
				 ;; a proper list.
				 (unless (eq structure :proper)
				   (error "with keyword parameters, ~
                                           the arguments must be a ~
                                           proper list."))
				 ;; Second, it must have an even
				 ;; number of elements.
				 (unless (evenp length)
				   (error "with keyword parameters, ~
                                           the keyword part of the ~
                                            arguments must have an ~
                                            even number of elements."))))
		       error-check-bindings))
	       ;; If &allow-other keys was not given, more checks have
	       ;; to be made.
	       (unless (allow-other-keys lambda-list)
		 (let ((temp (gensym))
		       (allowed-keywords (mapcar #'caar (keys lambda-list))))
		   (push temp variables-to-ignore)
		   ;; Perhaps there was a :allow-other-keys <true> in
		   ;; the argument list.  As usual, if there are
		   ;; several pairs :allow-other-keys <mumble> then it
		   ;; is the first one that counts.  This happens to
		   ;; be exactly what GETF checks for so use it.
		   (push `(,temp (unless (getf var2 :allow-other-keys)
				   ;; Either no :allow-other-keys was
				   ;; found, or the first one found
				   ;; had a value of NIL.  Then every
				   ;; keyword in the argument list
				   ;; must be one of the ones supplied
				   ;; in the parameters.
				   (let ()
				     (loop for keyword in var2 by #'cddr
					   unless (member keyword
							  ,allowed-keywords)
					     do (error "unknown keyword ~s"
						       keyword)))))
			 error-check-bindings))))
	      (t
	       ;; If there are no keyword parameters, but there is a
	       ;; &rest/&body, then we do no checks, which means that
	       ;; the argument list can have any structure, including
	       ;; circular.  the remaining list is simply matched with
	       ;; the &rest/&body pattern.
	       nil))
	(let ((rest-bindings
		(if (eq (rest-body lambda-list) :none)
		    '()
		    (destructure-pattern (rest-body lambda-list) var2)))
	      (key-bindings
		(destructure-keys (keys lambda-list) var2)))
	  (values (append required-bindings
			  optional-bindings
			  rest-bindings
			  (reverse error-check-bindings)
			  key-bindings
			  (if (eq (aux lambda-list) :none)
			      '()
			      (aux lambda-list)))
		  variables-to-ignore))))))

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
    (multiple-value-bind (bindings ignored-variables)
	(destructure-lambda-list parsed-lambda-list args-var)
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
	   (let* ,bindings
	     (declare (ignore ,@ignored-variables))
	     ,@body))))))

	 
