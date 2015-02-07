(cl:in-package #:cleavir-ast-interpreter)

(defgeneric interpret-ast (ast env))

(defun interpret (ast)
  (assert (typep ast 'cleavir-ast:top-level-function-ast))
  (let ((fun (interpret-ast ast '())))
    (apply fun
	   (mapcar #'eval (cleavir-ast:forms ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility function.

(defun interpret-sequence (sequence env)
  (loop for ast in sequence
	for value = (interpret-ast ast env)
	finally (return value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INTERPRET-AST.

(defmethod interpret-ast ((ast cleavir-ast:progn-ast) env)
  (interpret-sequence (cleavir-ast:form-asts ast) env))

(defmethod interpret-ast ((ast cleavir-ast:setq-ast) env)
  (set-lexical
   (cleavir-ast:lhs-ast ast)
   (interpret-ast (cleavir-ast:value-ast ast) env)
   env))

(defmethod interpret-ast ((ast cleavir-ast:lexical-ast) env)
  (lookup-lexical ast env))

(defmethod interpret-ast ((ast cleavir-ast:symbol-value-ast) env)
  (symbol-value (cleavir-ast:symbol ast)))

(defmethod interpret-ast ((ast cleavir-ast:block-ast) env)
  (let ((tag (list nil)))
    (set-lexical ast tag env)
    (catch tag
      (interpret-ast (cleavir-ast:body-ast ast) env))))

(defmethod interpret-ast ((ast cleavir-ast:return-from-ast) env)
  (let* ((block (cleavir-ast:block-ast ast))
	 (tag (lookup-lexical block env))
	 (form-ast (cleavir-ast:form-ast ast)))
    (throw tag
      (interpret-ast form-ast env))))

(defmethod interpret-ast ((ast cleavir-ast:if-ast) env)
  (if (interpret-ast (cleavir-ast:test-ast ast) env)
      (interpret-ast (cleavir-ast:then-ast ast) env)
      (interpret-ast (cleavir-ast:else-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:tag-ast) env)
  (declare (ignore env))
  nil)

(defmethod interpret-ast ((ast cleavir-ast:go-ast) env)
  (let* ((go-tag-ast (cleavir-ast:tag-ast ast))
	 (catch-tag (lookup-lexical go-tag-ast env)))
    (throw catch-tag go-tag-ast)))

(defmethod interpret-ast ((ast cleavir-ast:tagbody-ast) env)
  (let ((catch-tag (list nil))
	(item-asts (cleavir-ast:item-asts ast)))
    ;; Start by entering the catch tag as the value of each go tags in
    ;; the tagbody in the lexical environment.
    (loop for item-ast in item-asts
	  when (typep item-ast 'cleavir-ast:tag-ast)
	    do (set-lexical item-ast catch-tag env))
    ;; The variable REMAINING-ITEM-ASTS holds a list of the items that
    ;; should be interpreted before the job is done.  Initially, it is
    ;; the list of all the items in the TAGBODY, but a GO will alter
    ;; that.
    (let ((remaining-item-asts item-asts))
      (flet ((do-item ()
	       ;; We know there is at least one remaining item-ast to
	       ;; interpret.
	       (interpret-ast (pop remaining-item-asts)
			      env)))
	(flet ((do-items ()
		 ;; Interpret all remaining items.
		 (loop when (null remaining-item-asts)
			 ;; If there are no more items to interpret,
			 ;; this means whe have reached the end of the
			 ;; TAGBODY, so we are done interpreting it.
			 do (return-from interpret-ast nil)
		       do (do-item))))
	  ;; Each iteration of this loop corresponds to a GO.  If
	  ;; there is no GO in a particular iteration of this loop,
	  ;; then the local function DO-ITEMS will interpret all the
	  ;; remaining items and return from this method.  If there is
	  ;; a GO during the interpretation of one of the items, then
	  ;; the execution of DO-ITEMS will be interrupted by a THROW
	  ;; with the GO-TAG-AST as its value.  We CATCH that event
	  ;; and set the remaining items to the list of items starting
	  ;; with that GO-TAG-AST.
	  (loop do (let ((go-tag-ast (catch catch-tag (do-items))))
		     (setf remaining-item-asts
			   (member go-tag-ast item-asts)))))))))

(defmethod interpret-ast ((ast cleavir-ast:fdefinition-ast) env)
  (let ((name (cleavir-ast:name ast)))
    (fdefinition name)))

(defun load-environment (environment arguments lambda-list)
  (let ((table (make-hash-table :test #'eq))
	(remaining arguments)
	(ll lambda-list))
    (tagbody
     required
       (cond ((null remaining)
	      ;; We ran out of arguments. 
	      (cond ((null ll)
		     ;; There are no more arguments, and no more
		     ;; parameters in the lambda list.  We are done.
		     (go end))
		    ((member (first ll) lambda-list-keywords :test #'eq)
		     ;; There are no more arguments and no more
		     ;; required parameters in the lambda list, but
		     ;; there it at least one of &OPTIONAL, &REST, or
		     ;; &KEY.
		     (go maybe-optional))
		    (t
		     ;; There are no more arguments, but there are
		     ;; more required parameters in the lambda list.
		     (error "Too few arguments"))))
	     ((null ll)
	      ;; There are more arguments, but the remaining lambda
	      ;; list is empty, so there are no parameters to match
	      ;; those arguments to.  
	      (error "Too many arguments"))
	     ((member (first ll) lambda-list-keywords :test #'eq)
	      ;; There are more arguments, the lambda list has more
	      ;; entries in it, and the first entry in the lambda list
	      ;; is a lambda-list keyword.
	      (go maybe-optional))
	     (t
	      ;; There are more arguments, the lambda list has more
	      ;; entries in it, and the first entry in the lambda list
	      ;; is a required parameter in the form of a lexical
	      ;; variable.  Add the first of the remaining arguments
	      ;; to the hash table representing the environment.
	      (setf (gethash (pop ll) table) (pop remaining))
	      (go required)))
     maybe-optional
       ;; The first element of the lambda list is a lambda-list
       ;; keyword, but we don't know which one.  It could be
       ;; &optional, &rest, or &key
       (if (eq (first ll) '&optional)
	   (progn (pop ll)
		  (go optional))
	   (go maybe-rest))
     optional
       ;; We have already seen and removed the lambda-list keyword
       ;; &optional, and we are about to process any remaining
       ;; optional parameters.
       (cond ((null ll)
	      ;; There are no more parameters in the lambda list.
	      (cond ((null remaining)
		     ;; We also have no more arguments.  We are done.
		     (go end))
		    (t
		     ;; There are more arguments, but the remaining lambda
		     ;; list is empty, so there are no parameters to match
		     ;; those arguments to.  
		     (error "Too many arguments"))))
	     ((member (first ll) lambda-list-keywords :test #'eq)
	      ;; The lambda list has more entries in it, and the first
	      ;; entry in the lambda list is a lambda-list keyword.
	      (go maybe-rest))
	     (t
	      ;; We have more optional parameters.
	      (destructuring-bind (var supplied-p) (pop ll)
		(cond ((null remaining)
		       ;; We ran out of arguments.
		       (setf (gethash var table) nil)
		       (setf (gethash supplied-p table) nil))
		      (t
		       (setf (gethash var table) (pop remaining))
		       (setf (gethash supplied-p table) t))))
	      ;; There might be even optional parameters.
	      (go optional)))
     maybe-rest
       ;; The first element of the lambda list is a lambda-list
       ;; keyword, but we don't know which one.  It could be &rest or
       ;; &key.
       (when (eq (first ll) '&rest)
	 (pop ll)
	 (setf (gethash (pop ll) table)
	       (copy-list remaining)))
       (if (null ll)
	   (go end)
	   ;; If the lambda list is not empty, the first element must
	   ;; be &key.  Pop it off and start processing the keyword
	   ;; parameters.
	   (progn (pop ll)
		  (go key)))
     key
       ;; We have already seen and removed the lambda-list keyword
       ;; &key, and we are about to process any remaining key
       ;; parameters.
       (cond ((null ll)
	      ;; There are no more parameters in the lambda list.
	      (cond ((null remaining)
		     ;; We also have no more arguments.  We are done.
		     (go end))
		    (t
		     ;; There are more arguments, but the remaining lambda
		     ;; list is empty, so there are no parameters to match
		     ;; those arguments to.  
		     (error "Illegal keyword argument"))))
	     ((eq (first ll) '&allow-other-keys)
	      ;; The lambda list has more entries in it, and the first
	      ;; entry in the lambda list is the lambda-list keyword
	      ;; &allow-other-keys.  No matter whether there are more
	      ;; arguments, we are done.
	      (go end))
	     (t
	      ;; We have more key parameters.
	      (let ((default (list nil))) ; for use with GETF
		(destructuring-bind (keyword var supplied-p) (pop ll)
		  (cond ((null remaining)
			 ;; We ran out of arguments.
			 (setf (gethash var table) nil)
			 (setf (gethash supplied-p table) nil)
			 ;; There may be more keyword parameters in the
			 ;; lambda list.
			 (go key))
			(t
			 ;; There are more arguments, the lambda list
			 ;; has more entries in it, and the first
			 ;; entry in the lambda list is a key
			 ;; parameter in the form of a list of a
			 ;; keyword and two lexical variables, one for
			 ;; the argument itself, and one indicating
			 ;; that the argument has been supplied.
			 (let ((arg (getf remaining keyword default)))
			   (cond ((eq arg default)
				  (setf (gethash var table) nil)
				  (setf (gethash supplied-p table) nil))
				 (t
				  (setf (gethash var table) arg)
				  (setf (gethash supplied-p table) t)
				  ;; Remove any keyword arguments with
				  ;; the same key from the remaining
				  ;; argument list.
				  (loop while (remf remaining keyword)))))
			 ;; There may be more keyword parameters in
			 ;; the lambda list.
			 (go key)))))))
     end)
    (cons table environment)))

(defmethod interpret-ast ((ast cleavir-ast:function-ast) env)
  (lambda (&rest arguments)
    (let* ((lambda-list (cleavir-ast:lambda-list ast))
	   (new-env (load-environment env arguments lambda-list)))
      (interpret-ast (cleavir-ast:body-ast ast) new-env))))

(defmethod interpret-ast ((ast cleavir-ast:call-ast) env)
  (let ((callee (interpret-ast (cleavir-ast:callee-ast ast)
			       env))
	(args (loop for arg-ast in (cleavir-ast:argument-asts ast)
		    collect (interpret-ast arg-ast env))))
    (apply callee args)))

(defmethod interpret-ast ((ast cleavir-ast:bind-ast) env)
  (progv
      (list (cleavir-ast:symbol ast))
      (list (interpret-ast (cleavir-ast:value-ast ast) env))
    (interpret-ast (cleavir-ast:body-ast ast) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interpret EQ-AST.

(defmethod interpret-ast ((ast cleavir-ast:eq-ast) env)
  (eq (interpret-ast (cleavir-ast:arg1-ast ast) env)
      (interpret-ast (cleavir-ast:arg2-ast ast) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interpret THE-AST.
;;;
;;; Recall that the THE-AST contains a list of type specifiers.  This
;;; list has a length of 1 if VALUES was not used for the VALUE type
;;; of the THE form.
;;;
;;; The HyperSpec tells us that if there are fewer type specifiers
;;; than values of the FORM, then the remaining type specifiers are
;;; considered to be T.  In other words, in this case we only check
;;; the type of as many values as there are type specifiers.  The
;;; HyperSpec also tells us that if there are more type specifiers
;;; than values, then for the purpose of type checking, missing values
;;; are treated as NIL, so that NIL must be of the type specified by
;;; the additional type specifiers.  Finally, the HyperSpec tells us
;;; that independently of the number of type specifiers, the THE form
;;; yields as may values as the FORM does. 

(defmethod interpret-ast ((ast cleavir-ast:the-ast) env)
  (let* ((form-ast (cleavir-ast:form-ast ast))
	 (type-specifiers (cleavir-ast:type-specifiers ast))
	 (values (multiple-value-list (interpret-ast form-ast env))))
    (loop for value in values
	  for type-specifier in type-specifiers
	  do (assert (typep value type-specifier)))
    (loop for type-specifier in (nthcdr (length values) type-specifiers)
	  do (assert (typep nil type-specifier)))
    (apply #'values values)))
					
	 
