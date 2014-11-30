(in-package #:cleavir-generate-ast)

(defmethod convert-special :around (symbol form env)
  (declare (ignore env))
  (check-special-form-syntax symbol form)
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    ((symbol (eql 'quote)) form env)
  (cleavir-ast:make-constant-ast (cadr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special
    ((symbol (eql 'block)) form env)
  (let* ((ast (cleavir-ast:make-block-ast nil))
	 (new-env (cleavir-env:add-block env (cadr form) ast))
	 (forms (convert-sequence (cddr form) new-env)))
    (setf (cleavir-ast:body-ast ast)
	  (cleavir-ast:make-progn-ast forms))
    ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defmethod convert-special
    ((symbol (eql 'eval-when)) form environment)
  (destructuring-bind (situations . body) (rest form)
    (cond ((or (and (or (member :compile-toplevel situations)
			(member 'compile situations))
		    (or (member :load-time-toplevel situations)
			(member 'load situations)))
	       (and (not (or (member :compile-toplevel situations)
			     (member 'compile situations)))
		    (or (member :load-time-toplevel situations)
			(member 'load situations))
		    (or (member :execute situations)
			(member 'eval situations))
		    *compile-time-too*))
	   (let ((*compile-time-too* t))
	     (cleavir-ast:make-progn-ast
	      (convert-sequence (cddr form) environment))))
	  ((or (and (not (or (member :compile-toplevel situations)
			     (member 'compile situations)))
		    (or (member :load-time-toplevel situations)
			(member 'load situations))
		    (or (member :execute situations)
			(member 'eval situations))
		    (not *compile-time-too*))
	       (and (not (or (member :compile-toplevel situations)
			     (member 'compile situations)))
		    (or (member :load-time-toplevel situations)
			(member 'load situations))
		    (not (or (member :execute situations)
			     (member 'eval situations)))))
	   (let ((*compile-time-too* nil))
	     (cleavir-ast:make-progn-ast
	      (convert-sequence (cddr form) environment))))
	  ((or (and (or (member :compile-toplevel situations)
			(member 'compile situations))
		    (not (or (member :load-time-toplevel situations)
			     (member 'load situations))))
	       (and (not (or (member :compile-toplevel situations)
			     (member 'compile situations)))
		    (not (or (member :load-time-toplevel situations)
			     (member 'load situations)))
		    (or (member :execute situations)
			(member 'eval situations))
		    *compile-time-too*))
	   (cleavir-env:eval `(progn ,@body) environment environment)
	   (convert nil environment))
	  (t
	   (convert nil environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

(defmethod convert-special ((symbol (eql 'flet)) form env)
  (let ((new-env env))
    ;; Create a new environment with the additional names.
    (loop for def in (cadr form)
	  for name = (car def)
	  for var-ast = (cleavir-ast:make-lexical-ast name)
	  do (setf new-env (cleavir-env:add-local-function new-env name var-ast)))
    (let ((init-asts
	    (loop for (name lambda-list . body) in (cadr form)
		  for fun = (convert-code lambda-list body env)
		  collect (cleavir-ast:make-setq-ast
			   (let ((info (cleavir-env:function-info new-env name)))
			     (cleavir-env:identity info))
			   fun))))
      (multiple-value-bind (declarations forms)
	  (cleavir-code-utilities:separate-ordinary-body (cddr form))
	(setf new-env (augment-environment-with-declarations
		       new-env declarations))
	(cleavir-ast:make-progn-ast
	 (append init-asts (convert-sequence forms new-env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.

(defgeneric convert-function (info))

(defmethod convert-function ((info cleavir-env:global-function-info))
  (let* ((name (cleavir-env:name info))
	 (name-ast (cleavir-ast:make-constant-ast name)))
    (make-instance 'cleavir-ast:fdefinition-ast
      :name-ast name-ast)))

(defmethod convert-function ((info cleavir-env:local-function-info))
  (cleavir-env:identity info))

(defun convert-named-function (name environment)
  (let ((info (cleavir-env:function-info environment name)))
    (convert-function info)))

(defun convert-lambda-function (lambda-form env)
  (convert-code (cadr lambda-form) (cddr lambda-form) env))

(defmethod convert-special ((symbol (eql 'function)) form env)
  (if (proper-function-name-p (cadr form))
      (convert-named-function (cadr form) env)
      (convert-lambda-function (cadr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-special ((symbol (eql 'go)) form env)
  (let ((info (tag-info env (cadr form))))
    (cleavir-ast:make-go-ast
     (cleavir-env:identity info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-special ((symbol (eql 'if)) form env)
  (cleavir-ast:make-if-ast
   (convert (cadr form) env)
   (convert (caddr form) env)
   (if (null (cdddr form))
       (cleavir-ast:make-constant-ast nil)
       (convert (cadddr form) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LABELS.

(defmethod convert-special ((symbol (eql 'labels)) form env)
  (let ((new-env env))
    ;; Create a new environment with the additional names.
    (loop for def in (cadr form)
	  for name = (car def)
	  for var-ast = (cleavir-ast:make-lexical-ast name)
	  do (setf new-env (cleavir-env:add-local-function new-env name var-ast)))
    (let ((init-asts
	    (loop for (name lambda-list . body) in (cadr form)
		  for fun = (convert-code lambda-list body new-env)
		  collect (cleavir-ast:make-setq-ast
			   (let ((info (cleavir-env:function-info new-env name)))
			     (cleavir-env:identity info))
			   fun))))
      (multiple-value-bind (declarations forms)
	  (cleavir-code-utilities:separate-ordinary-body (cddr form))
	(setf new-env (augment-environment-with-declarations
		       new-env declarations))
	(cleavir-ast:make-progn-ast
	 (append init-asts (convert-sequence forms new-env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET and LET*

(defun init-form (binding)
  (if (or (symbolp binding) (null (cdr binding)))
      nil
      (cadr binding)))

;;; Separate a list of canonicalized declaration specifiers into two
;;; disjoint sets, returned as two values.  The first set contains All
;;; the declerations specifiers that concern an ordinary variable
;;; named NAME, and the second set the remaining declaration specifiers.
(defun separate-declarations (canonicalized-declaration-specifiers name)
  (loop for spec in canonicalized-declaration-specifiers
	if (or (and (eq (first spec) 'ignore)
		    (eq (second spec) name))
	       (and (eq (first spec) 'ignorable)
		    (eq (second spec) name))
	       (and (eq (first spec) 'dynamic-extent)
		    (eq (second spec) name))
	       (and (eq (first spec) 'special)
		    (eq (second spec) name))
	       (and (eq (first spec) 'type)
		    (eq (third spec) name)))
	  collect spec into first
	else
	  collect spec into second
	finally (return (values first second))))

;;; Given a list of variables bound by the LET form, and a list of
;;; canonicalized declarations specifiers, return an environment to be
;;; used to compile the body.  The ENV parameter is the environment in
;;; which the LET form is converted.  It is used in order to determine
;;; whether any variable is globally special. 
(defun construct-let-body-environment (variables declarations env)
  (flet ((augment (variable env orig-env)
	   (augment-environment-with-variable
	    variable declarations env orig-env)))
    (loop for variable in variables
	  for new-env = (augment variable env env)
	    then (augment variable new-env env)
	  finally (return new-env))))

(defmethod convert-special
    ((symbol (eql 'let)) form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      (let* ((canonical-declarations
	       (cleavir-code-utilities:canonicalize-declaration-specifiers 
		(mapcar #'cdr declarations)))
	     (variables (loop for binding in bindings
			      collect (if (symbolp binding)
					  binding
					  (first binding))))
	     (body-env (construct-let-body-environment
			variables canonical-declarations env))
	     (ast (cleavir-ast:make-progn-ast
		   (convert-sequence forms body-env))))
	(loop for binding in (reverse bindings)
	      for var in (reverse variables)
	      for info = (cleavir-env:variable-info body-env var)
	      for init = (if (symbolp binding) nil (second binding))
	      for init-ast = (convert init env)
	      do (setf ast
		       (if (variable-is-special-p
			    var canonical-declarations env)
			   (cleavir-ast:make-bind-ast var init-ast ast)
			   (let ((lexical (cleavir-env:identity info)))
			     (cleavir-ast:make-progn-ast 
			      (list (cleavir-ast:make-setq-ast lexical init-ast)
				    ast))))))
	ast))))

(defmethod convert-special
    ((symbol (eql 'let*)) form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      (let* ((canonical-declarations
	       (cleavir-code-utilities:canonicalize-declaration-specifiers 
		(mapcar #'cdr declarations)))
	     (variables (loop for binding in bindings
			      collect (if (symbolp binding)
					  binding
					  (first binding))))
	     (environments
	       (flet ((augment (variable env)
			(augment-environment-with-variable
			 variable canonical-declarations env env)))
		 (loop for variable in variables
		       for new-env = (augment variable env)
			 then (augment variable new-env)
		       collect new-env)))
	     (ast (cleavir-ast:make-progn-ast
		   (convert-sequence forms (first (last (cons env environments)))))))
	(loop for binding in (reverse bindings)
	      for var in (reverse variables)
	      for (new old) on (reverse (cons env environments))
	      for info = (cleavir-env:variable-info new var)
	      for init = (if (symbolp binding) nil (second binding))
	      for init-ast = (convert init old)
	      do (setf ast
		       (if (variable-is-special-p
			    var canonical-declarations env)
			   (cleavir-ast:make-bind-ast var init-ast ast)
			   (let ((lexical (cleavir-env:identity info)))
			     (cleavir-ast:make-progn-ast 
			      (list (cleavir-ast:make-setq-ast lexical init-ast)
				    ast))))))
	ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(defmethod convert-special
    ((symbol (eql 'load-time-value)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 2)
  (cleavir-ast:make-load-time-value-ast
   (convert (cadr form) environment)
   (caddr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, LOCALLY processes
;;; its subforms the same way as the form itself.

(defmethod convert-special
    ((symbol (eql 'locally)) form env)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body (cdr form))
    (let ((new-env (augment-environment-with-declarations
		    env declarations)))
      ;; The following expression means "if this form is a top-level
      ;; form, then make sure that forms compiled by a recursive call
      ;; to CONVERT are also top-level forms".
      (setf *top-level-form-p* *old-top-level-form-p*)
      (cleavir-ast:make-progn-ast
       (convert-sequence forms new-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;;; its subforms the same way as the form itself.

(defmethod convert-special
    ((symbol (eql 'macrolet)) form env)
  (destructuring-bind (definitions &rest body) (rest form)
    (let ((new-env env))
      (loop for (name lambda-list . body) in definitions
	    for lambda-expr = (cleavir-code-utilities:parse-macro
			        name lambda-list body env)
	    for expander = (cleavir-env:eval lambda-expr env env)
	    do (setf new-env
		     (cleavir-env:add-local-macro new-env name expander)))
      ;; The following expression means "if this form is a top-level
      ;; form, then make sure that forms compiled by a recursive call
      ;; to CONVERT are also top-level forms".
      (setf *top-level-form-p* *old-top-level-form-p*)
      (convert `(locally ,@body) new-env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, PROGN processes
;;; its subforms the same way as the form itself.

(defmethod convert-special
    ((head (eql 'progn)) form environment)
  ;; The following expression means "if this form is a top-level
  ;; form, then make sure that forms compiled by a recursive call
  ;; to CONVERT are also top-level forms".
  (setf *top-level-form-p* *old-top-level-form-p*)
  (cleavir-ast:make-progn-ast
   (convert-sequence (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special ((symbol (eql 'return-from)) form env)
  (let ((info (block-info env (cadr form))))
    (cleavir-ast:make-return-from-ast
     (cleavir-env:identity info)
     (convert (caddr form) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;
;;; Recall that the SETQ-AST is a NO-VALUE-AST-MIXIN.  We must
;;; therefore make sure it is always compiled in a context where its
;;; value is not needed.  We do that by wrapping a PROGN around it.

(defgeneric convert-setq (info form-ast))

(defmethod convert-setq ((info cleavir-env:constant-variable-info) form-ast)
  (error 'setq-constant-variable
	 :form (cleavir-env:name info)))

(defmethod convert-setq ((info cleavir-env:lexical-variable-info) form-ast)
  (cleavir-ast:make-progn-ast 
   (list (cleavir-ast:make-setq-ast
	  (cleavir-env:identity info)
	  form-ast)
	 (cleavir-env:identity info))))

(defmethod convert-setq ((info cleavir-env:special-variable-info) form-ast)
  (let ((temp (cleavir-ast:make-lexical-ast (gensym))))
    (cleavir-ast:make-progn-ast
     (list (cleavir-ast:make-setq-ast temp form-ast)
	   (cleavir-ast:make-set-symbol-value-ast
	    (cleavir-ast:make-constant-ast (cleavir-env:name info))
	    temp)
	   temp))))

(defun convert-elementary-setq (var form env)
  (convert-setq (variable-info env var) (convert form env)))
  
(defmethod convert-special
    ((symbol (eql 'setq)) form environment)
  (let ((form-asts (loop for (var form) on (cdr form) by #'cddr
			 collect (convert-elementary-setq
				  var form environment))))
    (cleavir-ast:make-progn-ast form-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-special
    ((head (eql 'symbol-macrolet)) form env)
  (let ((new-env env))
    (loop for (name expansion) in (cadr form)
	  do (setf new-env
		   (cleavir-env:add-local-symbol-macro new-env name expansion)))
    ;; The following expression means "if this form is a top-level
    ;; form, then make sure that forms compiled by a recursive call to
    ;; CONVERT are also top-level forms".
    (setf *top-level-form-p* *old-top-level-form-p*)
    (convert `(progn ,@(cddr form)) new-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.
;;;
;;; The TAGBODY special form always returns NIL.  We generate a PROGN
;;; with the TAGBODY-AST and a CONSTANT-AST in it, because the
;;; TAGBODY-AST (unlike hte TAGBODY special form) does not generate a
;;; value.

(defmethod convert-special
    ((symbol (eql 'tagbody)) form env)
  (let ((tag-asts
	  (loop for item in (cdr form)
		when (symbolp item)
		  collect (cleavir-ast:make-tag-ast item)))
	(new-env env))
    (loop for ast in tag-asts
	  do (setf new-env (cleavir-env:add-tag
			    new-env (cleavir-ast:name ast) ast)))
    (let ((items (loop for item in (cdr form)
		       collect (if (symbolp item)
				   (pop tag-asts)
				   (convert item new-env)))))
      (cleavir-ast:make-progn-ast
       (list (cleavir-ast:make-tagbody-ast items)
	     (cleavir-ast:make-constant-ast nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.

(defmethod convert-special
    ((symbol (eql 'the)) form environment)
  (destructuring-bind (value-type subform) (rest form)
    (cleavir-ast:make-the-ast
     (convert subform environment)
     (if (and (consp value-type) (eq (car value-type) 'values))
	 (cdr value-type)
	 (list value-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.

(defmethod convert-special
    ((symbol (eql 'multiple-value-call)) form environment)
  (destructuring-bind (function-form . forms) (rest form)
    (cleavir-ast:make-multiple-value-call-ast
     (convert function-form environment)
     (convert-sequence forms environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.

(defmethod convert-special
    ((symbol (eql 'multiple-value-prog1)) form environment)
  (destructuring-bind (first-form . forms) (rest form)
    (cleavir-ast:make-multiple-value-prog1-ast
     (convert first-form environment)
     (convert-sequence forms environment))))
