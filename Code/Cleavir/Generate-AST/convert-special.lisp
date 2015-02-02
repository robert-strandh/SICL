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
    (if (or (eq *compiler* 'cl:compile) (eq *compiler* 'cl:eval))
	(if (or (member :execute situations)
		(member 'eval situations))
	    (cleavir-ast:make-progn-ast
	     (convert-sequence (cddr form) environment))
	    (convert nil environment))
	(cond ((or (and (or (member :compile-toplevel situations)
			    (member 'compile situations))
			(or (member :load-toplevel situations)
			    (member 'load situations)))
		   (and (not (or (member :compile-toplevel situations)
				 (member 'compile situations)))
			(or (member :load-toplevel situations)
			    (member 'load situations))
			(or (member :execute situations)
			    (member 'eval situations))
			*compile-time-too*))
	       (let ((*compile-time-too* t))
		 (convert `(progn ,@(cddr form)) environment)))
	      ((or (and (not (or (member :compile-toplevel situations)
				 (member 'compile situations)))
			(or (member :load-toplevel situations)
			    (member 'load situations))
			(or (member :execute situations)
			    (member 'eval situations))
			(not *compile-time-too*))
		   (and (not (or (member :compile-toplevel situations)
				 (member 'compile situations)))
			(or (member :load-toplevel situations)
			    (member 'load situations))
			(not (or (member :execute situations)
				 (member 'eval situations)))))
	       (let ((*compile-time-too* nil))
		 (convert `(progn ,@(cddr form)) environment)))
	      ((or (and (or (member :compile-toplevel situations)
			    (member 'compile situations))
			(not (or (member :load-toplevel situations)
				 (member 'load situations))))
		   (and (not (or (member :compile-toplevel situations)
				 (member 'compile situations)))
			(not (or (member :load-toplevel situations)
				 (member 'load situations)))
			(or (member :execute situations)
			    (member 'eval situations))
			*compile-time-too*))
	       (cleavir-env:eval `(progn ,@body) environment environment)
	       (convert nil environment))
	      (t
	       (convert nil environment))))))

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
	(let ((canonicalized-dspecs
		(cleavir-code-utilities:canonicalize-declaration-specifiers
		 (reduce #'append (mapcar #'cdr declarations)))))
	  (setf new-env (augment-environment-with-declarations
			 new-env canonicalized-dspecs)))
	(cleavir-ast:make-progn-ast
	 (append init-asts (convert-sequence forms new-env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.

(defgeneric convert-function (info))

(defmethod convert-function ((info cleavir-env:global-function-info))
  (let ((name (cleavir-env:name info)))
    (make-instance 'cleavir-ast:fdefinition-ast
      :name name :info info)))

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
	(let ((canonicalized-dspecs
		(cleavir-code-utilities:canonicalize-declaration-specifiers
		 (reduce #'append (mapcar #'cdr declarations)))))
	  (setf new-env (augment-environment-with-declarations
			 new-env canonicalized-dspecs)))
	(cleavir-ast:make-progn-ast
	 (append init-asts (convert-sequence forms new-env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET and LET*

(defun binding-init-form (binding)
  (if (or (symbolp binding) (null (cdr binding)))
      nil
      (cadr binding)))

(defun binding-var (binding)
  (if (symbolp binding)
      binding
      (first binding)))

;;; For converting LET, we use a method that is very close to the
;;; exact wording of the Common Lisp HyperSpec, in that we first
;;; evaluate the INIT-FORMs in the original environment and save the
;;; resulting values.  We save those resulting values in
;;; freshly-created lexical variables.  Then we bind each original
;;; variable, either by using a SETQ-AST or a BIND-AST according to
;;; whether the variable to be bound is lexical or special.

;;; BINDINGS is a list of CONS cells.  The CAR of each CONS cell is a
;;; variable to be bound.  The CDR of each CONS cell is a LEXICAL-AST
;;; that corresponds to a lexical variable holding the result of the
;;; computation of the initial value for that variable.  IDSPECS is a
;;; list with the same length as BINDINGS of itemized canonicalized
;;; declaration specifiers.  Each item in the list is a list of
;;; canonicalized declaration specifiers associated with the
;;; corresponding variable in the BINDINGS list.  RDSPECS is a list of
;;; remaining canonicalized declaration specifiers that apply to the
;;; environment in which the FORMS are to be processed.
(defun process-remaining-let-bindings (bindings idspecs rdspecs forms env)
  (if (null bindings)
      ;; We ran out of bindings.  We must build an AST for the body of
      ;; the function.
      (let ((new-env (augment-environment-with-declarations env rdspecs)))
	(cleavir-ast:make-progn-ast (convert-sequence forms new-env)))
      (destructuring-bind (var . lexical-ast) (first bindings)
	(let* (;; We enter the new variable into the environment and
	       ;; then we process remaining parameters and ultimately
	       ;; the body of the function.
	       (new-env (augment-environment-with-variable
			 var (first idspecs) env env))
	       ;; We compute the AST of the remaining computation by
	       ;; recursively calling this same function with the
	       ;; remaining bindings (if any) and the environment that
	       ;; we obtained by augmenting the original one with the
	       ;; parameter variable.
	       (next-ast (process-remaining-let-bindings (rest bindings)
							 (rest idspecs)
							 rdspecs
							 forms
							 new-env)))
	  ;; All that is left to do now, is to construct the AST to
	  ;; return by using the new variable and the AST of the
	  ;; remaining computation as components.
	  (set-or-bind-variable var lexical-ast next-ast new-env)))))

(defmethod convert-special
    ((symbol (eql 'let)) form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      (let* ((canonical-dspecs
	       (cleavir-code-utilities:canonicalize-declaration-specifiers 
		(reduce #'append (mapcar #'cdr declarations))))
	     (variables (mapcar #'binding-var bindings))
	     (temp-asts (loop for var in variables
			      collect (cleavir-ast:make-lexical-ast (gensym))))
	     (init-asts (loop for binding in bindings
			      for init-form = (binding-init-form binding)
			      for temp-ast in temp-asts
			      collect (cleavir-ast:make-setq-ast
				       temp-ast (convert init-form  env)))))
	(multiple-value-bind (idspecs rdspecs)
	    (itemize-declaration-specifiers
	     (mapcar #'list variables)
	     canonical-dspecs)
	  (cleavir-ast:make-progn-ast
	   (append init-asts
		   (list (process-remaining-let-bindings
			  (mapcar #'cons variables temp-asts)
			  idspecs
			  rdspecs
			  forms
			  env)))))))))

;;; BINDINGS is a list of CONS cells.  The CAR of each CONS cell is a
;;; variable to be bound.  The CDR of each CONS cell is an init-form
;;; computing the initial value for that variable.  IDSPECS is a list
;;; with the same length as BINDINGS of itemized canonicalized
;;; declaration specifiers.  Each item in the list is a list of
;;; canonicalized declaration specifiers associated with the
;;; corresponding variable in the BINDINGS list.  RDSPECS is a list of
;;; remaining canonicalized declaration specifiers that apply to the
;;; environment in which the FORMS are to be processed.
(defun process-remaining-let*-bindings (bindings idspecs rdspecs forms env)
  (if (null bindings)
      ;; We ran out of bindings.  We must build an AST for the body of
      ;; the function.
      (let ((new-env (augment-environment-with-declarations env rdspecs)))
	(cleavir-ast:make-progn-ast (convert-sequence forms new-env)))
      (destructuring-bind (var . init-form) (first bindings)
	(let* (;; We enter the new variable into the environment and
	       ;; then we process remaining parameters and ultimately
	       ;; the body of the function.
	       (new-env (augment-environment-with-variable
			 var (first idspecs) env env))
	       ;; The initform of the &AUX parameter is turned into an
	       ;; AST in the original environment, i.e. the one that
	       ;; does not have the parameter variable in it.
	       (value-ast (convert init-form env))
	       ;; We compute the AST of the remaining computation by
	       ;; recursively calling this same function with the
	       ;; remaining bindings (if any) and the environment that
	       ;; we obtained by augmenting the original one with the
	       ;; parameter variable.
	       (next-ast (process-remaining-let*-bindings (rest bindings)
							  (rest idspecs)
							  rdspecs
							  forms
							  new-env)))
	  ;; All that is left to do now, is to construct the AST to
	  ;; return by using the new variable and the AST of the
	  ;; remaining computation as components.
	  (set-or-bind-variable var value-ast next-ast new-env)))))

(defmethod convert-special
    ((symbol (eql 'let*)) form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      (let* ((canonical-dspecs
	       (cleavir-code-utilities:canonicalize-declaration-specifiers 
		(reduce #'append (mapcar #'cdr declarations))))
	     (variables (mapcar #'binding-var bindings))
	     (init-forms (mapcar #'binding-init-form bindings)))
	(multiple-value-bind (idspecs rdspecs)
	    (itemize-declaration-specifiers
	     (mapcar #'list variables)
	     canonical-dspecs)
	  (process-remaining-let*-bindings (mapcar #'cons variables init-forms)
					   idspecs
					   rdspecs
					   forms
					   env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(defmethod convert-special
    ((symbol (eql 'load-time-value)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 2)
  (cleavir-ast:make-load-time-value-ast
   (cadr form) (caddr form)))

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
    (let ((canonicalized-dspecs
	    (cleavir-code-utilities:canonicalize-declaration-specifiers
	     (reduce #'append (mapcar #'cdr declarations)))))
      (let ((new-env (augment-environment-with-declarations
		      env canonicalized-dspecs)))
	;; The following expression means "if this form is a top-level
	;; form, then make sure that forms compiled by a recursive
	;; call to CONVERT are also top-level forms".
	(setf *top-level-form-p* *old-top-level-form-p*)
	(cleavir-ast:make-progn-ast
	 (convert-sequence forms new-env))))))

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
	    (cleavir-env:name info)
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
