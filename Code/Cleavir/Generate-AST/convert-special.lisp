(in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    ((symbol (eql 'quote)) form env system)
  (db s (quote const) form
    (declare (ignore quote))
    (convert-constant const env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special
    ((symbol (eql 'block)) form env system)
  (db s (block name . body) form
    (declare (ignore block))
    (let* ((ast (cleavir-ast:make-block-ast nil))
	   (new-env (cleavir-env:add-block env name ast)))
      (setf (cleavir-ast:body-ast ast)
	    (process-progn (convert-sequence body new-env system)))
      ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defmethod convert-special
    ((symbol (eql 'eval-when)) form environment system)
  (with-preserved-toplevel-ness
    (db s (eval-when situations . body) form
      (declare (ignore eval-when))
      (if (or (eq *compiler* 'cl:compile)
	      (eq *compiler* 'cl:eval)
	      (not *current-form-is-top-level-p*))
	  (if (or (member :execute situations)
		  (member 'eval situations))
	      (process-progn
	       (convert-sequence body environment system))
	      (convert nil environment system))
	  (cond ((or
		  ;; CT   LT   E    Mode
		  ;; Yes  Yes  ---  ---
		  (and (or (member :compile-toplevel situations)
			   (member 'compile situations))
		       (or (member :load-toplevel situations)
			   (member 'load situations)))
		  ;; CT   LT   E    Mode
		  ;; No   Yes  Yes  CTT
		  (and (not (or (member :compile-toplevel situations)
				(member 'compile situations)))
		       (or (member :load-toplevel situations)
			   (member 'load situations))
		       (or (member :execute situations)
			   (member 'eval situations))
		       *compile-time-too*))
		 (let ((*compile-time-too* t))
		   (convert `(progn ,@body) environment system)))
		((or
		  ;; CT   LT   E    Mode
		  ;; No   Yes  Yes  NCT
		  (and (not (or (member :compile-toplevel situations)
				(member 'compile situations)))
		       (or (member :load-toplevel situations)
			   (member 'load situations))
		       (or (member :execute situations)
			   (member 'eval situations))
		       (not *compile-time-too*))
		  ;; CT   LT   E    Mode
		  ;; No   Yes  No   ---
		  (and (not (or (member :compile-toplevel situations)
				(member 'compile situations)))
		       (or (member :load-toplevel situations)
			   (member 'load situations))
		       (not (or (member :execute situations)
				(member 'eval situations)))))
		 (let ((*compile-time-too* nil))
		   (convert `(progn ,@body) environment system)))
		((or
		  ;; CT   LT   E    Mode
		  ;; Yes  No   ---  ---
		  (and (or (member :compile-toplevel situations)
			   (member 'compile situations))
		       (not (or (member :load-toplevel situations)
				(member 'load situations))))
		  ;; CT   LT   E    Mode
		  ;; No   No   Yes  CTT
		  (and (not (or (member :compile-toplevel situations)
				(member 'compile situations)))
		       (not (or (member :load-toplevel situations)
				(member 'load situations)))
		       (or (member :execute situations)
			   (member 'eval situations))
		       *compile-time-too*))
		 (cleavir-env:eval `(progn ,@body) environment environment)
		 (convert nil environment system))
		(t
		 (convert nil environment system)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

;;; Take an environment and a single function definition, and return a
;;; new environment which is like the one passed as an argument except
;;; that it has been augmented by the name of the local function.
(defun augment-environment-from-fdef (environment definition)
  (let* ((name (dfirst definition))
	 (var-ast (cleavir-ast:make-lexical-ast name)))
    (cleavir-env:add-local-function environment name var-ast)))

;;; Take an environment and a list of function definitions, and return
;;; a new environment which is like the one passed as an argument,
;;; except that is has been augmented by the names of the local
;;; functions in the list.
(defun augment-environment-from-fdefs (environment definitions)
  (loop with result = environment
	for defs = definitions then (drest defs)
	until (null defs)
	do (setf result
		 (augment-environment-from-fdef result (dfirst defs)))
	finally (return result)))

;;; Given an environment and the name of a function, return the
;;; LEXICAL-AST that will have the function with that name as a value.
;;; It is known that the environment contains an entry corresponding
;;; to the name given as an argument.
(defun function-lexical (environment name)
  (cleavir-env:identity (cleavir-env:function-info environment name)))

;;; Convert a list of local function definitions.
(defun convert-local-functions (definitions environment system)
  (loop for (name lambda-list . body) in definitions
	for block-name = (if (symbolp name) name (second name))
	collect (convert-code lambda-list body environment system block-name)))

;;; Given a list of local function definitions, return a list of
;;; the names of those functions.
(defun function-names (definitions)
  (mapcar #'car definitions))

(defmethod convert-special
    ((symbol (eql 'flet)) form env system)
  (db s (flet definitions . body) form
    (declare (ignore flet))
    (let ((new-env (augment-environment-from-fdefs env definitions)))
      (let* ((funs (convert-local-functions definitions env system))
	     (init-asts
	       (loop for fun in funs
		     for name in (function-names definitions)
		     collect (cleavir-ast:make-setq-ast
			      (function-lexical new-env name)
			      fun))))
	(multiple-value-bind (declarations forms)
	    (cleavir-code-utilities:separate-ordinary-body body)
	  (let ((canonicalized-dspecs
		  (cleavir-code-utilities:canonicalize-declaration-specifiers
		   (reduce #'append (mapcar #'cdr declarations)))))
	    (setf new-env (augment-environment-with-declarations
			   new-env canonicalized-dspecs)))
	  (process-progn
	   (append init-asts (convert-sequence forms new-env system))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.

(defgeneric convert-global-function (info global-env system))

(defmethod convert-global-function (info global-env system)
  (declare (ignore global-env))
  (cleavir-ast:make-fdefinition-ast
   (cleavir-ast:make-load-time-value-ast `',(cleavir-env:name info) t)
   info))

(defmethod convert-function
    ((info cleavir-env:global-function-info) env system)
  (convert-global-function info (cleavir-env:global-environment env) system))

(defmethod convert-function
    ((info cleavir-env:local-function-info) env system)
  (declare (ignore env system))
  (cleavir-env:identity info))

(defmethod convert-function
    ((info cleavir-env:global-macro-info) env system)
  (error 'function-name-names-global-macro
	 :expr (cleavir-env:name info)))

(defmethod convert-function
    ((info cleavir-env:local-macro-info) env system)
  (error 'function-name-names-local-macro
	 :expr (cleavir-env:name info)))

(defmethod convert-function
    ((info cleavir-env:special-operator-info) env system)
  (error 'function-name-names-special-operator
	 :expr (cleavir-env:name info)))

(defun convert-named-function (name environment system)
  (let ((info (function-info environment name)))
    (convert-function info environment system)))

(defun convert-lambda-function (lambda-form env system)
  (convert-code (cadr lambda-form) (cddr lambda-form) env system))

(defmethod convert-special ((symbol (eql 'function)) form env system)
  (db s (function name) form
    (declare (ignore function))
    (if (proper-function-name-p name)
	(convert-named-function name env system)
	(convert-lambda-function name env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-special ((symbol (eql 'go)) form env system)
  (declare (ignore system))
  (db s (go tag) form
    (declare (ignore go))
    (let ((info (tag-info env (raw tag))))
      (cleavir-ast:make-go-ast
       (cleavir-env:identity info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-special ((symbol (eql 'if)) form env system)
  (db s (if test then . tail) form
    (declare (ignore if))
    (let ((test-ast (convert test env system))
	  (true-ast (convert then env system))
	  (false-ast (if (null tail)
			 (convert-constant nil env system)
			 (db s (else) tail
			   (convert else env system)))))
      (if (typep test-ast 'cleavir-ast:boolean-ast-mixin)
	  (cleavir-ast:make-if-ast
	   test-ast
	   true-ast
	   false-ast)
	  (cleavir-ast:make-if-ast
	   (cleavir-ast:make-eq-ast test-ast (convert-constant nil env system))
	   false-ast
	   true-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LABELS.

(defmethod convert-special ((symbol (eql 'labels)) form env system)
  (db s (labels definitions . body) form
    (declare (ignore labels))
    (let ((new-env (augment-environment-from-fdefs env definitions)))
      (let ((init-asts
	      (loop for (name lambda-list . body) in definitions
		    for block-name = (if (symbolp name) name (second name))
		    for fun = (convert-code lambda-list body new-env system block-name)
		    collect (cleavir-ast:make-setq-ast
			     (function-lexical new-env name)
			     fun))))
	(multiple-value-bind (declarations forms)
	    (cleavir-code-utilities:separate-ordinary-body body)
	  (let ((canonicalized-dspecs
		  (cleavir-code-utilities:canonicalize-declaration-specifiers
		   (reduce #'append (mapcar #'cdr declarations)))))
	    (setf new-env (augment-environment-with-declarations
			   new-env canonicalized-dspecs)))
	  (process-progn
	   (append init-asts (convert-sequence forms new-env system))))))))

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
(defun process-remaining-let-bindings (bindings idspecs rdspecs forms env system)
  (if (null bindings)
      ;; We ran out of bindings.  We must build an AST for the body of
      ;; the function.
      (let ((new-env (augment-environment-with-declarations env rdspecs)))
	(process-progn (convert-sequence forms new-env system)))
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
							 new-env
							 system)))
	  ;; All that is left to do now, is to construct the AST to
	  ;; return by using the new variable and the AST of the
	  ;; remaining computation as components.
	  (set-or-bind-variable var lexical-ast next-ast new-env system)))))

(defmethod convert-special
    ((symbol (eql 'let)) form env system)
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
				       temp-ast
				       (convert init-form env system)))))
	(multiple-value-bind (idspecs rdspecs)
	    (itemize-declaration-specifiers
	     (mapcar #'list variables)
	     canonical-dspecs)
	  (process-progn
	   (append init-asts
		   (list (process-remaining-let-bindings
			  (mapcar #'cons variables temp-asts)
			  idspecs
			  rdspecs
			  forms
			  env
			  system)))))))))

;;; BINDINGS is a list of CONS cells.  The CAR of each CONS cell is a
;;; variable to be bound.  The CDR of each CONS cell is an init-form
;;; computing the initial value for that variable.  IDSPECS is a list
;;; with the same length as BINDINGS of itemized canonicalized
;;; declaration specifiers.  Each item in the list is a list of
;;; canonicalized declaration specifiers associated with the
;;; corresponding variable in the BINDINGS list.  RDSPECS is a list of
;;; remaining canonicalized declaration specifiers that apply to the
;;; environment in which the FORMS are to be processed.
(defun process-remaining-let*-bindings
    (bindings idspecs rdspecs forms env system)
  (if (null bindings)
      ;; We ran out of bindings.  We must build an AST for the body of
      ;; the function.
      (let ((new-env (augment-environment-with-declarations env rdspecs)))
	(process-progn (convert-sequence forms new-env system)))
      (destructuring-bind (var . init-form) (first bindings)
	(let* (;; We enter the new variable into the environment and
	       ;; then we process remaining parameters and ultimately
	       ;; the body of the function.
	       (new-env (augment-environment-with-variable
			 var (first idspecs) env env))
	       ;; The initform of the &AUX parameter is turned into an
	       ;; AST in the original environment, i.e. the one that
	       ;; does not have the parameter variable in it.
	       (value-ast (convert init-form env system))
	       ;; We compute the AST of the remaining computation by
	       ;; recursively calling this same function with the
	       ;; remaining bindings (if any) and the environment that
	       ;; we obtained by augmenting the original one with the
	       ;; parameter variable.
	       (next-ast (process-remaining-let*-bindings (rest bindings)
							  (rest idspecs)
							  rdspecs
							  forms
							  new-env
							  system)))
	  ;; All that is left to do now, is to construct the AST to
	  ;; return by using the new variable and the AST of the
	  ;; remaining computation as components.
	  (set-or-bind-variable var value-ast next-ast new-env system)))))

(defmethod convert-special
    ((symbol (eql 'let*)) form env system)
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
					   env
					   system))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(defmethod convert-special
    ((symbol (eql 'load-time-value)) form environment system)
  (declare (ignore system))
  (db s (load-time-value form . remaining) form
    (declare (ignore load-time-value))
    (cleavir-ast:make-load-time-value-ast
     form
     (if (null remaining)
	 nil
	 (db s (read-only-p) remaining
	   (raw read-only-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, LOCALLY processes
;;; its subforms the same way as the form itself.

(defmethod convert-special
    ((symbol (eql 'locally)) form env system)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body (cdr form))
    (let ((canonicalized-dspecs
	    (cleavir-code-utilities:canonicalize-declaration-specifiers
	     (reduce #'append (mapcar #'cdr declarations)))))
      (let ((new-env (augment-environment-with-declarations
		      env canonicalized-dspecs)))
	(with-preserved-toplevel-ness
	  (process-progn
	   (convert-sequence forms new-env system)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;;; its subforms the same way as the form itself.

(defmethod convert-special
    ((symbol (eql 'macrolet)) form env system)
  (destructuring-bind (definitions &rest body) (rest form)
    (let ((new-env env))
      (loop for (name lambda-list . body) in definitions
	    for lambda-expr = (cleavir-code-utilities:parse-macro
			        name lambda-list body env)
	    for expander = (cleavir-env:eval lambda-expr env env)
	    do (setf new-env
		     (cleavir-env:add-local-macro new-env name expander)))
      (with-preserved-toplevel-ness
	(convert `(locally ,@body) new-env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, PROGN processes
;;; its subforms the same way as the form itself.

(defmethod convert-special
    ((head (eql 'progn)) form environment system)
  (with-preserved-toplevel-ness
    (db s (progn . forms) form
      (declare (ignore progn))
      (process-progn
       (convert-sequence forms environment system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special
    ((symbol (eql 'return-from)) form env system)
  (let ((info (block-info env (cadr form))))
    (cleavir-ast:make-return-from-ast
     (cleavir-env:identity info)
     (convert (caddr form) env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;
;;; Recall that the SETQ-AST is a NO-VALUE-AST-MIXIN.  We must
;;; therefore make sure it is always compiled in a context where its
;;; value is not needed.  We do that by wrapping a PROGN around it.

(defgeneric convert-setq (info form env system))

(defmethod convert-setq
    ((info cleavir-env:constant-variable-info) form env system)
  (declare (ignore env system form))
  (error 'setq-constant-variable
	 :form (cleavir-env:name info)))

(defmethod convert-setq
    ((info cleavir-env:lexical-variable-info) form env system)
  (process-progn 
   (list (cleavir-ast:make-setq-ast
	  (cleavir-env:identity info)
	  (convert form env system))
	 (cleavir-env:identity info))))

(defmethod convert-setq
    ((info cleavir-env:symbol-macro-info) form env system)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (lambda (form env)
			      (declare (ignore form env))
			      (cleavir-env:expansion info))
			    (cleavir-env:name info)
			    env)))
    (convert `(setf ,expansion ,form) env system)))

(defgeneric convert-setq-special-variable
    (info form-ast global-env system))

(defmethod convert-setq-special-variable
    (info form-ast global-env system)
  (declare (ignore system))
  (let ((temp (cleavir-ast:make-lexical-ast (gensym))))
    (process-progn
     (list (cleavir-ast:make-setq-ast temp form-ast)
	   (cleavir-ast:make-set-symbol-value-ast
	    (cleavir-ast:make-load-time-value-ast `',(cleavir-env:name info))
	    temp)
	   temp))))

(defmethod convert-setq
    ((info cleavir-env:special-variable-info) form env system)
  (let ((global-env (cleavir-env:global-environment env)))
    (convert-setq-special-variable info
				   (convert form env system)
				   global-env
				   system)))

(defun convert-elementary-setq (var form env system)
  (convert-setq (variable-info env var)
		form
		env
		system))
  
(defmethod convert-special
    ((symbol (eql 'setq)) form environment system)
  (let ((form-asts (loop for (var form) on (cdr form) by #'cddr
			 collect (convert-elementary-setq
				  var form environment system))))
    (process-progn form-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-special
    ((head (eql 'symbol-macrolet)) form env system)
  (let ((new-env env))
    (loop for (name expansion) in (cadr form)
	  do (setf new-env
		   (cleavir-env:add-local-symbol-macro new-env name expansion)))
    (with-preserved-toplevel-ness
      (convert `(progn ,@(cddr form)) new-env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.
;;;
;;; The TAGBODY special form always returns NIL.  We generate a PROGN
;;; with the TAGBODY-AST and a CONSTANT-AST in it, because the
;;; TAGBODY-AST (unlike hte TAGBODY special form) does not generate a
;;; value.

(defmethod convert-special
    ((symbol (eql 'tagbody)) form env system)
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
				   (convert item new-env system)))))
      (process-progn
       (list (cleavir-ast:make-tagbody-ast items)
	     (convert-constant nil env system))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.

(defmethod convert-special
    ((symbol (eql 'the)) form environment system)
  (destructuring-bind (value-type subform) (rest form)
    (cleavir-ast:make-the-ast
     (convert subform environment system)
     (if (and (consp value-type) (eq (car value-type) 'values))
	 (cdr value-type)
	 (list value-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.

(defmethod convert-special
    ((symbol (eql 'multiple-value-call)) form environment system)
  (destructuring-bind (function-form . forms) (rest form)
    (cleavir-ast:make-multiple-value-call-ast
     (convert function-form environment system)
     (convert-sequence forms environment system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.

(defmethod convert-special
    ((symbol (eql 'multiple-value-prog1)) form environment system)
  (destructuring-bind (first-form . forms) (rest form)
    (cleavir-ast:make-multiple-value-prog1-ast
     (convert first-form environment system)
     (convert-sequence forms environment system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized to operators for which we do not provide a
;;; conversion method.

(defmethod convert-special
    ((symbol (eql 'unwind-protect)) form environment system)
  (declare (ignore form environment system))
  (error 'no-default-method :operator symbol))

(defmethod convert-special
    ((symbol (eql 'catch)) form environment system)
  (declare (ignore form environment system))
  (error 'no-default-method :operator symbol))

(defmethod convert-special
    ((symbol (eql 'throw)) form environment system)
  (declare (ignore form environment system))
  (error 'no-default-method :operator symbol))

(defmethod convert-special
    ((symbol (eql 'progv)) form environment system)
  (declare (ignore form environment system))
  (error 'no-default-method :operator symbol))
