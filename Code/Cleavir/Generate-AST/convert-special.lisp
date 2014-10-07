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
  (if (not (null (intersection '(:execute eval) (second form))))
      (cleavir-ast:make-progn-ast
       (convert-sequence (cddr form) environment))
      (convert 'nil environment)))

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

(defun convert-named-function (name environment)
  (let ((info (cleavir-env:function-info environment name)))
    (make-instance 'cleavir-ast:fdefinition-ast
      :name (cleavir-env:name info))))

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
  (let ((info (cleavir-env:tag-info env (cadr form))))
    (if (null info)
	(error "undefined go tag: ~s" form)
	(cleavir-ast:make-go-ast
	 (cleavir-env:identity info)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET and LET*

(defun init-form (binding)
  (if (or (symbolp binding) (null (cdr binding)))
      nil
      (cadr binding)))

(defun convert-simple-let (binding body env)
  (let* ((var (if (symbolp binding) binding (car binding)))
	 (var-ast (cleavir-ast:make-lexical-ast var))
	 (init-form (if (symbolp binding) nil (cadr binding)))
	 (new-env (cleavir-env:add-lexical-variable env var var-ast))
	 (info (cleavir-env:variable-info new-env var)))
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      ;; FIXME: handle declarations
      ;; FIXME: in particular, if there is a SPECIAL declaration
      ;; then generate totally different code. 
      (declare (ignore declarations))
      (cleavir-ast:make-progn-ast
       (cons (cleavir-ast:make-setq-ast
	      var-ast
	      (convert init-form env))
	     (convert-sequence forms new-env))))))

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

;;; We convert a LET form recursively.  If it has a single binding, we
;;; convert it into a SETQ.  If it has more than one binding, we
;;; convert it as follows:
;;;
;;; (let ((<var> <init-form>)
;;;       <more-bindings>)
;;;   <body>)
;;; =>
;;; (let ((temp <init-form>))
;;;   (let (<more-bindings>)
;;;     (let ((<var> temp))
;;;       <body>)))

(defmethod convert-special
    ((symbol (eql 'let)) form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (if (= (length bindings) 1)
	(convert-simple-let (car bindings) body env)
	(let* ((first (car bindings))
	       (var (if (symbolp first) first (car first)))
	       (init-form (if (symbolp first) nil (cadr first)))
	       (temp (gensym)))
	  (multiple-value-bind (declarations forms)
	      (cleavir-code-utilities:separate-ordinary-body body)
	    (multiple-value-bind (first remaining)
		(separate-declarations 
		 (cleavir-code-utilities:canonicalize-declaration-specifiers 
		  (mapcar #'cdr declarations))
		 var)
	      (convert
	       `(let ((,temp ,init-form))
		  (let ,(cdr bindings)
		    (declare ,@remaining)
		    (let ((,var ,temp))
		      (declare ,@first)
		      ,@forms)))
	       env)))))))

;;; Given a list of canonicalized declaration specifiers for a single
;;; varible.  Return a type specifier resulting from all the type
;;; declarations present in the list.
(defun declared-type (declarations)
  `(and ,@(loop for declaration in declarations
		when (eq (car declaration) 'type)
		  collect (cadr declaration))))

;;; Given a single variable bound by a LET form, a list of
;;; canonicalized declaration specifiers, and an environment in which
;;; the LET form is compiled, return true if and only if the variable
;;; to be bound is special.  Return a second value indicating whether
;;; the variable is globally special.
;;;
;;; FIXME: also return information indicating whether the variable is
;;; globally special.
(defun variable-is-special-p (variable declarations env)
  (let ((existing-var-info (cleavir-env:variable-info env variable)))
    (cond ((and (and (not (null existing-var-info))
		     (typep existing-var-info
			    'cleavir-env:special-variable-info))
		(cleavir-env:global-p existing-var-info))
	   ;; If it is globally special, it is special inside the LET
	   ;; or LET* as well, no matter how it is declared.
	   (values t t))
	  ((member 'special declarations :key #'car)
	   ;; If it is not globally special, it is special only if it
	   ;; is declared special.
	   (values t nil))
	  (t
	   (values nil nil)))))

;;; Given a single variable bound by the LET form, and a list of
;;; canonicalized declaration specifiers concerning that variable,
;;; return a new environment that contains information about that
;;; variable.  
(defun augment-environment-with-variable (variable declarations env)
  (let ((new-env env))
    (multiple-value-bind (special-p globally-p)
	(variable-is-special-p variable declarations env)
      (if (and special-p (not globally-p))
	  (setf new-env
		(cleavir-env:add-special-variable new-env variable))
	  (let ((var-ast (cleavir-ast:make-lexical-ast variable)))
	    (setf new-env
		  (cleavir-env:add-lexical-variable new-env variable var-ast)))))
    (let ((type (declared-type declarations)))
      (unless (equal type '(and))
	(setf new-env
	      (cleavir-env:add-variable-type new-env variable type))))
    (when (member 'dynamic-extent declarations :test #'eq :key #'car)
      (setf new-env
	    (cleavir-env:add-variable-dynamic-extent new-env 'variable)))
    new-env))

;;; Given a list of variables bound by the LET form, and a list of
;;; canonicalized declarations specifiers, return an environment to be
;;; used to compile the body.  The ENV parameter is the environment in
;;; which the LET form is converted.  It is used in order to determine
;;; whether any variable is globally special. 
(defun construct-let-body-environment (variables declarations env)
  (flet ((augment (env variable)
	   (augment-environment-with-variable variable declarations env)))
    (loop for variable in variables
	  for new-env = env then (augment new-env variable)
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
	     (ast (convert-sequence forms body-env)))
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
	       (flet ((augment (env variable)
			(augment-environment-with-variable
			 variable canonical-declarations env)))
		 (loop for variable in variables
		       for new-env = (augment env variable)
			 then (augment new-env variable)
		       collect new-env)))
	     (ast (convert-sequence forms (first (last environments)))))
	(loop for binding in (reverse bindings)
	      for var in (reverse variables)
	      for environment in (rest (reverse (cons env environments)))
	      for info = (cleavir-env:variable-info environment var)
	      for init = (if (symbolp binding) nil (second binding))
	      for init-ast = (convert init environment)
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

(defmethod convert-special
    ((symbol (eql 'locally)) form env)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body (cdr form))
    (let ((new-env (augment-environment-with-declarations
		    env declarations)))
      (cleavir-ast:make-progn-ast
       (convert-sequence forms new-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.

;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;; its subforms the same way as the form itself.

(defmethod convert-special
    ((symbol (eql 'macrolet)) form env)
  (destructuring-bind (macrolet definitions &rest body) form
    (declare (ignore macrolet))
    (let ((new-env env))
      (loop for definition in definitions
	    for name = (first definition)
	    for expander = (minimally-compile-macro-definition definition env)
	    do (setf new-env
		     (cleavir-env:add-local-macro new-env name expander)))
      (convert-special 'locally `(locally ,@body) new-env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.

(defmethod convert-special
    ((head (eql 'progn)) form environment)
  (cleavir-ast:make-progn-ast
   (convert-sequence (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special ((symbol (eql 'return-from)) form env)
  (let ((info (cleavir-env:block-info env (cadr form))))
    (if (null info)
	(error 'block-name-unknown
	       :expr (cadr form))
	(cleavir-ast:make-return-from-ast
	 (cleavir-env:identity info)
	 (convert (caddr form) env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defun convert-elementary-setq (var form env)
  (let* ((info (cleavir-env:variable-info env var))
	 (identity (cleavir-env:identity info)))
    (if (typep info 'cleavir-env:constant-variable-info)
	(error 'setq-constant-variable
	       :form var)
	(cleavir-ast:make-setq-ast
	 identity
	 (convert form env)))))
  
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
    (convert `(progn ,@(cddr form)) new-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.

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
      (cleavir-ast:make-tagbody-ast items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.

(defmethod convert-special
    ((symbol (eql 'the)) form environment)
  (apply #'cleavir-ast:make-the-ast
	 (convert (caddr form) environment)
	 (mapcar #'cleavir-ast:make-constant-ast
		 (if (and (consp (cadr form)) (eq (car (cadr form)) 'values))
		     (cdr (cadr form))
		     (list (cadr form))))))
