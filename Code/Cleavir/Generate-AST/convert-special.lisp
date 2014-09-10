(in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    ((symbol (eql 'quote)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1)
  (cleavir-ast:make-constant-ast (cadr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special
    ((symbol (eql 'block)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (unless (symbolp (cadr form))
    (error 'block-name-must-be-a-symbol
	   :expr (cadr form)))
  (let* ((ast (cleavir-ast:make-block-ast nil))
	 (new-env (cleavir-env:add-block env (cadr form)))
	 (forms (convert-sequence (cddr form) new-env)))
    (setf (cleavir-ast:body-ast ast)
	  (cleavir-ast:make-progn-ast forms))
    ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defmethod convert-special
    ((symbol (eql 'eval-when)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (unless (cleavir-code-utilities:proper-list-p (second form))
    (error 'situations-must-be-proper-list
	   :expr (second form)))
  ;; Check each situation
  (loop for situation in (second form)
	do (unless (and (symbolp situation)
			(member situation
				'(:compile-toplevel :load-toplevel :execute
				  compile load eval)))
	     ;; FIXME: perhaps we should warn about the deprecated situations
	     (error 'invalid-eval-when-situation
		    :expr situation)))
  (if (not (null (intersection '(:execute eval) (second form))))
      (cleavir-ast:make-progn-ast
       (convert-sequence (cddr form) environment))
      (convert 'nil environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

(defmethod convert-special ((symbol (eql 'flet)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (unless (cleavir-code-utilities:proper-list-p (cadr form))
    (error 'flet-functions-must-be-proper-list
	   :expr form))
  (let ((new-env env))
    ;; Create a new environment with the additional names.
    (loop for def in (cadr form)
	  for name = (car def)
	  do (setf new-env (cleavir-env:add-local-function new-env name)))
    (let ((init-asts
	    (loop for (name lambda-list . body) in (cadr form)
		  for fun = (convert-code lambda-list body env)
		  collect (cleavir-ast:make-setq-ast
			   (let* ((info (cleavir-env:function-info name new-env))
				  (identity (cleavir-env:identity info)))
			     (find-or-create-ast identity))
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
  (let* ((info (cleavir-env:function-info name environment))
	 (identity (cleavir-env:identity info)))
    (find-or-create-ast identity)))

(defun convert-lambda-function (lambda-form env)
  (unless (cleavir-code-utilities:proper-list-p lambda-form)
    (error 'lambda-must-be-proper-list
	   :expr lambda-form))
  (convert-code (cadr lambda-form) (cddr lambda-form) env))

(defun proper-function-name-p (name)
  (or (symbolp name)
      (and (cleavir-code-utilities:proper-list-p name)
	   (= (length name) 2)
	   (eq (car name) 'setf)
	   (symbolp (cadr name)))))

(defmethod convert-special ((symbol (eql 'function)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1)
  (unless (or (proper-function-name-p (cadr form))
	      (and (consp (cadr form))
		   (eq (car (cadr form)) 'lambda)))
    (error 'function-argument-must-be-function-name-or-lambda-expression
	   :expr (cadr form)))
  (if (proper-function-name-p (cadr form))
      (convert-named-function (cadr form) env)
      (convert-lambda-function (cadr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-special ((symbol (eql 'go)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 1)
  (let ((info (cleavir-env:tag-info (cadr form) env)))
    (if (null info)
	(error "undefined go tag: ~s" form)
	(cleavir-ast:make-go-ast
	 (cleavir-env:identity info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-special ((symbol (eql 'if)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 3)
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

(defun check-binding-forms (binding-forms)
  (unless (cleavir-code-utilities:proper-list-p binding-forms)
    (error 'bindings-must-be-proper-list :expr binding-forms))
  (loop for binding-form in binding-forms
	do (unless (or (symbolp binding-form) (consp binding-form))
	     (error 'binding-must-be-symbol-or-list
		    :expr binding-form))
	   (when (and (consp binding-form)
		      (or (not (listp (cdr binding-form)))
			  (not (null (cddr binding-form)))))
	     (error 'binding-must-have-length-one-or-two
		    :expr binding-form))
	   (when (and (consp binding-form)
		      (not (symbolp (car binding-form))))
	     (error 'variable-must-be-a-symbol
		    :expr (car binding-form)))))

(defun init-form (binding)
  (if (or (symbolp binding) (null (cdr binding)))
      nil
      (cadr binding)))

(defun convert-simple-let (binding body env)
  (let* ((var (if (symbolp binding) binding (car binding)))
	 (init-form (if (symbolp binding) nil (cadr binding)))
	 (new-env (cleavir-env:add-lexical-variable env var))
	 (info (cleavir-env:variable-info var new-env))
	 (identity (cleavir-env:identity info)))
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      ;; FIXME: handle declarations
      ;; FIXME: in particular, if there is a SPECIAL declaration
      ;; then generate totally different code. 
      (declare (ignore declarations))
      (cleavir-ast:make-progn-ast
       (cons (cleavir-ast:make-setq-ast
	      (find-or-create-ast identity)
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
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (destructuring-bind (bindings &rest body) (cdr form)
    (check-binding-forms bindings)
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

;;; We convert a LET* form recursively.  If it has a single binding,
;;; we convert it into a SETQ.  If it has more than one binding, we
;;; convert it as follows:
;;;
;;; (let* ((<var> <init-form>)
;;;        <more-bindings>)
;;;   <body>)
;;; =>
;;; (let ((<var> <init-form>))
;;;   (let (<more-bindings>)
;;;      <body>)))

(defmethod convert-special
    ((symbol (eql 'let*)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  (destructuring-bind (bindings &rest body) (cdr form)
    (check-binding-forms bindings)
    (if (= (length bindings) 1)
	(convert-simple-let (car bindings) body env)
	(let* ((first (car bindings))
	       (var (if (symbolp first) first (car first)))
	       (init-form (if (symbolp first) nil (cadr first))))
	  (multiple-value-bind (declarations forms)
	      (cleavir-code-utilities:separate-ordinary-body body)
	    (multiple-value-bind (first remaining)
		(separate-declarations 
		 (cleavir-code-utilities:canonicalize-declaration-specifiers 
		  (mapcar #'cdr declarations))
		 var)
	      (convert
	       `(let ((,var ,init-form))
		  (declare ,@first)
		  (let* ,(cdr bindings)
		    (declare ,@remaining)
		    ,@forms))
	       env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(defmethod convert-special
    ((symbol (eql 'load-time-value)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 2)
  (unless (null (cddr form))
    ;; The HyperSpec specifically requires a "boolean"
    ;; and not a "generalized boolean".
    (unless (member (caddr form) '(nil t))
      (error 'read-only-p-must-be-boolean
	     :expr (caddr form))))
  (cleavir-ast:make-load-time-value-ast
   (convert (cadr form) environment)
   (caddr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.

(defmethod convert-special
    ((symbol (eql 'locally)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.

(defmethod convert-special
    ((head (eql 'progn)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-ast:make-progn-ast
   (convert-sequence (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special ((symbol (eql 'return-from)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 2)
  (unless (symbolp (cadr form))
    (error 'block-name-must-be-a-symbol
	   :expr (cadr form)))
  (let ((info (cleavir-env:block-info (cadr form) env)))
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
  (unless (symbolp var)
    (error 'setq-var-must-be-symbol
	   :expr var))
  (let* ((info (cleavir-env:variable-info var env))
	 (identity (cleavir-env:identity info)))
    (if (typep info 'cleavir-env:constant-variable-info)
	(error 'setq-constant-variable
	       :form var)
	(cleavir-ast:make-setq-ast
	 (find-or-create-ast identity)
	 (convert form env)))))
  
(defmethod convert-special
    ((symbol (eql 'setq)) form environment)
  (cleavir-code-utilities:check-form-proper-list form)
  (unless (oddp (length form))
    (error 'setq-must-have-even-number-of-arguments
	   :expr form))
  (let ((form-asts (loop for (var form) on (cdr form) by #'cddr
			 collect (convert-elementary-setq
				  var form environment))))
    (cleavir-ast:make-progn-ast form-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-special
    ((head (eql 'symbol-macrolet)) form env)
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 1 nil)
  ;; FIXME: syntax check bindings
  ;; FIXME: separate declarations from forms.
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
  (cleavir-code-utilities:check-form-proper-list form)
  (let ((tag-asts
	  (loop for item in (cdr form)
		when (symbolp item)
		  collect (cleavir-ast:make-tag-ast item)))
	(new-env env))
    (loop for ast in tag-asts
	  do (setf new-env (cleavir-env:add-tag
			    new-env (cleavir-ast:name ast))))
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
  (cleavir-code-utilities:check-form-proper-list form)
  (cleavir-code-utilities:check-argcount form 2 2)
  (apply #'cleavir-ast:make-the-ast
	 (convert (caddr form) environment)
	 (mapcar #'cleavir-ast:make-constant-ast
		 (if (and (consp (cadr form)) (eq (car (cadr form)) 'values))
		     (cdr (cadr form))
		     (list (cadr form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCALL.
;;;
;;; While FUNCALL is a function and not a special operator, we convert
;;; it here anyway.

(defmethod convert-special
    ((symbol (eql 'funcall)) form environment)
  (let ((args (convert-sequence (cdr form) environment)))
    (cleavir-ast:make-call-ast (car args) (cdr args))))

