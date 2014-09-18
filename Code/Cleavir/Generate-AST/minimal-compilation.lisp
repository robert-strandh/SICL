(in-package #:cleavir-generate-ast)

(defgeneric minimally-compile-form (form info env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities for minimally compiling function definitions and lambda
;;; expressions.

(defun minimally-compile-sequence (sequence env)
  (loop for form in sequence
	collect (minimally-compile form env)))

(defun minimally-compile-optional-or-key (optional-or-key env)
  `(,(first optional-or-key)
    ,(minimally-compile (second optional-or-key) env)
    ,@(if (null (rest (rest optional-or-key)))
	  '()
	  (rest (rest optional-or-key)))))

(defun minimally-compile-lambda-list (parsed-lambda-list env)
  `(,@(cleavir-code-utilities:required parsed-lambda-list)
    ,@(let ((whole (cleavir-code-utilities:whole parsed-lambda-list)))
	(if (eq whole :none)
	    '()
	    `(&whole ,(second whole))))
    ,@(let ((environment (cleavir-code-utilities:environment parsed-lambda-list)))
	(if (eq environment :none)
	    '()
	    `(&environment ,(second environment))))
    ,@(let ((rest (cleavir-code-utilities:rest-body parsed-lambda-list)))
	(if (eq rest :none)
	    '()
	    `(&rest ,rest)))
    ,@(let ((optionals (cleavir-code-utilities:optionals parsed-lambda-list)))
	(if (eq optionals :none)
	    '()
	    `(&optional
	      ,@(loop for optional in optionals
		      collect (minimally-compile-optional-or-key optional env)))))
    ,@(let ((keys (cleavir-code-utilities:keys parsed-lambda-list)))
	(if (eq keys :none)
	    '()
	    `(&key
	      ,@(loop for key in keys
		      collect (minimally-compile-optional-or-key key env)))))
    ,@(if (cleavir-code-utilities:allow-other-keys parsed-lambda-list)
	  '(&allow-other-keys)
	  '())
    ,@(let ((aux (cleavir-code-utilities:aux parsed-lambda-list)))
	(if (eq aux :none)
	    '()
	    `(&aux
	      ,@(loop for entry in aux
		      collect `(,(first entry)
				,(minimally-compile (second entry) env))))))))

(defun minimally-compile-code (lambda-list body env)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    (let ((parsed-lambda-list
	    (cleavir-code-utilities:parse-ordinary-lambda-list lambda-list)))
      `(,(minimally-compile-lambda-list parsed-lambda-list env)
	,@declarations
	,(if (null documentation)
	     '()
	     `(,documentation))
	,@(minimally-compile-sequence forms env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MINIMALLY-COMPILE-LAMBDA-CALL.

(defun minimally-compile-lambda-call (form env)
  (destructuring-bind ((lambda lambda-list &rest body) &rest args) form
    (declare (ignore lambda))
    `((lambda ,@(minimally-compile-code lambda-list body env))
      ,@(minimally-compile-sequence args env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MINIMALLY-COMPILE.  The main entry point.

(defun minimally-compile (form env)
  (cond ((and (not (consp form)) (not (symbolp form)))
	 form)
	((symbolp form)
	 (let ((info (variable-info env form)))
	   (minimally-compile-form form info env)))
	((symbolp (first form))
	 (let ((info (function-info env (first form))))
	   (minimally-compile-form form info env)))
	(t
	 (minimally-compile-lambda-call form env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on MINIMALLY-COMPILE-FORM.

;;; Minimally compiling a symbol that has a definition as a symbol
;;; macro.
(defmethod minimally-compile-form
    (form (info cleavir-env:symbol-macro-info) env)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (lambda (form env)
			      (declare (ignore form env))
			      (cleavir-env:expansion info))
			    form
			    env)))
    (minimally-compile expansion env)))

;;; Minimally compiling a symbol that has a definition as a constant
;;; variable.
(defmethod minimally-compile-form
    (form (info cleavir-env:constant-variable-info) env)
  (declare (ignore env))
  form)
	  
;;; Minimally compiling a symbol that has a definition as a lexical
;;; variable.
(defmethod minimally-compile-form
    (form (info cleavir-env:lexical-variable-info) env)
  (declare (ignore env))
  form)

;;; Minimally compiling a symbol that has a definition as a special
;;; variable.
(defmethod minimally-compile-form
    (form (info cleavir-env:special-variable-info) env)
  (declare (ignore env form))
  ;; It is possible that the name in the INFO instance is different
  ;; from the first element of FORM, because if an error occurred
  ;; because the name was undefined, then a restart may have
  ;; substituted a different name.  For that reason, we use the name
  ;; in the INFO instance rather than the form.
  (cleavir-env:name info))

;;; Minimally compiling a compound form that calls a local macro.
(defmethod minimally-compile-form
    (form (info cleavir-env:local-macro-info) env)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (cleavir-env:expander info)
			    form
			    env)))
    (minimally-compile expansion env)))

;;; Minimally compiling a compound form that calls a global macro.
(defmethod minimally-compile-form
    (form (info cleavir-env:global-macro-info) env)
  (let ((compiler-macro (cleavir-env:compiler-macro info)))
    (if (null compiler-macro)
	;; There is no compiler macro, so we just apply the macro
	;; expander, and then minmally compile the resulting form.
	(minimally-compile (funcall (coerce *macroexpand-hook* 'function)
				    (cleavir-env:expander info)
				    form
				    env)
			   env)
	;; There is a compiler macro, so we must see whether it will
	;; accept or decline.
	(let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
				      compiler-macro
				      form
				      env)))
	  (if (eq form expanded-form)
	      ;; If the two are EQ, this means that the compiler macro
	      ;; declined.  Then we appply the macro function, and
	      ;; then minimally compile the resulting form, just like
	      ;; we did when there was no compiler macro present.
	      (minimally-compile (funcall (coerce *macroexpand-hook* 'function)
					  (cleavir-env:expander info)
					  expanded-form
					  env)
				 env)
	      ;; If the two are not EQ, this means that the compiler
	      ;; macro replaced the original form with a new form.
	      ;; This new form must then again be minimally compiled
	      ;; without taking into account the real macro expander.
	      (minimally-compile expanded-form env))))))

;;; Minimally compiling a compound form that calls a local function.
;;; A local function can not have a compiler macro associated with it.
(defmethod minimally-compile-form
    (form (info cleavir-env:local-function-info) env)
  `(,(first form)
    ,@(minimally-compile-sequence (rest form) env)))

;;; Minimally compiling a compound form that calls a global function.
;;; A global function can have compiler macro associated with it.
(defmethod minimally-compile-form
    (form (info cleavir-env:global-function-info) env)
  ;; It is possible that the name in the INFO instance is different
  ;; from the first element of FORM, because if an error occurred
  ;; because the name was undefined, then a restart may have
  ;; substituted a different name.  For that reason, we start by
  ;; updating the form.
  (setf form (cons (cleavir-env:name info) (rest form)))
  (let ((compiler-macro (cleavir-env:compiler-macro info)))
    (if (null compiler-macro)
	;; There is no compiler macro.  Minimally compile the arguments.
	`(;; We take the name from the INFO instance rather than from
	  ;; the form, because in case of an error, a restart might
	  ;; have replaced the function name by a different one.
	  ,(first form)
	  ,@(minimally-compile-sequence (rest form) env))
	;; There is a compiler macro.  We must see whether it will
	;; accept or decline.
	(let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
				      compiler-macro
				      form
				      env)))
	  (if (eq form expanded-form)
	      ;; If the two are EQ, this means that the compiler macro
	      ;; declined.  We are left with function-call form.
	      ;; Minimally compile the arguments, just as if there
	      ;; were no compiler macro present.
	      `(,(first form)
		,@(minimally-compile-sequence (rest form) env))
	      ;; If the two are not EQ, this means that the compiler
	      ;; macro replaced the original form with a new form.
	      ;; This new form must then be minimally compiled.
	      (minimally-compile expanded-form env))))))

(defgeneric minimally-compile-special-form (symbol form env))

;;; Minimally compiling a special form.
(defmethod minimally-compile-form
    (form (info cleavir-env:special-operator-info) env)
  (minimally-compile-special-form (first form) form env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; :AROUND method on MINIMALLY-COMPILE-SPECIAL-FORM that calls the
;;; syntax checker for special forms.

(defmethod minimally-compile-special-form :around (symbol form env)
  (declare (ignore env))
  (check-special-form-syntax symbol form)
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on MINIMALLY-COMPILE-SPECIAL-FORM.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling QUOTE.

(defmethod minimally-compile-special-form
    ((symbol (eql 'quote)) form env)
  (declare (ignore env))
  form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling BLOCK.

(defmethod minimally-compile-special-form
    ((symbol (eql 'block)) form env)
  `(block ,(second form)
     ,@(minimally-compile-sequence (rest (rest form)) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling EVAL-WHEN.

(defmethod minimally-compile-special-form
    ((symbol (eql 'eval-when)) form env)
  `(eval-when ,(second form)
     ,@(minimally-compile-sequence (rest (rest form)) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling FLET and LABELS.

(defun minimally-compile-flet-or-labels (symbol form env)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body (cddr form))
    `(,symbol ,(loop for (name lambda-list . local-body) in (second form)
		     collect `(name ,(minimally-compile-code
				      lambda-list local-body env)
				    ,@declarations
				    ,@(minimally-compile-sequence forms env))))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'flet)) form env)
  (minimally-compile-flet-or-labels symbol form env))

(defmethod minimally-compile-special-form
    ((symbol (eql 'labels)) form env)
  (minimally-compile-flet-or-labels symbol form env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling FUNCTION.

(defun minimally-compile-lambda-function (lambda-form env)
  (destructuring-bind (lambda-list . body) lambda-form
    `(lambda
	 ,@(minimally-compile-code lambda-list body env))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'function)) form env)
  `(function
    ,(if (proper-function-name-p (second form))
	 (second form)
	 (minimally-compile-lambda-function (second form) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling GO.

(defmethod minimally-compile-special-form
    ((symbol (eql 'go)) form env)
  (declare (ignore env))
  form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling IF.

(defmethod minimally-compile-special-form
    ((symbol (eql 'if)) form env)
  `(if ,(minimally-compile (second form) env)
       ,(minimally-compile (third form) env)
       ,(if (null (nthcdr 3 form))
	    nil
	    (minimally-compile (fourth form) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling LET and LET*
;;;
;;; What we are doing here is not quite correct, because we assume
;;; that the variables in the bindings are lexical variables.
;;; However, this is minimal compilation, so the only purpose of
;;; adding a variable binding to the environment is to shadow a
;;; possible symbol macro.  For that purpose, the exact nature of the
;;; variable does not matter. 

(defun minimally-compile-binding (binding env)
  (if (symbolp binding)
      `(,binding nil)
      `(,(first binding)
	,(if (null (rest binding))
	     nil
	     (minimally-compile (second binding) env)))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'let)) form env)
  (destructuring-bind (bindings &rest body) (rest form)
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      `(let ,(loop for binding in bindings
		   collect (minimally-compile-binding binding env))
	 ,@(if (null declarations)
	       '()
	       declarations)
	 ,@(let ((new-env env))
	     (loop for binding in bindings
		   for var = (if (symbolp binding) binding (first binding))
		   do (setf new-env
			    (cleavir-env:add-lexical-variable new-env var)))
	     (minimally-compile-sequence forms new-env))))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'let*)) form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (multiple-value-bind (declarations forms)
	(cleavir-code-utilities:separate-ordinary-body body)
      (let ((new-env env))
	`(let* ,(loop for binding in bindings
		      for var = (if (symbolp binding) binding (first binding))
		      collect (minimally-compile-binding binding new-env)
		      do (setf new-env
			       (cleavir-env:add-lexical-variable new-env var)))
	   ,@(if (null declarations)
		 '()
		 declarations)
	   ,@(minimally-compile-sequence forms env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling LOAD-TIME-VALUE.

(defmethod minimally-compile-special-form
    ((symbol (eql 'load-time-value)) form env)
  `(load-time-value
    ,(minimally-compile (second form) env)
    ,@(if (null (rest (rest form)))
	  '()
	  (list (third form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling LOCALLY.

(defmethod minimally-compile-special-form
    ((symbol (eql 'locally)) form env)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body (rest form))
    (let ((new-env (augment-environment-with-declarations
		    env declarations)))
      `(locally
	   ,@(if (null declarations)
		 '()
		 declarations)
	 ,(minimally-compile-sequence forms new-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling MACROLET.

(defun minimally-compile-macro-definition (definition env)
  (destructuring-bind (name lambda-list &rest body) definition
    (multiple-value-bind (declarations documentation forms)
	(cleavir-code-utilities:separate-function-body body)
      (let* ((parsed-lambda-list
	       (cleavir-code-utilities:parse-macro-lambda-list lambda-list))
	     (compiled-lambda-list
	       (minimally-compile-lambda-list parsed-lambda-list env)))
	(let ((expander-form
		(cleavir-code-utilities:parse-macro
		 name
		 compiled-lambda-list
		 `(,@(if (null declarations)
			 '()
			 (list declarations))
		   ,@(if (null documentation)
			 '()
			 (list documentation))
		   ,@(minimally-compile-sequence forms env))
		 env)))
	  (compile nil expander-form))))))

(defmethod minimally-compile-special-form
    ((symbol (eql 'macrolet)) form env)
  (destructuring-bind (macrolet definitions &rest body) form
    (declare (ignore macrolet))
    (let ((new-env env))
      (loop for definition in definitions
	    for name = (first definition)
	    for expander = (minimally-compile-macro-definition definition env)
	    do (setf new-env
		     (cleavir-env:add-local-macro new-env name expander)))
      (multiple-value-bind (declarations forms)
	  (cleavir-code-utilities:separate-ordinary-body body)
	`(locally ,@declarations
	   ,@(minimally-compile-sequence forms new-env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling PROGN.

(defmethod minimally-compile-special-form
    ((head (eql 'progn)) form env)
  `(progn
     ,@(minimally-compile-sequence (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling RETURN-FROM.

(defmethod minimally-compile-special-form
    ((symbol (eql 'return-from)) form env)
  (destructuring-bind (return-from block-name result) form
    (declare (ignore return-from))
    `(return-from ,block-name ,(minimally-compile result env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling SETQ.

(defmethod minimally-compile-special-form
    ((symbol (eql 'setq)) form env)
  `(setq
    ,@(loop for (var form) on (cdr form) by #'cddr
	    collect `(var (minimally-compile form env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling SYMBOL-MACROLET.

(defmethod minimally-compile-special-form
    ((head (eql 'symbol-macrolet)) form env)
  (let ((new-env env))
    (loop for (name expansion) in (second form)
	  do (setf new-env
		   (cleavir-env:add-local-symbol-macro new-env name expansion)))
    (minimally-compile `(progn ,@(rest (rest form))) new-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling TAGBODY.

(defmethod minimally-compile-special-form
    ((symbol (eql 'tagbody)) form env)
  `(tagbody
      ,@(loop for item in (rest form)
	      collect (if (symbolp item)
			  item
			  (minimally-compile item env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling THE.

(defmethod minimally-compile-special-form
    ((symbol (eql 'the)) form env)
  `(the ,(second form)
	,(minimally-compile (third form) env)))
