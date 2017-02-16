(cl:in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a symbol macro.

(defmethod convert-form
    (form (info cleavir-env:symbol-macro-info) env system)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (lambda (form env)
			      (declare (ignore form env))
			      (cleavir-env:expansion info))
			    form
			    env)))
    (with-preserved-toplevel-ness
      (convert expansion env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a constant variable.

(defmethod convert-form
    (form (info cleavir-env:constant-variable-info) env system)
  (convert-constant (cleavir-env:value info) env system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a lexical variable.

(defun maybe-wrap-the (type ast)
  "Make a THE ast if the type is actually helpful (i.e. not all T)
If AST is already a THE-AST, collapses both into one."
  (multiple-value-bind (req opt rest restp)
      (parse-values-type type)
    ;; this takes care of the THE fudginess.
    (unless restp (setf rest t))
    ;; collapse (the ... (the ...))
    (loop while (typep ast 'cleavir-ast:the-ast)
	  do (setf (values req opt rest)
		   (meet-values-types
		    req opt rest
		    (cleavir-ast:required-types ast)
		    (cleavir-ast:optional-types ast)
		    (cleavir-ast:rest-type ast))
		   ast (cleavir-ast:form-ast ast)))
    (if (and (every (lambda (x) (subtypep t x)) req)
	     (every (lambda (x) (subtypep t x)) opt)
	     (subtypep t rest))
	;; useless type, don't bother
	ast
	(cleavir-ast:make-the-ast ast req opt rest))))

(defmethod convert-form
    (form (info cleavir-env:lexical-variable-info) env system)
  (declare (ignore system))
  (when (eq (cleavir-env:ignore info) 'ignore)
    (warn 'ignored-variable-referenced :expr form))
  (maybe-wrap-the (cleavir-env:type info)
		  (cleavir-env:identity info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a special variable.
;;; We do this by generating a call to SYMBOL-VALUE.

(defgeneric convert-special-variable (info global-env system))

(defmethod convert-special-variable (info global-env system)
  (declare (ignore global-env))
  (let ((symbol (cleavir-env:name info)))
    (cleavir-ast:make-symbol-value-ast
     (cleavir-ast:make-load-time-value-ast `',symbol))))

(defmethod convert-form
    (form (info cleavir-env:special-variable-info) env system)
  (declare (ignore form))
  (let ((global-env (cleavir-env:global-environment env)))
    (convert-special-variable info global-env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a local macro.
;;; A local macro can not have a compiler macro associated with it.
;;;
;;; If we found a local macro in ENV, it means that ENV is not the
;;; global environment.  And it must be the same kind of agumentation
;;; environment that was used when the local macro was created by the
;;; use of MACROLET.  Therefore, the expander should be able to handle
;;; being passed the same kind of environment.

(defmethod convert-form
    (form (info cleavir-env:local-macro-info) env system)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (cleavir-env:expander info)
			    form
			    env)))
    (with-preserved-toplevel-ness
      (convert expansion env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a global macro.
;;; A global macro can have a compiler macro associated with it.

(defmethod convert-form
    (form (info cleavir-env:global-macro-info) env system)
  (let ((compiler-macro (cleavir-env:compiler-macro info)))
    (with-preserved-toplevel-ness
      (if (null compiler-macro)
	  ;; There is no compiler macro, so we just apply the macro
	  ;; expander, and then convert the resulting form.
	  (convert (funcall (coerce *macroexpand-hook* 'function)
			    (cleavir-env:expander info)
			    form
			    env)
		   env system)
	  ;; There is a compiler macro, so we must see whether it will
	  ;; accept or decline.
	  (let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
					compiler-macro
					form
					env)))
	    (if (eq form expanded-form)
		;; If the two are EQ, this means that the compiler macro
		;; declined.  Then we appply the macro function, and
		;; then convert the resulting form, just like we did
		;; when there was no compiler macro present.
		(convert (funcall (coerce *macroexpand-hook* 'function)
				  (cleavir-env:expander info)
				  expanded-form
				  env)
			 env system)
		;; If the two are not EQ, this means that the compiler
		;; macro replaced the original form with a new form.
		;; This new form must then again be converted without
		;; taking into account the real macro expander.
		(convert expanded-form env system)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a local function.
;;; A local function can not have a compiler macro associated with it.

(defmethod convert-form
    (form (info cleavir-env:local-function-info) env system)
  (make-call form info env (cdr form) system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONVERT-FUNCTION.
;;;
;;; This generic function converts a function given by a description
;;; in the form of an INFO instance.  That INFO instance was obtained
;;; by calling FUNCTION-INFO with an environment and a function name.
;;; The function signals an error if the INFO instance is not a
;;; GLOBAL-FUNCTION-INFO or a LOCAL-FUNCTION-INFO.  Client code can
;;; override the default behavior by adding methods to this function,
;;; specialized to the particular system defined by that client code.

(defgeneric convert-function (info env system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a global function.
;;; A global function can have  compiler macro associated with it.

;; Force a type from ftype information (thus, hopefully a subtype
;;  of FUNCTION) into a strict (function lambda-list return-values)
;; FIXME: This is bad. Good type canonicalization is important.
(defun canonicalize-ftype (type)
  (if (consp type)
      (case (first type)
	((and or) (if (= (length type) 2)
		      (canonicalize-ftype (second type))
		      '(function (&rest t) (values &rest t))))
	((function)
	 (destructuring-bind (&optional (ll '*) (ret '*))
	     (rest type)
	   (let ((ll (if (eq ll '*)
			 ;; (function *) and (function (&rest t))
			 ;;  actually mean different things: the
			 ;;  first is a strict superset of the
			 ;;  second. But for our purposes here,
			 ;;  there's no problem.
			 '(&rest t)
			 ll))
		 (ret (cond ((eq ret '*)
			     ;; these actually are equivalent.
			     '(values &rest t))
			    ((and (consp ret)
				  (eq (first ret) 'values))
			     ret)
			    (t `(values ,ret)))))
	     `(function ,ll ,ret))))
	(t '(function (&rest t) (values &rest t))))
      '(function (&rest t) (values &rest t))))

;; ftype must be canonicalized.
(defun function-type-lambda-list (ftype) (second ftype))
(defun function-type-return-values (ftype) (third ftype))

;; assumes ftype is valid!
(defun maybe-type-wrap-arguments (ftype-ll arg-asts)
  "Given the lambda list of a function's ftype, and a list of ASTs in a call to that function, returns a list of ASTs possibly wrapped in THE-ASTs, based on the ftype.
If the call is invalid, returns like inline-lambda-init."
  (loop with state = :required
	for param in ftype-ll
	when (find param '(&optional &rest &key))
	  do (setf state param)
	else when (eq state :required)
	       when (null arg-asts)
		 do (return
		      (values nil 'not-enough-arguments-warning))
	       else collect (maybe-wrap-the param (first arg-asts))
		      into result
		    and do (setf arg-asts (rest arg-asts))
	else when (eq state '&optional)
	       when arg-asts
		 collect (maybe-wrap-the param (first arg-asts))
		   into result
		 and do (setf arg-asts (rest arg-asts))
	       else do (return result)
	else when (eq state '&rest)
	  ;; technically, there could be a &key, but that is such
	  ;;  a bizarre circumstance that I don't mind ignoring it.
	  do (return
	       (nconc result
		      (mapcar
		       (lambda (a)
			 (maybe-wrap-the param a))
		       arg-asts)))
	else when (eq state '&key)
	       do (return (if (evenp (length arg-asts))
			      (nconc result arg-asts)
			      (values
			       nil
			       'odd-keyword-portion-warning)))
	finally (return
		  (if (null arg-asts)
		      result
		      (values nil 'too-many-arguments-warning)))))

(defun inline-lambda-init (env system lambda-list arg-asts)
  "This mess takes an environment and system, and then a lambda list and a list of converted arguments, and returns a list of forms that initialize the lambda variables with the arguments.
If this is impossible for some reason, a second non-nil value will be returned. T indicates that the call is still valid, so this function is declining for some other reason (like not having the semantics implemented). Anything else is a condition class designator.
This function should not require an environment or system, but it unfortunately does its own conversions (a call to cl:list for &rest, and a constant nil for &optional)."
  (flet ((noinline (&optional (condition t))
	   (return-from inline-lambda-init (values nil condition)))
	 (make-list-call (args)
	   ;; ew.
	   (let* ((info (cleavir-env:function-info env 'list))
		  (function-ast
		   (convert-function info env system)))
	     (cleavir-ast:make-call-ast function-ast args))))
    ;; we know the list's format from general-purpose-asts, so we
    ;;  don't do any error checking.
    ;; we do a standard "parse". state is the last
    ;;  lambda-list-keyword or :required. rest is whether we've
    ;;  seen a &rest parameter, useful for error checking and &key
    (loop with state = :required
	  with rest = nil
	  for parameter in lambda-list
	  when (find parameter '(&rest &optional &key))
	    do (setf state parameter)
	  else when (eq state :required)
		 if arg-asts
		   collect (cleavir-ast:make-setq-ast
			    parameter (first arg-asts))
		   and do (setf arg-asts (rest arg-asts))
	       else do (noinline
			'not-enough-arguments-style-warning)
	  else when (eq state '&optional)
		 if arg-asts
		   collect (cleavir-ast:make-setq-ast
			    (second parameter)
			    (first arg-asts))
		   and collect (cleavir-ast:make-setq-ast
				(first parameter)
				(second parameter))
		   and do (setf arg-asts (rest arg-asts))
	         else
		   collect (cleavir-ast:make-setq-ast
			    (second parameter)
			    (convert nil env system))
	  else when (eq state '&rest)
		 collect (cleavir-ast:make-setq-ast
			  parameter (make-list-call arg-asts))
		 and do (setf rest parameter)
	  else when (eq state '&key)
		 ;; TODO
		 ;; There are 3 major issues with &key inlining:
		 ;; 1) in general we need a &rest list. if there's
		 ;;    no &rest, that means we have to add an lvar.
		 ;; 2) we'd like to deal with constant keys, but
		 ;;    these are converted into inscrutable ASTs.
		 ;;    One solution would be to avoid converting
		 ;;     arguments and just look for keys; the
		 ;;     general solution is type inference.
		 ;; 3) parsing keys sucks. &aok and getf, oy vey
		 do (return (noinline))
	  finally (when (and arg-asts (not rest))
		    ;; out of parameters; if arg-asts = nil,
		    ;;  we return normally
		    (return
		      (values
		       nil
		       'too-many-arguments-style-warning))))))

;; Given a canonical ftype or a converted lambda list, return
;; (values minimum-expected-arguments maximum).
;; Maximum can be NIL, indicating no upper limit.
;; FIXME: Maybe do this earlier and certainly do it more carefully.
(defun expected-arguments (lambda-list)
  (loop with min = 0
	with max = 0
	with state = :required
	for param in lambda-list
	do (case param
	     ((&optional) (setf state '&optional))
	     ((&rest) (return (values min nil)))
	     ((&key) (return (values min nil)))
	     (t (if (eq state :required)
		    (incf min)
		    ;; &optional
		    (incf max))))
	finally (return (values min max))))

(defun make-call (form info env arguments system)
  (let ((argument-asts (convert-sequence arguments env system))
	(ast (cleavir-env:ast info))
	(ftype (canonicalize-ftype (cleavir-env:type info))))
    (labels ((violated-type (failure)
	       (multiple-value-bind (min max)
		   (expected-arguments (second ftype))
		 (warn failure
		       :expr form
		       :expected-min min :expected-max max
		       :callee-ftype ftype))
	       (return-from make-call (noinline)))
	     (inline-mismatch (failure)
	       (multiple-value-bind (min max)
		   (expected-arguments
		    (cleavir-ast:lambda-list ast))
		 (warn failure
		       :expr form
		       :expected-min min :expected-max max))
	       (return-from make-call (noinline)))
	     (maybe-wrap-return (ast)
	       (let ((ret (function-type-return-values ftype)))
		 (maybe-wrap-the ret ast)))
	     ;; Basic uninlined call with no type declarations.
	     (noinline ()
	       (let ((function-ast
		       (convert-function info env system)))
		 (maybe-wrap-return
		  (cleavir-ast:make-call-ast function-ast
					     argument-asts)))))
      (multiple-value-bind (asts failure)
	  (maybe-type-wrap-arguments
	   (function-type-lambda-list ftype)
	   argument-asts)
	(if failure
	    ;; return immediately
	    (violated-type failure)
	    (setf argument-asts asts)))
      (if (and (eq (cleavir-env:inline info) 'cl:inline)
	       (not (null ast)))
	  ;; We might be able to inline the call.
	  ;; Try to make the expansion.
	  ;; We must clone first, because the lambda list lvars
	  ;;  have to match those in the setqs.
	  (let ((clone
		  (cleavir-ast-transformations:clone-ast ast)))
	    (multiple-value-bind (init failure)
		(inline-lambda-init env system
				    (cleavir-ast:lambda-list clone)
				    argument-asts)
	      (cond ((eq failure t) (noinline))
		    (failure (inline-mismatch failure))
		    (t
		     (process-progn
		      (append
		       init
		       (list (maybe-wrap-return
			      (cleavir-ast:body-ast clone)))))))))
	  ;; Generate an ordinary call.
	  (noinline)))))

(defmethod convert-form
    (form (info cleavir-env:global-function-info) env system)
  ;; When we compile a call to a global function, it is possible that
  ;; we are in COMPILE-TIME-TOO mode.  In that case, we must first
  ;; evaluate the form.
  (when (and *current-form-is-top-level-p* *compile-time-too*)
    (cleavir-env:eval form env env))
  (let ((compiler-macro (cleavir-env:compiler-macro info))
	(notinline (eq 'notinline (cleavir-env:inline info))))
    (if (or notinline (null compiler-macro))
	;; There is no compiler macro.  Create the call.
	(make-call form info env (cdr form) system)
	;; There is a compiler macro.  We must see whether it will
	;; accept or decline.
	(let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
				      compiler-macro
				      form
				      env)))
	  (if (eq form expanded-form)
	      ;; If the two are EQ, this means that the compiler macro
	      ;; declined.  We are left with function-call form.
	      ;; Create the call, just as if there were no compiler
	      ;; macro present.
	      (make-call form info env (cdr form) system)
	      ;; If the two are not EQ, this means that the compiler
	      ;; macro replaced the original form with a new form.
	      ;; This new form must then be converted.
	      (convert expanded-form env system))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a special form.

(defgeneric convert-special (head form environment system))

(defmethod convert-special :around (head form environment system)
  (declare (ignore system))
  (check-special-form-syntax head (raw form))
  (when (and *compile-time-too*
	     *current-form-is-top-level-p*
	     (not (member head
			  '(progn locally macrolet symbol-macrolet eval-when))))
    (cleavir-env:eval form environment environment))
  (call-next-method))

(defmethod convert-form
    (form (info cleavir-env:special-operator-info) env system)
  (convert-special (car form) form env system))
