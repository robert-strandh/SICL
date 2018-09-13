(cl:in-package #:cleavir-macroexpand-all)

(defgeneric expand-special (symbol form env system))

(defgeneric expand (form environment system))

(defun expand-sequence (forms environment system)
  (loop for form in forms
        collect (expand form environment system)))

(defgeneric expand-form (form info env system))

(defmethod expand-form
    (form (info cleavir-env:symbol-macro-info) env system)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (lambda (form env)
			      (declare (ignore form env))
			      (cleavir-env:expansion info))
			    form
			    env)))
    (expand expansion env system)))
  
(defmethod expand-form
    (form (info cleavir-env:constant-variable-info) env system)
  (declare (ignore env system))
  form)

(defmethod expand-form
    (form (info cleavir-env:lexical-variable-info) env system)
  (declare (ignore env system))
  form)

(defmethod expand-form
    (form (info cleavir-env:special-variable-info) env system)
  (declare (ignore env system))
  form)

(defmethod expand-form
    (form (info cleavir-env:local-macro-info) env system)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (cleavir-env:expander info)
			    form
			    env)))
    (expand expansion env system)))

(defmethod expand-form
    (form (info cleavir-env:global-macro-info) env system)
  (expand (funcall (coerce *macroexpand-hook* 'function)
                   (cleavir-env:expander info)
                   form
                   env)
          env system))

(defmethod expand-form
    (form (info cleavir-env:local-function-info) env system)
  (destructuring-bind (function-name . forms) form
    `(,function-name ,@(expand-sequence forms env system))))

(defmethod expand-form
    (form (info cleavir-env:global-function-info) env system)
  (destructuring-bind (function-name . forms) form
    `(,function-name ,@(expand-sequence forms env system))))

(defmethod expand-form
    (form (info cleavir-env:special-operator-info) env system)
  (expand-special (car form) form env system))

(defun expand-lambda-list-element (element env system)
  (if (and (consp element)
           (consp (rest element)))
      (destructuring-bind (first second . rest) element
        `(,first ,(expand second env system) . ,rest))
      element))

(defun expand-ordinary-lambda-list (lambda-list env system)
  (loop for element in lambda-list
        collect (expand-lambda-list-element element env system)))

(defmethod expand (form env system)
  (cond ((and (not (consp form)) (not (symbolp form)))
         form)
	((and (symbolp form) (constantp form))
	 form)
	((symbolp form)
	 (let ((info (variable-info env form)))
	   (expand-form form info env system)))
	((symbolp (car form))
	 (let ((info (function-info env (car form))))
	   (expand-form form info env system)))
	(t
	 ;; The form must be a compound form where the CAR is a lambda
	 ;; expression.
         (destructuring-bind ((lambda lambda-list . body) . args) form
             `((,lambda ,(expand-ordinary-lambda-list lambda-list env system)
                 ,@(expand-sequence body env system))
               ,@(expand-sequence args env system))))))

(defun macroexpand-all (form env system)
  (expand form env system))
