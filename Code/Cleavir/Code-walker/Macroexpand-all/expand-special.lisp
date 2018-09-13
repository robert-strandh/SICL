(cl:in-package #:cleavir-macroexpand-all)

(defgeneric expand-special (symbol form env system))

(defmethod expand-special ((symbol (eql 'quote)) form env system)
  form)

(defmethod expand-special ((symbol (eql 'block)) form env system)
  (destructuring-bind (block name . body) form
    (let* ((new-env (cleavir-env:add-block env name)))
      `(,block ,name ,@(expand-sequence body new-env system)))))

(defmethod expand-special ((symbol (eql 'eval-when)) form env system)
  (destructuring-bind (eval-when situations . body) form
    `(,eval-when ,situations ,@(expand-sequence body env system))))

(defun expand-local-function-definition (definition env system)
  (destructuring-bind (name lambda-list . body) definition
    `(,name ,(expand-ordinary-lambda-list lambda-list env system)
            ,@(expand-sequence body env system))))

(defun expand-local-function-definitions (definitions env system)
  (loop for definition in definitions
        collect (expand-local-function-definition definition env system)))

(defmethod expand-special ((symbol (eql 'flet)) form env system)
  (destructuring-bind (flet definitions . body) form
    `(,flet ,(expand-local-function-definition definitions env system)
       ,@(expand-sequence body env system))))

(defmethod expand-special ((symbol (eql 'labels)) form env system)
  (destructuring-bind (flet definitions . body) form
    `(,flet ,(expand-local-function-definition definitions env system)
       ,@(expand-sequence body env system))))

(defmethod expand-special ((symbol (eql 'function)) form env system)
  (destructuring-bind (function argument) form
    (if (symbolp argument)
        form
        (destructuring-bind (lambda lambda-list . body) argument
          `(,function (,lambda ,(expand-ordinary-lambda-list lambda-list env system)
                        ,@(expand-sequence body env system)))))))

(defmethod expand-special ((symbol (eql 'go)) form env system)
  form)

(defmethod expand-special ((symbol (eql 'if)) form env system)
  (destructuring-bind (if condition then-branch . tail) form
    (if (null tail)
        `(,if ,(expand condition env system)
              ,(expand then-branch env system))
        `(,if ,(expand condition env system)
              ,(expand then-branch env system)
              ,(expand (car tail) env system)))))

(defun expand-binding (binding env system)
  (if (consp binding)
      (destructuring-bind (var . rest) binding
        (if (consp rest)
            `(,var ,(expand (first rest) env system))
            binding))
      binding))

(defmethod expand-let-let* (form env system)
  (destructuring-bind (let bindings . body) form
    `(,let ,(loop for binding in bindings
                  collect (expand-binding binding env system))
       ,@(expand-sequence body env system))))

(defmethod expand-special ((symbol (eql 'let)) form env system)
  (expand-let-let* form env system))

(defmethod expand-special ((symbol (eql 'let*)) form env system)
  (expand-let-let* form env system))

(defmethod expand-special ((symbol (eql 'load-time-value)) form env system)
  (destructuring-bind (load-time-value form . rest) form
    `(,load-time-value ,(expand form env system) . ,rest)))

(defmethod expand-special ((symbol (eql 'locally)) form env system)
  (destructuring-bind (locally . body) form
    `(,locally ,@(expand-sequence body env system))))

;;; Given a MACROLET definition and an environment, return a macro
;;; expander (or macro function) for the definition.
(defun expander (definition environment)
  (destructuring-bind (name lambda-list . body) definition
    (let ((lambda-expression
	    (cleavir-code-utilities:parse-macro name
						lambda-list
						body
						environment)))
      (cleavir-env:eval lambda-expression
                        (cleavir-env:compile-time environment)
                        environment))))

(defmethod expand-special ((symbol (eql 'macrolet)) form env system)
  (destructuring-bind (macrolet definitions &rest body) form
    (declare (ignore macrolet))
    (let ((new-env env))
      (loop for definition in definitions
	    for name = (first definition)
	    for expander = (expander definition env)
	    do (setf new-env
		     (cleavir-env:add-local-macro new-env name expander)))
      `(locally ,@(expand-sequence body new-env system)))))

(defmethod expand-special ((symbol (eql 'progn)) form env system)
  (destructuring-bind (progn . body) form
    `(,progn ,@(expand-sequence body env system))))

(defmethod expand-special ((symbol (eql 'return-from)) form env system)
  (destructuring-bind (return-from block-name . rest) form
    (if (null rest)
        form
        `(,return-from ,block-name ,(expand (car rest) env system)))))

(defmethod expand-setq
    ((info cleavir-env:constant-variable-info) var form env system)
  `(,var ,(expand form env system)))

(defmethod expand-setq
    ((info cleavir-env:lexical-variable-info) var form env system)
  `(,var ,(expand form env system)))

(defmethod expand-setq
    ((info cleavir-env:symbol-macro-info) var form env system)
  (let ((expansion (funcall (coerce *macroexpand-hook* 'function)
			    (lambda (form env)
			      (declare (ignore form env))
			      (cleavir-env:expansion info))
			    (cleavir-env:name info)
			    env)))
    `(,expansion ,(expand form env system))))

(defmethod expand-setq
    ((info cleavir-env:special-variable-info) var form env system)
  `(,var ,(expand form env system)))

(defun expand-elementary-setq (var form env system)
  (expand-setq (variable-info env var)
               var
               form
               env
               system))

(defmethod expand-special ((symbol (eql 'setq)) form env system)
  (let ((converted (loop for (var form) on (cdr form) by #'cddr
                         append (expand-elementary-setq var form env system))))
    (if (loop for var in (cdr form) by #'cddr
              for expansion in (cdr converted) by #'cddr
              always (eq var expansion))
        `(setq ,@converted)
        `(progn ,@(loop for var in (cdr form) by #'cddr
                        for expansion in converted by #'cddr
                        for form in (cdr converted) by #'cddr
                        collect (if (eq var expansion)
                                    `(setq ,var ,form)
                                    `(setf ,var ,form)))))))

(defmethod expand-special ((symbol (eql 'symbol-macrolet)) form env system)
  (let ((new-env env))
    (loop for (name expansion) in (cadr form)
	  do (setf new-env
		   (cleavir-env:add-local-symbol-macro new-env name expansion)))
    `(locally ,@(expand-sequence (cddr form) new-env system))))

(defmethod expand-special ((symbol (eql 'tagbody)) form env system)
  (destructuring-bind (tagbody . items) form
    `(,tagbody ,@(loop for item in items
                       collect (if (or (symbolp item) (integerp item))
                                   item
                                   (expand item env system))))))

(defmethod expand-special ((symbol (eql 'the)) form env system)
  (destructuring-bind (the value-type subform) form
    `(,the ,value-type ,(expand subform env system))))

(defmethod expand-special ((symbol (eql 'multiple-value-prog1)) form env system)
  (destructuring-bind (multiple-value-prog1 . forms) form
  `(,multiple-value-prog1 ,@(expand-sequence forms env system))))

(defmethod expand-special ((symbol (eql 'multiple-value-call)) form env system)
  (destructuring-bind (multiple-value-call . forms) form
  `(,multiple-value-call ,@(expand-sequence forms env system))))

(defmethod expand-special ((symbol (eql 'unwind-protect)) form env system)
  (destructuring-bind (unwind-protect . forms) form
  `(,unwind-protect ,@(expand-sequence forms env system))))

(defmethod expand-special ((symbol (eql 'catch)) form env system)
  (destructuring-bind (catch . forms) form
  `(,catch ,@(expand-sequence forms env system))))

(defmethod expand-special ((symbol (eql 'throw)) form env system)
  (destructuring-bind (throw . forms) form
  `(,throw ,@(expand-sequence forms env system))))

(defmethod expand-special ((symbol (eql 'progv)) form env system)
  (destructuring-bind (progv . forms) form
  `(,progv ,@(expand-sequence forms env system))))
