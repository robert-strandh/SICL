(cl:in-package #:sicl-global-environment)

(defmethod trucler:macro-function (name (env environment))
  (macro-function name env))

(defmethod trucler:compiler-macro-function (name (env environment))
  (compiler-macro-function name env))

(defmethod trucler:symbol-macro-expansion (name (env environment))
  (multiple-value-bind (expander expansion)
      (symbol-macro name env)
    (if (null expander)
	name
	expansion)))

(defmethod (setf sicl-genv:function-lambda-list)
    (value function-name (env trucler-reference:environment))
  (setf (sicl-genv:function-lambda-list
         function-name
         (trucler:global-environment (make-instance 'trucler-reference:client) env))
        value))

(defmethod (setf sicl-genv:function-type)
    (value function-name (env trucler-reference:environment))
  (setf
   (sicl-genv:function-type
    function-name
    (trucler:global-environment (make-instance 'trucler-reference:client) env))
   value))

(defmethod (setf sicl-genv:special-variable)
    (value symbol (env trucler-reference:environment) initialize-p)
  (setf
   (sicl-genv:special-variable
    symbol
    (trucler:global-environment (make-instance 'trucler-reference:client) env)
    initialize-p)
   value))

(defmethod (setf sicl-genv:constant-variable)
    (value symbol (env trucler-reference:environment))
  (setf
   (sicl-genv:constant-variable
    symbol
    (trucler:global-environment (make-instance 'trucler-reference:client) env))
   value))
