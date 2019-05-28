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
