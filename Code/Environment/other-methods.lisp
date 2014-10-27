(cl:in-package #:sicl-global-environment)

(defmethod cleavier-env:macro-function (name (env environment))
  (macro-function name env))

(defmethod cleavier-env:compiler-macro-function (name (env environment))
  (compiler-macro-function name env))
