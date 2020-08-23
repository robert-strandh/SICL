(cl:in-package #:sicl-ast-evaluator)

(defclass environment
    (sicl-simple-environment:simple-environment)
  ())

(defmethod initialize-instance :after
    ((environment environment) &key)
  (do-external-symbols (symbol '#:common-lisp)
    (when (special-operator-p symbol)
      (setf (env:special-operator symbol environment) t))))
