(cl:in-package #:sicl-env)

(defmacro defmacro (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (sicl-env:macro-function ',name sicl-env:*global-environment*)
	   ,(cleavir-code-utilities:parse-macro name lambda-list body))))
