(cl:in-package #:sicl-evaluation-and-compilation)

(defmacro defmacro (&environment environment name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (sicl-env:macro-function ',name ,environment)
	   ,(cleavir-code-utilities:parse-macro name lambda-list body))))
