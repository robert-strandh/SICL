(cl:in-package #:sicl-evaluation-and-compilation)

(defmacro defmacro (name lambda-list &body body)
  (let ((expansion
          (cleavir-code-utilities:parse-macro name lambda-list body)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (macro-function ',name) ,expansion))))
