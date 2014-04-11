(cl:in-package #:cleavir-ast-to-source)

(defgeneric to-source (ast dictionary))

(defun ast-to-source (ast)
  (to-source ast nil))

(defmethod to-source (ast dictionary)
  (declare (ignore ast dictionary))
  '???)
