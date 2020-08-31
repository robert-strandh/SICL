(cl:in-package #:sicl-data-and-control-flow)

(defun destructuring-bind-expander (lambda-list expression body)
  (let ((expression-var (gensym))
        (parsed-lambda-list
          (cleavir-code-utilities:parse-destructuring-lambda-list lambda-list)))
    (multiple-value-bind (bindings variables-to-ignore)
        (cleavir-code-utilities:destructure-lambda-list
         parsed-lambda-list expression-var)
      `(let* ((,expression-var ,expression)
              ,@bindings)
         (declare (ignore ,@variables-to-ignore))
         ,@body))))
