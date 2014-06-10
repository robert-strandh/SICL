(cl:in-package #:sicl-generate-ast)

(defmethod convert-compound
    ((symbol (eql 'sicl-cons:load-car)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (cleavir-ast:make-car-ast
   (convert (cadr form) environment)))

(defmethod convert-compound
    ((symbol (eql 'sicl-cons:load-cdr)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (cleavir-ast:make-cdr-ast
   (convert (cadr form) environment)))
