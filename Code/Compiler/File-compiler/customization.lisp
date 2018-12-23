(cl:in-package #:sicl-file-compiler)

(defmethod cleavir-cst-to-ast:convert-global-function-reference
    (cst
     (info cleavir-env:global-function-info)
     (env sicl-genv:environment)
     system)
  (declare (ignore system))
  (cleavir-ast:make-constant-fdefinition-ast
   (cleavir-env:name info)
   :origin (cst:source cst)))

;;; For this environment, whenever Cleavir asks for information about
;;; a function name and that function name is a symbol in the
;;; CLEAVIR-PRIMOP package, we want to return an object of type
;;; CLEAVIR-ENV:SPECIAL-OPERATOR-INFO so that the form can be treated
;;; as a special form by GENERATE-AST.  The exception being
;;; CLEAVIER-PRIMOP:CALL-WITH-VARIABLE-BOUND which is a function.
(defmethod cleavir-env:function-info :around
    ((env sicl-genv:environment)
     function-name)
  (if (and (symbolp function-name)
	   (eq (symbol-package function-name)
	       (find-package '#:cleavir-primop))
           (not (eq function-name 'cleavir-primop:call-with-variable-bound)))
      (make-instance 'cleavir-env:special-operator-info
	:name function-name)
      (call-next-method)))
