(cl:in-package #:sicl-file-compiler)

;;; When we are asked to compile the name of a global function, by
;;; default Cleavir generates an FDEFINITION-AST taking the function
;;; name as an input.  For SICL, we do not want that.  Instead we want
;;; it to generate an access to the CAR of the global function cell
;;; that contains the function.  And we want the function cell to be
;;; accessed at load time.
(defmethod cleavir-generate-ast:convert-global-function
    ((info cleavir-env:global-function-info)
     (env sicl-genv:environment)
     system)
  (declare (ignore system))
  (cleavir-ast:make-car-ast
   (cleavir-ast:make-load-time-value-ast
    `(sicl-genv:function-cell
      ',(cleavir-env:name info)
      (sicl-genv:global-environment))
    ;; The cell is not read-only.
    nil)))

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
