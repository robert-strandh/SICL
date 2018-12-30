(cl:in-package #:sicl-minimal-extrinsic-environment)

;;; The default method on CLEAVIR-GENERATE-AST:CONVERT-SPECIAL-BINDING
;;; generates a call to CLEAVIR-PRIMOP:CALL-WITH-VARIABLE-BOUND, but
;;; that function does not have a default definition.  We define that
;;; function under a different name here, and then we import it into
;;; the environment with the name that is used by
;;; CONVERT-SPECIAL-BINDING
;;;
;;; Notice that this function must be a host function.  It accesses
;;; the host variable *DYNAMIC-ENVIRONMENT* which the caller SETS to
;;; its dynamic run-time environment immediately before the call.  In
;;; order to follow the same protocol, we must also SET this variable
;;; before calling THUNK.
(defun call-with-variable-bound (variable-name value thunk)
  (setf *dynamic-environment*
	(cons (make-instance 'variable-binding
		:symbol variable-name
		:value value)
	      *dynamic-environment*))
  (funcall thunk))

;;; For this environment, whenever Cleavir asks for information about
;;; a function name and that function name is a symbol in the
;;; CLEAVIR-PRIMOP package, we want to return an object of type
;;; CLEAVIR-ENV:SPECIAL-OPERATOR-INFO so that the form can be treated
;;; as a special form by GENERATE-AST.  The exception being
;;; CLEAVIER-PRIMOP:CALL-WITH-VARIABLE-BOUND which is a function.
(defmethod cleavir-env:function-info :around ((env environment) function-name)
  (if (and (symbolp function-name)
	   (eq (symbol-package function-name)
	       (find-package '#:cleavir-primop))
           (not (eq function-name 'cleavir-primop:call-with-variable-bound)))
      (make-instance 'cleavir-env:special-operator-info
	:name function-name)
      (call-next-method)))
