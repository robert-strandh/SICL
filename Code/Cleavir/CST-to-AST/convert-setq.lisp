(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;
;;; Recall that the SETQ-AST is a NO-VALUE-AST-MIXIN.  We must
;;; therefore make sure it is always compiled in a context where its
;;; value is not needed.  We do that by wrapping a PROGN around it.

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:constant-variable-info) env system)
  (declare (ignore var-cst form-cst env system))
  (error 'setq-constant-variable
	 :expr (cleavir-env:name info)))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:lexical-variable-info) env system)
  (process-progn 
   (list (cleavir-ast:make-setq-ast
	  (cleavir-env:identity info)
	  (convert form-cst env system)
	  :origin (cst:source var-cst))
	 (cleavir-env:identity info))))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:symbol-macro-info) env system)
  (let* ((expansion (funcall (coerce *macroexpand-hook* 'function)
                             (lambda (form env)
                               (declare (ignore form env))
                               (cleavir-env:expansion info))
                             (cleavir-env:name info)
                             env))
         (expansion-cst (cst:reconstruct expansion var-cst)))
    (convert (cst:list (cst:cst-from-expression 'setf)
                       expansion-cst
                       form-cst)
             env system)))
