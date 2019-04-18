(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;
;;; Recall that the SETQ-AST is a NO-VALUE-AST-MIXIN.  We must
;;; therefore make sure it is always compiled in a context where its
;;; value is not needed.  We do that by wrapping a PROGN around it.

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:constant-variable-info) env client)
  (declare (ignore var-cst form-cst env client))
  (error 'setq-constant-variable
	 :expr (cleavir-env:name info)))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:lexical-variable-info) env client)
  (process-progn 
   (list (make-instance 'cleavir-ast:setq-ast
	  :lhs-ast (cleavir-env:identity info)
	  :value-ast (convert client form-cst env))
	 (cleavir-env:identity info))))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:symbol-macro-info) env client)
  (let* ((expansion (funcall (coerce *macroexpand-hook* 'function)
                             (lambda (form env)
                               (declare (ignore form env))
                               (cleavir-env:expansion info))
                             (cleavir-env:name info)
                             env))
         (expansion-cst (cst:reconstruct expansion var-cst client)))
    (convert client
             (cst:list (cst:cst-from-expression 'setf)
                       expansion-cst
                       form-cst)
             env)))

(defmethod convert-setq-special-variable
    (var-cst form-ast info global-env client)
  (declare (ignore client))
  (let ((temp (make-instance 'cleavir-ast:lexical-ast :name (gensym))))
    (process-progn
     (list (make-instance 'cleavir-ast:setq-ast
             :lhs-ast temp
             :value-ast form-ast)
	   (make-instance 'cleavir-ast:set-symbol-value-ast
             :name (cleavir-env:name info)
             :value-ast temp)
	   temp))))

(defmethod convert-setq
    (var-cst form-cst (info cleavir-env:special-variable-info) env client)
  (let ((global-env (cleavir-env:global-environment env)))
    (convert-setq-special-variable var-cst
                                   (convert client form-cst env)
				   info
				   global-env
				   client)))

(defun convert-elementary-setq (client var-cst form-cst env)
  (let* ((symbol (cst:raw var-cst))
         (info (cleavir-env:variable-info env symbol)))
    (loop while (null info)
	  do (restart-case (error 'cleavir-env:no-variable-info
				  :name symbol
				  :origin (cst:source var-cst))
	       (recover ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'cleavir-env:special-variable-info
                         :name symbol)))
               ;; This is identical to RECOVER, but more specifically named.
	       (consider-special ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'cleavir-env:special-variable-info
                         :name symbol)))
	       (substitute (new-symbol)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq info (cleavir-env:variable-info env new-symbol)))))
    (convert-setq var-cst form-cst info env client)))
