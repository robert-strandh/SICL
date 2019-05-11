(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;
;;; Recall that the SETQ-AST is a NO-VALUE-AST-MIXIN.  We must
;;; therefore make sure it is always compiled in a context where its
;;; value is not needed.  We do that by wrapping a PROGN around it.

(defmethod convert-setq
    (client var-cst form-cst (info cleavir-env:constant-variable-info) lexical-environment dynamic-environment-ast)
  (declare (ignore var-cst form-cst lexical-environment client))
  (error 'setq-constant-variable
	 :expr (cleavir-env:name info)))

(defmethod convert-setq
    (client var-cst form-cst (info cleavir-env:lexical-variable-info) lexical-environment dynamic-environment-ast)
  (process-progn 
   (list (make-instance 'cleavir-ast:setq-ast
	  :lhs-ast (cleavir-env:identity info)
	  :value-ast (convert client form-cst lexical-environment dynamic-environment-ast)
          :dynamic-environment-input-ast dynamic-environment-ast)
	 (cleavir-env:identity info))
   dynamic-environment-ast))

(defmethod convert-setq
    (client var-cst form-cst (info cleavir-env:symbol-macro-info) lexical-environment dynamic-environment-ast)
  (let* ((expansion (funcall (coerce *macroexpand-hook* 'function)
                             (lambda (form lexical-environment)
                               (declare (ignore form lexical-environment))
                               (cleavir-env:expansion info))
                             (cleavir-env:name info)
                             lexical-environment))
         (expansion-cst (cst:reconstruct expansion var-cst client)))
    (convert client
             (cst:list (cst:cst-from-expression 'setf)
                       expansion-cst
                       form-cst)
             lexical-environment
             dynamic-environment-ast)))

(defmethod convert-setq-special-variable
    (client var-cst form-ast info global-env dynamic-environment-ast)
  (declare (ignore client))
  (let ((temp (make-instance 'cleavir-ast:lexical-ast :name (gensym))))
    (process-progn
     (list (make-instance 'cleavir-ast:setq-ast
             :lhs-ast temp
             :value-ast form-ast
             :dynamic-environment-input-ast dynamic-environment-ast)
	   (make-instance 'cleavir-ast:set-symbol-value-ast
             :name-ast (make-instance 'cleavir-ast:constant-ast
                         :dynamic-environment-input-ast dynamic-environment-ast
                         :value (cleavir-env:name info))
             :value-ast temp
             :dynamic-environment-input-ast dynamic-environment-ast)
	   temp)
     dynamic-environment-ast)))

(defmethod convert-setq
    (client var-cst form-cst (info cleavir-env:special-variable-info) lexical-environment dynamic-environment-ast)
  (let ((global-env (cleavir-env:global-environment lexical-environment)))
    (convert-setq-special-variable client
                                   var-cst
                                   (convert client form-cst lexical-environment dynamic-environment-ast)
				   info
				   global-env
                                   dynamic-environment-ast)))

(defun convert-elementary-setq (client var-cst form-cst lexical-environment dynamic-environment-ast)
  (let* ((symbol (cst:raw var-cst))
         (info (cleavir-env:variable-info lexical-environment symbol)))
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
		 (setq info (cleavir-env:variable-info lexical-environment new-symbol)))))
    (convert-setq client var-cst form-cst info lexical-environment dynamic-environment-ast)))
