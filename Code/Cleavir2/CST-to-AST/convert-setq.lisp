(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;
;;; Recall that the SETQ-AST is a NO-VALUE-AST-MIXIN.  We must
;;; therefore make sure it is always compiled in a context where its
;;; value is not needed.  We do that by wrapping a PROGN around it.

(defmethod convert-setq
    (client var-cst form-cst (info trucler:constant-variable-description) lexical-environment)
  (declare (ignore var-cst form-cst lexical-environment client))
  (error 'setq-constant-variable :cst form-cst))

(defmethod convert-setq
    (client var-cst form-cst (info trucler:lexical-variable-description) lexical-environment)
  (process-progn 
   (list (make-instance 'cleavir-ast:setq-ast
	  :lhs-ast (trucler:identity info)
	  :value-ast (convert client form-cst lexical-environment))
	 (trucler:identity info))))

(defmethod convert-setq
    (client var-cst form-cst (info trucler:symbol-macro-description) lexical-environment)
  (let* ((expansion (funcall (coerce *macroexpand-hook* 'function)
                             (lambda (form lexical-environment)
                               (declare (ignore form lexical-environment))
                               (trucler:expansion info))
                             (trucler:name info)
                             lexical-environment))
         (expansion-cst (cst:reconstruct expansion var-cst client)))
    (convert client
             (cst:list (cst:cst-from-expression 'setf)
                       expansion-cst
                       form-cst)
             lexical-environment)))

(defmethod convert-setq-special-variable
    (client var-cst form-ast info global-env)
  (declare (ignore client))
  (let ((temp (make-instance 'cleavir-ast:lexical-ast
                :name (gensym))))
    (process-progn
     (list (make-instance 'cleavir-ast:setq-ast
             :lhs-ast temp
             :value-ast form-ast)
	   (make-instance 'cleavir-ast:set-symbol-value-ast
             :name-ast (make-instance 'cleavir-ast:constant-ast
                         :value (trucler:name info))
             :value-ast temp)
	   temp))))

(defmethod convert-setq
    (client var-cst form-cst (info trucler:special-variable-description) lexical-environment)
  (let ((global-env (trucler:global-environment client lexical-environment)))
    (convert-setq-special-variable client
                                   var-cst
                                   (convert client form-cst lexical-environment)
				   info
				   global-env)))

(defun convert-elementary-setq (client var-cst form-cst lexical-environment)
  (let* ((symbol (cst:raw var-cst))
         (info (trucler:describe-variable client lexical-environment symbol)))
    (loop while (null info)
	  do (restart-case (error 'trucler:no-variable-description
				  :name symbol
				  :origin (cst:source var-cst))
	       (recover ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'trucler:special-variable-description
                         :name symbol)))
               ;; This is identical to RECOVER, but more specifically named.
	       (consider-special ()
		 :report (lambda (stream)
			   (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'trucler:special-variable-description
                         :name symbol)))
	       (substitute (new-symbol)
		 :report (lambda (stream)
			   (format stream "Substitute a different name."))
		 :interactive (lambda ()
				(format *query-io* "Enter new name: ")
				(list (read *query-io*)))
		 (setq info (trucler:describe-variable client lexical-environment new-symbol)))))
    (convert-setq client var-cst form-cst info lexical-environment)))
