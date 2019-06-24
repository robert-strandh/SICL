(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;
;;; Recall that the SETQ-AST is a NO-VALUE-AST-MIXIN.  We must
;;; therefore make sure it is always compiled in a context where its
;;; value is not needed.  We do that by wrapping a PROGN around it.

(defmethod convert-setq
    (client var-cst form-cst (info trucler:constant-variable-description) environment)
  (declare (ignore var-cst environment client))
  (error 'setq-constant-variable :cst form-cst))

(defmethod convert-setq
    (client var-cst form-cst (info trucler:lexical-variable-description) environment)
  (process-progn 
   (list (cleavir-ast:make-ast 'cleavir-ast:setq-ast
          :lhs-ast (trucler:identity info)
          :value-ast (convert client form-cst environment))
         (trucler:identity info))))

(defmethod convert-setq
    (client var-cst form-cst (info trucler:symbol-macro-description) environment)
  (let* ((expansion (trucler:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-variable (expand-macro expander var-cst environment))
         (expanded-cst (cst:reconstruct expanded-variable var-cst client))
         (origin (cst:source var-cst)))
    (setf (cst:source expanded-cst) origin)
    (convert client
             (cst:cons (make-atom-cst 'setf origin)
                       (cst:list expanded-cst form-cst)
                       :source origin)
             environment)))

(defmethod convert-setq-special-variable
    (client var-cst form-ast info global-env)
  (declare (ignore client))
  (let ((temp (cleavir-ast:make-ast 'cleavir-ast:lexical-ast
                :name (gensym))))
    (process-progn
     (list (cleavir-ast:make-ast 'cleavir-ast:setq-ast
             :lhs-ast temp
             :value-ast form-ast)
           (cleavir-ast:make-ast 'cleavir-ast:set-symbol-value-ast
             :name-ast (cleavir-ast:make-ast 'cleavir-ast:constant-ast
                         :value (trucler:name info))
             :value-ast temp)
           temp))))

(defmethod convert-setq
    (client var-cst form-cst (info trucler:special-variable-description) environment)
  (let ((global-env (trucler:global-environment client environment)))
    (convert-setq-special-variable client
                                   var-cst
                                   (convert client form-cst environment)
                                   info
                                   global-env)))

(defun convert-elementary-setq (client var-cst form-cst environment)
  (let* ((symbol (cst:raw var-cst))
         (info (trucler:describe-variable client environment symbol)))
    (loop while (null info)
          do (restart-case (error 'trucler:no-variable-description
                                  :name symbol
                                  :origin (cst:source var-cst))
               (continue ()
                 :report (lambda (stream)
                           (format stream "Consider the variable as special."))
                 (setf info
                       (make-instance 'trucler:special-variable-description
                         :name symbol)))
               ;; This is identical to CONTINUE, but more specifically named.
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
                 (setq info (trucler:describe-variable client environment new-symbol)))))
    (convert-setq client var-cst form-cst info environment)))
