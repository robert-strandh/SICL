(cl:in-package #:cleavir-ast-transformations)

(defun no-child-found (old parent)
  (error "No child ~s found in ~s" old parent))

;;; Substitute NEW for OLD as a child of PARENT.
(defgeneric substitute-ast (new old parent))

;;; The default method works for every AST class that stores each
;;; individual child in its own slot.  It does not work for AST
;;; classes with a variable number of children stored in a list.
(defmethod substitute-ast (new old (parent cleavir-ast:ast))
  (loop for (initarg slot-reader) in (cleavir-io:save-info parent)
        when (eq old (funcall slot-reader parent))
          do (reinitialize-instance parent initarg new)
             (loop-finish)
        finally (no-child-found old parent)))

;;; For the PROGN-AST we need to scan the list of FORM-ASTS.
(defmethod substitute-ast (new old (parent cleavir-ast:progn-ast))
  (if (member old (cleavir-ast:form-asts parent))
      (nsubstitute new old (cleavir-ast:form-asts parent))
      (no-child-found old parent)))

(defmethod substitute-ast (new old (parent cleavir-ast:call-ast))
  (cond ((eq old (cleavir-ast:callee-ast parent))
         (reinitialize-instance parent :callee-ast new))
        ((member old (cleavir-ast:argument-asts parent))
         (nsubstitute new old (cleavir-ast:argument-asts parent)))
        (t
         (no-child-found old parent))))

(defmethod substitute-ast (new old (parent cleavir-ast:named-call-ast))
  (if (member old (cleavir-ast:argument-asts parent))
      (nsubstitute new old (cleavir-ast:argument-asts parent))
      (no-child-found old parent)))

(defmethod substitute-ast (new old (parent cleavir-ast:multiple-value-setq-ast))
  (cond ((eq old (cleavir-ast:form-ast parent))
         (reinitialize-instance parent :form-ast new))
        ((member old (cleavir-ast:lhs-asts parent))
         (nsubstitute new old (cleavir-ast:lhs-asts parent)))
        (t
         (no-child-found old parent))))

(defmethod substitute-ast (new old (parent cleavir-ast:tagbody-ast))
  (if (member old (cleavir-ast:item-asts parent))
      (nsubstitute new old (cleavir-ast:item-asts parent))
      (no-child-found old parent)))

(defmethod substitute-ast (new old (parent cleavir-ast:multiple-value-call-ast))
  (cond ((eq old (cleavir-ast:function-form-ast parent))
         (reinitialize-instance parent :function-form-ast new))
        ((member old (cleavir-ast:form-asts parent))
         (nsubstitute new old (cleavir-ast:form-asts parent)))
        (t
         (no-child-found old parent))))

(defmethod substitute-ast (new old (parent cleavir-ast:values-ast))
  (if (member old (cleavir-ast:argument-asts parent))
      (nsubstitute new old (cleavir-ast:argument-asts parent))
      (no-child-found old parent)))

(defmethod substitute-ast (new old (parent cleavir-ast:multiple-value-prog1-ast))
  (cond ((eq old (cleavir-ast:form-ast parent))
         (reinitialize-instance parent :form-ast new))
        ((member old (cleavir-ast:form-asts parent))
         (nsubstitute new old (cleavir-ast:form-asts parent)))
        (t
         (no-child-found old parent))))
