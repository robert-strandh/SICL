(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:return-from-with-variable-ast))
  (let* ((variable-reference-ast (ico:variable-reference-ast ast))
         (definition-ast (ico:definition-ast variable-reference-ast))
         (identity-register (find-register definition-ast))
         (form-ast (ico:form-ast ast))
         (value-register
           (make-instance
               (if (eql *values-count* :all)
                   'hir:multiple-value-register
                   'hir:single-value-register)))
         (*next-instruction*
           (make-instance 'hir:unwind-instruction
             :inputs (list *dynamic-environment-register*
                           identity-register
                           value-register)
             :outputs '()
             :successors
             (list (assoc definition-ast *block-next-instructions*)))))
    (translate-ast client form-ast)))
                                      
    
