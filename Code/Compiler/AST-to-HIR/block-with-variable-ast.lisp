(cl:in-package #:sicl-ast-to-hir)

(defparameter *block-receive-instruction* '())

(defmethod translate-ast (client (ast ico:block-with-variable-ast))
  (let ((variable-definition-ast (ico:variable-definition-ast ast))
        (identity-register (make-instance 'hir:single-value-register)))
    (setf (find-register variable-definition-ast) identity-register)
    (let* ((receive-instruction
             (make-instance 'hir:receive-instruction
               :successors (list *next-instruction*)))
           (*block-receive-instruction*
             (acons variable-definition-ast receive-instruction
                    *block-receive-instruction*))
           (current-dynamic-environment-register
             *dynamic-environment-register*)
           (*dynamic-environment-register*
             (make-instance 'hir:single-value-register))
           (body-instruction
             (translate-implicit-progn client (ico:form-asts ast))))
      (make-instance 'hir:exit-point-instruction
        :inputs (list current-dynamic-environment-register)
        :outputs (list *dynamic-environment-register*
                       identity-register
                       *target-register*)
        :successors (list body-instruction)))))
