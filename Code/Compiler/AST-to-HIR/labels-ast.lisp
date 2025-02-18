(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:labels-ast))
  (let* ((local-function-asts (ico:binding-asts ast))
         (registers
           (loop for local-function-ast in local-function-asts
                 for variable-definition-ast
                   = (ico:name-ast local-function-ast)
                 for register = (make-instance 'hir:single-value-register)
                 do (setf (find-register variable-definition-ast) register)
                 collect register)))
    (let ((result
            (translate-implicit-progn client (ico:form-asts ast))))
      (loop for local-function-ast in (reverse local-function-asts)
            for register in (reverse registers)
            do (setf result
                     (make-instance 'hir:enclose-instruction
                       :parse-arguments-instruction
                       (translate-ast client local-function-ast)
                       :outputs (list register)
                       :successors (list result))))
      result)))
