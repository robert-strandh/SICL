(cl:in-package #:sicl-ast-to-hir)

;;; This variable holds an association list that maps
;;; VARIABLE-DEFINITION-ASTs of BLOCK-WITH-VARIABLE-ASTs to
;;; RECEIVE-INSTRUCTIONs to be used by UNWIND-INSTRUCTIONs resulting
;;; from the translation of RETURN-FROM-WITH-VARIABLE-AST
(defparameter *block-receive-instruction* '())

;;; This variable holds an association list that maps
;;; VARIABLE-DEFINITION-ASTs of BLOCK-WITH-VARIABLE-ASTs to
;;; VALUES-COUNT (0, 1, or :ALL) to be used as the VALUES-COUNT
;;; context variable when the FORM-AST of a
;;; RETURN-FROM-WITH-VARIABLE-AST is translated.
(defparameter *block-values-count* '())

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
           (*block-values-count*
             (acons variable-definition-ast *values-count*
                    *block-values-count*))
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
