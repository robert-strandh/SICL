(cl:in-package #:cleavir-ast-transformations)

;;; We turn unused BLOCK-ASTs into PROGN-ASTs.
(defun remove-unused-block-asts (ast)
  (let ((used-block-asts '()))
    ;; For each RETURN-FROM-AST, register its corresponding BLOCK-AST
    ;; as being used.
    (map-asts (lambda (ast)
                (when (typep ast 'cleavir-ast:return-from-ast)
                  (pushnew (cleavir-ast:block-ast ast) used-block-asts
                           :test #'eq)))
              ast)
    ;; For each unused BLOCK-AST, turn it into a PROGN-AST.
    (map-asts (lambda (ast)
                (when (and (typep ast 'cleavir-ast:block-ast)
                           (not (member ast used-block-asts :test #'eq)))
                  (change-class ast 'cleavir-ast:progn-ast
                                :form-asts
                                (list (cleavir-ast:body-ast ast)))))
              ast))
  (values))
