(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn a list of ASTs into either a PROGN-AST or a CONSTANT-AST
;;; containing NIL in case the list of ASTs is NIL.

(defun process-progn (asts)
  (cond ((null asts)
         (cleavir-ast:make-ast 'cleavir-ast:constant-ast :value nil))
        ((null (rest asts))
         (first asts))
        (t ;; Do some PROGN compression.
         (cleavir-ast:make-ast 'cleavir-ast:progn-ast
           :form-asts (loop for ast in asts
                            if (typep ast 'cleavir-ast:progn-ast)
                              append (cleavir-ast:form-asts ast)
                            else
                              collect ast)))))
