(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn a list of ASTs into either a CONSTANT-AST if the list of ASTs
;;; is empty, into the first AST in the list, if the list contains a
;;; singleton element, or into a PROGN-AST with the list of ASTs as it
;;; FORM-AST slot otherwise.

(defun process-progn (asts)
  (cond ((null asts)
         (cleavir-ast:make-ast 'cleavir-ast:constant-ast :value nil))
        ((null (rest asts))
         (first asts))
        (t
         ;; Do some PROGN compression.  Remove a redundant AST only
         ;; if it does not have any ORIGIN information.
         (cleavir-ast:make-ast 'cleavir-ast:progn-ast
           :form-asts (loop for ast in asts
                            if (and (typep ast 'cleavir-ast:progn-ast)
                                    (null (origin ast)))
                              append (cleavir-ast:form-asts ast)
                            else
                              collect ast)))))
