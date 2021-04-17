(cl:in-package #:cleavir-ast-transformations)

;;; Hoist LOAD-TIME-VALUE-ASTs.
;;; 
;;; An occurrence of a LOAD-TIME-VALUE-AST is replaced by a freshly
;;; created LEXICAL-AST.  We also create a fresh SETQ-AST that
;;; contains the contents of the FORM-AST slot of the original
;;; LOAD-TIME-VALUE-AST as the VALUE-AST and the freshly created
;;; LEXICAL-AST as the LHS-AST.
;;;
;;; Finally, a PROGN-AST is created that contains all the freshly
;;; created SETQ-ASTs as its first forms and the original AST as its
;;; last form.

(defun find-load-time-value-asts (ast)
  (let ((result '()))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (when (typep node 'cleavir-ast:load-time-value-ast)
         (push node result)))
     ast)
    result))

(defun hoist-load-time-value (ast)
  (let ((load-time-value-asts (find-load-time-value-asts ast))
        (form-asts (list ast)))
    (loop for load-time-value-ast in load-time-value-asts
          for form-ast = (cleavir-ast:form-ast load-time-value-ast)
          for setq-ast = (make-instance 'cleavir-ast:setq-ast
                           :lhs-ast load-time-value-ast
                           :value-ast form-ast)
          do (change-class load-time-value-ast 'cleavir-ast:lexical-ast
                           :name (gensym))
             (push setq-ast form-asts))
    (make-instance 'cleavir-ast:progn-ast :form-asts form-asts)))
