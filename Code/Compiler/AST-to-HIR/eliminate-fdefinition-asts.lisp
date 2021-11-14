(cl:in-package #:sicl-ast-to-hir)

(defun find-fdefinition-asts (ast)
  (let ((result '()))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (when (typep node 'cleavir-ast:fdefinition-ast)
         (push node result)))
     ast)
    result))

;;; when CST-to-AST detects a global function reference in a FUNCTION
;;; special form, it creates an FDEFINITION-AST containing a
;;; LITERAL-AST in the NAME-AST slot.  As source code, it would look
;;; like (FDEFINITION 'NAME).  We change that so that it, as source
;;; code, looks like (CAR (LOAD-TIME-VALUE (FUNCTION-CELL 'NAME))).
;;; In other words, the function cell is requested from the
;;; environment at load time and then becomes a literal value at run
;;; time.  So at run time, only a CAR-INSTRUCTION is executed in order
;;; to obtain the function object.
;;;
;;; Notice that the FDEFINITION-AST is not created as a result of a
;;; global function reference in a call position of a compound form.
;;; Then, instead a NAMED-CALL-AST is created.
(defun eliminate-fdefinition-asts (top-level-ast)
  (let ((fdefinition-asts (find-fdefinition-asts top-level-ast)))
    (loop for ast in fdefinition-asts
          for origin = (cleavir-cst-to-ast:origin ast)
          for name-ast = (cleavir-ast:name-ast ast)
          for literal-cell = (list nil)
          for call-ast = (cleavir-ast:make-ast 'cleavir-ast:named-call-ast
                           :origin origin
                           :callee-name 'sicl-data-and-control-flow:function-cell
                           :argument-asts (list name-ast))
          for load-literal-ast = (cleavir-ast:make-ast 'cleavir-ast:load-literal-ast
                                   :origin origin
                                   :location-info literal-cell)
          for patch-literal-ast = (cleavir-ast:make-ast 'sicl-ast:patch-literal-ast
                                    :literal-cell literal-cell
                                    :literal-ast call-ast
                                    :code-vector-index-ast
                                    (cleavir-ast:make-ast 'cleavir-ast:literal-ast
                                      :value 0)
                                    :literals-vector-index-ast
                                    (cleavir-ast:make-ast 'cleavir-ast:literal-ast
                                      :value 0))
          do (push patch-literal-ast
                   (cleavir-ast:form-asts top-level-ast))
             (change-class ast 'cleavir-ast:car-ast
                           :cons-ast load-literal-ast))))
