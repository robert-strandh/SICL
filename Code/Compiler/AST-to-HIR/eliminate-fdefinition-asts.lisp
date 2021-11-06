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
(defun eliminate-fdefinition-asts (ast)
  (let ((fdefinition-asts (find-fdefinition-asts ast)))
    (loop for ast in fdefinition-asts
          for origin = (cleavir-cst-to-ast:origin ast)
          for name-ast = (cleavir-ast:name-ast ast)
          for call-ast = (make-instance 'cleavir-ast:named-call-ast
                           :origin origin
                           :callee-name 'sicl-data-and-control-flow:function-cell
                           :argument-asts (list name-ast))
          for load-time-value-ast = (make-instance 'cleavir-ast:load-time-value-ast
                                      :origin origin
                                      :form-ast call-ast)
          do (change-class ast 'cleavir-ast:car-ast
                           :cons-ast load-time-value-ast))))
