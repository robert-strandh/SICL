(cl:in-package #:sicl-compiler)

;;; Since we map in DEPTH-FIRST PRE-ORDER we accumulate the outermost
;;; ASTs first, but since we then PUSH them to a list, the list ends
;;; up having the innermost ASTs first.
(defun find-load-time-value-asts (ast)
  (let ((result '()))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (when (typep node 'cleavir-ast:load-time-value-ast)
	 (push node result)))
     ast)
    result))

;;; Since we want the innermost LOAD-TIME-VALUEs to be executed first,
;;; we must push them in the order of the outermost first.  For that
;;; reason, we REVERSE the list before processing it.
(defun hoist-load-time-value (ast array-variable-ast)
  (let ((load-time-value-asts (find-load-time-value-asts ast))
        (form-asts (list ast)))
    (loop for count from 0
          for load-time-value-ast in (reverse load-time-value-asts)
          for form-ast = (cleavir-ast:form-ast load-time-value-ast)
          for index-ast = (make-instance 'cleavir-ast:constant-ast
                            :value count)
          for aset-ast = (make-instance 'cleavir-ast:aset-ast
                           :array-ast array-variable-ast
                           :index-ast index-ast
                           :element-ast form-ast
                           :element-type t
                           :simple-p t
                           :boxed-p t)
	  do (change-class load-time-value-ast 'cleavir-ast:load-constant-ast
                           :location-info count)
             (push aset-ast form-asts)
          finally (return (values (make-instance 'cleavir-ast:progn-ast
                                    :form-asts form-asts)
                                  count)))))
                  
