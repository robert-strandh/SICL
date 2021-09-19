(cl:in-package #:sicl-ast-to-hir)

;;;; LOAD-TIME-VALUE ASTs are handled as follows.  To tie a code
;;;; object to an environment, the top-level function is called with a
;;;; vector as a single required argument.  That vector will contain
;;;; constants, i.e., results of evaluating LOAD-TIME-VALUE forms and
;;;; also non-trivial constants created by the reader of the FASL
;;;; file.  So, we wrap the FORM-ATSs of the LOAD-TIME-VALUE ASTs in
;;;; an ASET-AST, where the ARRAY-AST is the parameter of the
;;;; top-level function, and the ELEMENT-AST is the FORM-AST of the
;;;; LOAD-TIME-VALUE AST.  The index starts with 0 and is incremented
;;;; for each new LOAD-TIME-VALUE AST found.  The LOAD-TIME-VALUE AST
;;;; itself is turned into a LOAD-CONSTANT-AST with the index as its
;;;; LOCATION-INFO.  We return a new AST which is the one we were
;;;; given, preceded by these ASET-ASTs and all if it wrapped in a
;;;; PROGN-AST.  We also return the number of LOAD-TIME-VALUE ASTs
;;;; found, so that we know at which index in the vector to start
;;;; storing constants created by the reader.

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
(defun hoist-load-time-value (ast)
  (let ((load-time-value-asts (find-load-time-value-asts ast))
        (form-asts (list ast)))
    (loop for count from 0
          for load-time-value-ast in (reverse load-time-value-asts)
          for form-ast = (cleavir-ast:form-ast load-time-value-ast)
          for code-vector-index-ast
            = (make-instance 'cleavir-ast:constant-ast :value 0)
          for literals-vector-index-ast
            = (make-instance 'cleavir-ast:constant-ast :value 0)
          for literal-cell = (list nil)
          for patch-literal-ast
            = (make-instance 'sicl-ast:patch-literal-ast
                :literal-cell literal-cell
                :literal-ast form-ast
                :code-vector-index-ast code-vector-index-ast
                :literals-vector-index-ast literals-vector-index-ast)
          do (change-class load-time-value-ast 'cleavir-ast:load-literal-ast
                           :location-info literal-cell)
             (push patch-literal-ast form-asts)
          finally (return (values (make-instance 'cleavir-ast:progn-ast
                                    :form-asts form-asts)
                                  count)))))
