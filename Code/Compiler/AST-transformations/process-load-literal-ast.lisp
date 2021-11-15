(cl:in-package #:sicl-ast-transformations)

;;; When the AST is created by the compiler using file-compilation
;;; semantics, there may be code at the top level to create literals
;;; to be used somewhere else, typically deeper down in the code.
;;; Such creation code has a LEXICAL-BIND-AST that creates a
;;; LEXICAL-AST from an AST representing the creation code.  This
;;; LEXICAL-AST is shared by a LOAD-LITERAL-AST at the place where the
;;; literal is used, in the LOCATION-INFO slot of a LOAD-LITERAL-AST.
;;; But for SICL, we do not want shared variables across function
;;; boundaries for literals.  So we introduce an instance of a
;;; PATCH-LITERAL-AST to be evaluated after the LEXICAL-AST has been
;;; created at the top level.  And we replace the LEXICAL-AST in
;;; the LOAD-LITERAL-AST by a CONS cell that is shared with the
;;; introduced PATCH-LITERAL-AST.
(defun process-load-literal-ast (ast)
  (let ((load-literal-ast-table (make-hash-table :test #'eq)))
    ;; First, we find every instance of a LOAD-LITERAL-AST and we
    ;; enter it into the hash table, using the LEXICAL-AST as a
    ;; key.
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (ast-node)
       (when (typep ast-node 'cleavir-ast:load-literal-ast)
         (let ((lexical-ast (cleavir-ast:location-info ast-node)))
           (assert (typep lexical-ast 'cleavir-ast:lexical-ast))
           (setf (gethash lexical-ast load-literal-ast-table)
                 ast-node))))
     ast)
    ;; Next, traverse the AST again, and look for LEXICAL-BIND-ASTs
    ;; that create the LEXICAL-ASTs collected.
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (ast-node)
       (when (typep ast-node 'cleavir-ast:lexical-bind-ast)
         (let* ((lexical-ast (cleavir-ast:lexical-variable-ast ast-node))
                (load-literal-ast (gethash lexical-ast load-literal-ast-table)))
           (unless (null load-literal-ast)
             (let ((cell (list nil)))
               (setf (cleavir-ast:location-info load-literal-ast) cell)
               (change-class ast-node 'cleavir-ast:progn-ast
                             :form-asts
                             (list (cleavir-ast-transformations:clone-ast ast-node)
                                   (cleavir-ast:make-ast 'sicl-ast:patch-literal-ast
                                     :code-vector-index-ast 0
                                     :literals-vector-index-ast 0
                                     :literal-cell cell))))))))
     ast)))
