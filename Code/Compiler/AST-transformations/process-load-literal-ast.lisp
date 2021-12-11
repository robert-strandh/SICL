(cl:in-package #:sicl-ast-transformations)

;;; When the AST is created by the compiler using file-compilation
;;; semantics, there may be code at the top level (i.e., code executed
;;; at load time) to create literals to be used somewhere else,
;;; typically deeper down in the code.  This code is created by the
;;; compiler for creating similar literals at load time, or by
;;; LOAD-TIME-VALUE.  Such creation code has a LEXICAL-BIND-AST that
;;; creates a LEXICAL-AST from an AST representing the creation code.
;;; this LEXICAL-AST is then used elsewhere, including in code inside
;;; nested functions that are typically not executed at load time.
;;; But for SICL, we do not want shared variables across function
;;; boundaries for literals.  So we introduce an instance of a
;;; PATCH-LITERAL-AST to be evaluated after the LEXICAL-AST has been
;;; created at the top level.  And we replace the LEXICAL-AST in
;;; nested code with a LOAD-LITERAL-AST.
;;;
;;; The technique we use is to identify instances of LEXICAL-AST
;;; created at the top level.  The code that creates those
;;; LEXICAL-ASTs is executed exactly once at load time, so the value
;;; of the LEXICAL-AST never changes after that.  We take advantage of
;;; that fact to replace the use of the LEXICAL-AST in nested
;;; functions by a LOAD-LITERAL-AST, and we insert a PATCH-LITERAL-AST
;;; after the creation code.  These two new ASTs communicate using a
;;; shared CONS cell.
;;;
;;; By doing it this way, we do not take into account the exact way
;;; the code for creating and using the LEXICAL-AST was generated.  No
;;; matter how it was created, we take advantage of this unique
;;; create/use pattern.

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
    (let ((lexical-bind-asts '()))
      (cleavir-ast:map-ast-depth-first-preorder
       (lambda (ast-node)
         (when (typep ast-node 'cleavir-ast:lexical-bind-ast)
           (let* ((lexical-ast (cleavir-ast:lexical-variable-ast ast-node))
                  (load-literal-ast (gethash lexical-ast load-literal-ast-table)))
             (unless (null load-literal-ast)
               (push ast-node lexical-bind-asts)))))
       ast)
      ;; Finally process the LEXICAL-BIND-ASTs found.
      (loop for ast-node in lexical-bind-asts
            do (let* ((cell (list nil))
                      (lexical-ast (cleavir-ast:lexical-variable-ast ast-node))
                      (load-literal-ast (gethash lexical-ast load-literal-ast-table)))
                 (setf (cleavir-ast:location-info load-literal-ast) cell)
                 (change-class ast-node 'cleavir-ast:progn-ast
                               :form-asts
                               (list (cleavir-ast:make-ast 'cleavir-ast:lexical-bind-ast
                                       :origin (cleavir-cst-to-ast:origin ast-node)
                                       :lexical-variable-ast lexical-ast
                                       :value-ast (cleavir-ast:value-ast ast-node))
                                     (cleavir-ast:make-ast 'sicl-ast:patch-literal-ast
                                       :origin (cleavir-cst-to-ast:origin ast-node)
                                       :literal-ast lexical-ast
                                       :code-vector-index-ast 0
                                       :literals-vector-index-ast 0
                                       :literal-cell cell))))))))
