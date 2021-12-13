(cl:in-package #:cleavir-ast-transformations)

(defun compute-parents (ast)
  (let ((result (make-hash-table :test #'eq)))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (loop for child in (cleavir-ast:children node)
             do (push node (gethash child result '()))))
     ast)
    result))

