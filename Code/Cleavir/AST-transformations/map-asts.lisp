(cl:in-package #:cleavir-ast-transformations)

(defun map-asts (function ast)
  (let ((visited (make-hash-table :test #'eq))
        (worklist (list ast)))
    (loop until (null worklist)
          do (let ((next-ast (pop worklist)))
               (unless (gethash next-ast visited)
                 (setf (gethash next-ast visited) t)
                 (funcall function next-ast)
                 (setf worklist
                       (append (cleavir-ast:children next-ast) worklist)))))))
