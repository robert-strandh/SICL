(cl:in-package #:cleavir-ast-transformations)

(defun find-shared-variables (ast)
  (let ((visited (make-hash-table :test #'eq))
        (result (make-hash-table :test #'eq)))
    (labels ((traverse-function (function ast)
               (if (typep ast 'cleavir-ast:lexical-ast)
                   (pushnew function (gethash ast result) :test #'eq)
                   (unless (gethash ast visited)
                     (setf (gethash ast visited) t)
                     (typecase ast
                       (cleavir-ast:function-ast
                        (loop for child in (cleavir-ast:children ast)
                              do (traverse-function ast child)))
                       (t
                        (loop for child in (cleavir-ast:children ast)
                              do (traverse-function function child))))))))
      (traverse-function nil ast))
    result))
