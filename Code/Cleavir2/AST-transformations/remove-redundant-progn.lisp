(in-package #:sicl-file-compiler)

(defun simplify-ast (ast)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((aux (ast)
               (unless (gethash ast table)
                 (setf (gethash ast table) t)
                 (loop for child in (cleavir-ast:children ast)
                       do (aux child)
                          (when (typep ast 'cleavir-ast:progn-ast)
                            (reinitialize-instance
                             ast
                             :form-asts
                             (loop for child in (cleavir-ast:children ast)
                                   if (typep child 'cleavir-ast:progn-ast)
                                     append (cleavir-ast:children child)
                                   else
                                     collect child)))))))
      (aux ast))))
