(cl:in-package #:cleavir-cst-to-ast-test)

(defun assign-sources (cst)
  (labels ((aux (cst path)
             (reinitialize-instance cst :source path)
             (when (cst:consp cst)
               (loop for i from 0
                     for rest = cst then (cst:rest rest)
                     until (cst:null rest)
                     do (aux (cst:first rest) (append path (list i)))))))
    (aux cst '(0))))
