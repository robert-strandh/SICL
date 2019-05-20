(cl:in-package #:sicl-hir-to-cl)

(defmethod translate (client (instruction cleavir-ir:multiple-to-fixed-instruction) context)
  (let* ((values-location (first (cleavir-ir:inputs instruction)))
         (name (gethash values-location (values-locations context)))
         (outputs (cleavir-ir:outputs instruction)))
    `(,@(loop for output in outputs
              collect `(setq ,(cleavir-ir:name output)
                             (pop ,name))))))

(defmethod translate (client (instruction cleavir-ir:fixed-to-multiple-instruction) context)
  (let* ((values-location (first (cleavir-ir:outputs instruction)))
         (name (gethash values-location (values-locations context)))
         (inputs (cleavir-ir:inputs instruction)))
    `((setq ,name (list ,@(mapcar #'cleavir-ir:name inputs))))))
