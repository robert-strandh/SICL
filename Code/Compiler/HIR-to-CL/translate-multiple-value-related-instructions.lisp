(cl:in-package #:sicl-hir-to-cl)

(defmethod translate (client (instruction cleavir-ir:multiple-to-fixed-instruction) context)
  (let ((outputs (cleavir-ir:outputs instruction)))
    `(,@(loop for output in outputs
              collect `(setq ,(cleavir-ir:name output)
                             (pop ,(values-location context)))))))

(defmethod translate (client (instruction cleavir-ir:fixed-to-multiple-instruction) context)
  (let ((inputs (cleavir-ir:inputs instruction)))
    `((setq ,(values-location context)
            (list ,@(mapcar #'cleavir-ir:name inputs))))))
