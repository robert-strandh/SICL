(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:catch-instruction)
     lexical-environment)
  (destructuring-bind (continuation-output dynamic-environment-output)
      (cleavir-ir:outputs instruction)
    (let* ((successors (cleavir-ir:successors instruction))
           (transfer-tag (list nil))
           (abandon-tag (list nil)))
      (setf (lexical-value continuation-output lexical-environment)
            transfer-tag)
      (setf (lexical-value dynamic-environment-output lexical-environment)
            (cons (make-instance 'sicl-run-time:block/tagbody-entry
                    :continuation transfer-tag
                    :stack-pointer nil
                    :frame-pointer abandon-tag)
                  (lexical-value (cleavir-ir:dynamic-environment-location instruction)
                                 lexical-environment)))
      (catch abandon-tag
        (loop for successor = (first successors)
                then  (catch transfer-tag
                        (interpret-instructions client successor lexical-environment)))))))
