(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:catch-instruction)
     lexical-environment)
  (let ((output (first (cleavir-ir:outputs instruction)))
        (successors (cleavir-ir:successors instruction))
        (tag (list nil)))
    (setf (gethash output lexical-environment) tag)
    (nth (catch tag
           (interpret-instructions client (first successors) lexical-environment)
           (1- (length successors)))
         successors)))
