(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:enclose-instruction)
     lexical-environment)
  (let* ((output (first (cleavir-ir:outputs instruction)))
         (enter (cleavir-ir:code instruction))
         (initializer (cleavir-ir:initializer instruction))
         ;; Don't count the closure object input.
         (env-length (length (rest (cleavir-ir:inputs initializer)))))
    (setf (lexical-value output lexical-environment)
          (funcall (aref (lexical-value 'static-environment lexical-environment)
                         sicl-compiler:+enclose-function-index+)
                   (hir-to-host-function client enter)
                   (aref (lexical-value 'static-environment lexical-environment)
                         sicl-compiler:+code-object-index+)
                   env-length)))
  (first (cleavir-ir:successors instruction)))
