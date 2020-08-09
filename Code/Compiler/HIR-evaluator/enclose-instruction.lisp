(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:enclose-instruction)
     lexical-environment)
  (let* ((static-environment-cell
           (value-cell 'static-environment lexical-environment))
         (enter-fn
           (hir-to-host-function client (cleavir-ir:code instruction)))
         (initializer
           (cleavir-ir:initializer instruction))
         (env-length
           ;; Don't count the closure object input.
           (length (rest (cleavir-ir:inputs initializer)))))
    (make-thunk (client instruction lexical-environment :inputs 0 :outputs 1)
      (let ((static-environment (car static-environment-cell)))
        (setf (output 0)
              (funcall (aref static-environment sicl-compiler:+enclose-function-index+)
                       enter-fn
                       (aref static-environment sicl-compiler:+code-object-index+)
                       env-length))
        (successor 0)))))
