(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:enclose-instruction)
     lexical-environment)
  (let* ((enter-fn
           #'dummy-enter-fn)
         (initializer
           (cleavir-ir:initializer instruction))
         (env-length
           ;; Don't count the closure object input.
           (if (null initializer)
               0
               (length (rest (cleavir-ir:inputs initializer))))))
    (prog1 (make-thunk (client instruction lexical-environment :inputs 0 :outputs 1)
             (setf (output 0)
                   (enclose enter-fn
                            env-length))
             (successor 0))
      (setf enter-fn
            (hir-to-host-function client (cleavir-ir:code instruction))))))

(defun dummy-enter-fn ()
  (error "Invocation of the dummy enter fn."))
