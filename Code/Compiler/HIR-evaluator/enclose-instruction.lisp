(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:enclose-instruction)
     lexical-environment)
  (let* ((static-environment-lref
           (ensure-lref 'static-environment lexical-environment))
         (enter-fn
           #'dummy-enter-fn)
         (initializer
           (cleavir-ir:initializer instruction))
         (env-length
           ;; Don't count the closure object input.
           (if (null initializer)
               0
               (length (rest (cleavir-ir:inputs initializer))))))
    (prog1 (make-thunk (client instruction lexical-environment :inputs 0 :outputs 1)
             (let ((static-environment (lref static-environment-lref)))
               (setf (output 0)
                     (enclose enter-fn
                              (aref static-environment sicl-compiler:+code-object-index+)
                              env-length
                              (lexical-locations)))
               (successor 0)))
      (setf enter-fn
            (hir-to-host-function client (cleavir-ir:code instruction) lexical-environment)))))

(defun dummy-enter-fn ()
  (error "Invocation of the dummy enter fn."))
