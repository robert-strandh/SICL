(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:dynamic-catch-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment
                      :inputs 1 :outputs 1 :successors 1)
    (let ((abandon-tag (list nil)))
      (setf (output 0)
            (cons (make-instance 'sicl-run-time:catch-entry
                    :frame-pointer abandon-tag
                    :tag (input 0))
                  dynamic-environment))
      (catch abandon-tag
        (loop for thunk = (successor 0)
                then (setf thunk (funcall thunk (lexical-locations))))))))
