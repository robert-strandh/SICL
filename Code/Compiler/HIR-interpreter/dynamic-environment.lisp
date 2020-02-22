(cl:in-package #:sicl-hir-interpreter)

(defvar *global-values-location*)

(defmethod interpret-instruction :before
    (client instruction lexical-environment)
  (let ((env1 (lexical-value 'dynamic-environment lexical-environment))
        (env2 (lexical-value (cleavir-ir:dynamic-environment-location
                              instruction)
                             lexical-environment)))
    (unless (or (eq env1 env2)
                (> (length env2) (length env1)))
      (loop for env = env1 then (rest env)
            for entry = (first env)
            until (eq env env2)
            do (invalidate-entry entry))
      (let ((last-block/tagbody
              (loop with result = nil
                    for env = env1 then (rest env)
                    for entry = (first env)
                    until (eq env env2)
                    when (typep entry 'unwind-protect-entry)
                      do (funcall (thunk entry))
                    when (typep entry 'block/tagbody-entry)
                      do (setf result entry)
                    finally (return result))))
        (unless (null last-block/tagbody)
          (throw (frame-pointer last-block/tagbody)
            instruction))))))

(defmethod interpret-instruction :after
    (client instruction lexical-environment)
  (declare (ignore client))
  (setf (lexical-value 'dynamic-environment lexical-environment)
        (lexical-value (cleavir-ir:dynamic-environment-location
                        instruction)
                       lexical-environment)))
