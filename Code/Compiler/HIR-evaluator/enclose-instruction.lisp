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
              (funcall (aref (car static-environment-cell)
                             sicl-compiler:+enclose-function-index+)
                       enter-fn
                       (aref (car static-environment-cell)
                             sicl-compiler:+code-object-index+)
                       env-length))
        (successor 0)))))

(defun hir-to-host-function (client enter-instruction)
  (lambda (arguments static-environment dynamic-environment)
    (let ((lexical-environment (make-hash-table :test #'eq))
          (static-environment-output
            (cleavir-ir:static-environment enter-instruction))
          (dynamic-environment-output
            (cleavir-ir:dynamic-environment-output enter-instruction)))
      (setf (car (value-cell static-environment-output lexical-environment))
            static-environment)
      (setf (car (value-cell 'static-environment lexical-environment))
            static-environment)
      (setf (car (value-cell dynamic-environment-output lexical-environment))
            dynamic-environment)
      (setf (car (value-cell 'dynamic-environment lexical-environment))
            dynamic-environment)
      (setf (car (value-cell 'arguments lexical-environment))
            (coerce arguments 'vector))
      (catch 'return
        (evaluate-hir client enter-instruction lexical-environment)))))

(defun top-level-hir-to-host-function (client enter-instruction)
  (lambda (static-environment)
    (let ((lexical-environment (make-hash-table :test #'eq))
          (static-environment-output
            (cleavir-ir:static-environment enter-instruction))
          (dynamic-environment-output
            (cleavir-ir:dynamic-environment-output enter-instruction)))
      (setf (car (value-cell static-environment-output lexical-environment))
            static-environment)
      (setf (car (value-cell 'static-environment lexical-environment))
            static-environment)
      (setf (car (value-cell dynamic-environment-output lexical-environment))
            '())
      (setf (car (value-cell 'dynamic-environment lexical-environment))
            '())
      (catch 'return
        (evaluate-hir client enter-instruction lexical-environment)))))
