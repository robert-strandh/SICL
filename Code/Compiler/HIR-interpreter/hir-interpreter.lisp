(cl:in-package #:sicl-hir-interpreter)

(defun interpret-hir (client enter-instruction lexical-environment)
  (let ((instruction (first (cleavir-ir:successors enter-instruction))))
    (loop do (setf instruction
                   (interpret-instruction client instruction lexical-environment)))))

(defun hir-to-host-function (client enter-instruction)
  (lambda (arguments static-environment dynamic-environment)
    (let ((lexical-environment (make-hash-table :test #'eq))
          (static-environment-output
            (cleavir-ir:static-environment enter-instruction))
          (dynamic-environment-output
            (cleavir-ir:dynamic-environment-output enter-instruction)))
      (setf (lexical-value static-environment-output lexical-environment)
            static-environment)
      (setf (lexical-value 'static-environment lexical-environment)
            static-environment)
      (setf (lexical-value dynamic-environment-output lexical-environment)
            dynamic-environment)
      (setf (lexical-value 'dynamic-environment lexical-environment) dynamic-environment)
      (setf (lexical-value 'arguments lexical-environment)
            (coerce arguments 'vector))
      (catch 'return
        (interpret-hir client enter-instruction lexical-environment)))))

(defun top-level-hir-to-host-function (client enter-instruction)
  (lambda (static-environment)
    (let ((lexical-environment (make-hash-table :test #'eq))
          (static-environment-output
            (cleavir-ir:static-environment enter-instruction))
          (dynamic-environment-output
            (cleavir-ir:dynamic-environment-output enter-instruction)))
      (setf (lexical-value static-environment-output lexical-environment)
            static-environment)
      (setf (lexical-value 'static-environment lexical-environment)
            static-environment)
      (setf (lexical-value dynamic-environment-output lexical-environment)
            '())
      (setf (lexical-value 'dynamic-environment lexical-environment) '())
      (catch 'return
        (interpret-hir client enter-instruction lexical-environment)))))
