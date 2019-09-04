(cl:in-package #:sicl-hir-interpreter)

(defun interpret-hir (client enter-instruction lexical-environment)
  (let ((instruction (first (cleavir-ir:successors enter-instruction))))
    (setf *previous-instruction* instruction)
    (loop do (setf instruction
                   (interpret-instruction client instruction lexical-environment)))))

(defun hir-to-host-function (client enter-instruction)
  (lambda (arguments static-environment dynamic-environment)
    (let ((lexical-environment (make-hash-table :test #'eq))
          (static-environment-output
            (cleavir-ir:static-environment enter-instruction))
          (dynamic-environment-output
            (cleavir-ir:dynamic-environment-output enter-instruction)))
      (setf (gethash static-environment-output lexical-environment)
            static-environment)
      (setf (gethash 'static-environment lexical-environment)
            static-environment)
      (setf (gethash dynamic-environment-output lexical-environment)
            dynamic-environment)
      (setf (gethash 'arguments lexical-environment)
            (coerce arguments 'vector))
      (catch 'return
        (interpret-hir client enter-instruction lexical-environment)))))

(defun top-level-hir-to-host-function (client enter-instruction)
  (lambda (function-cell-finder static-environment)
    (let ((lexical-environment (make-hash-table :test #'eq))
          (static-environment-output
            (cleavir-ir:static-environment enter-instruction))
          (dynamic-environment-output
            (cleavir-ir:dynamic-environment-output enter-instruction)))
      (setf (gethash static-environment-output lexical-environment)
            static-environment)
      (setf (gethash 'static-environment lexical-environment)
            static-environment)
      (setf (gethash dynamic-environment-output lexical-environment)
            '())
      (setf (gethash 'arguments lexical-environment)
            (vector))
      (setf (gethash 'function-cell-finder lexical-environment)
            function-cell-finder)
      (catch 'return
        (interpret-hir client enter-instruction lexical-environment)))))

(defmethod interpret-instruction
    (client
     (instruction sicl-hir-transformations:find-function-cell-instruction)
     lexical-environment)
  (let* ((function-name (sicl-hir-transformations:name instruction))
         (output (first (cleavir-ir:outputs instruction))))
    (setf (gethash output lexical-environment)
          (funcall (gethash 'function-cell-finder lexical-environment)
                   function-name)))
  (first (cleavir-ir:successors instruction)))
