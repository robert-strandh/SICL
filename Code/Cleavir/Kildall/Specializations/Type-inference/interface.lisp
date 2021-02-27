(in-package #:cleavir-kildall-type-inference)

(defun infer-types (initial-instruction environment
                    &key prune liveness)
  (let* ((s (make-instance 'type-inference :env environment))
         (d (cleavir-kildall:kildall s initial-instruction
                                     :liveness liveness)))
    (when prune
      (prune-typeqs initial-instruction s d))
    initial-instruction))
