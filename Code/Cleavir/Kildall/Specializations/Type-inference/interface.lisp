(in-package #:cleavir-kildall-type-inference)

(defun infer-types (initial-instruction environment
                    &key prune draw liveness)
  (let* ((s (make-instance 'type-inference :env environment))
         (d (cleavir-kildall:kildall s initial-instruction
                                     :liveness liveness)))
    ;; since drawing is just for debugging, it's probably more
    ;; useful to do before the dangerous pruning step.
    (when draw
      (cleavir-kildall-graphviz:draw-flowchart-with-inputs
       initial-instruction draw s d))
    (when prune
      (prune-typeqs initial-instruction s d))
    initial-instruction))
